(ns gd-edit.io.stash
  (:require [gd-edit.structure :as s]
            [gd-edit.utils :as u]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.game-dirs :as dirs])
  (:import  [java.nio ByteBuffer ByteOrder]))

(def ^:dynamic *debug* false)

(defn read-int-no-update
  [bb context]
  (gdc/decrypt-int (.getInt bb) (:enc-state @context)))

(defn write-int-no-update
  [bb data context]
  (.putInt bb (gdc/encrypt-int data (:enc-state @context))))

(defn struct-block
  [block-specs]
  (with-meta block-specs
    {:struct/type :block}))

(defmethod s/read-spec :block
  [spec bb _ context]
  (gdc/read-block bb context spec))

(defmethod s/write-spec :block
  [spec bb data context]
  (gdc/write-block bb data context spec))

(defn- read-transfer-item!
  [^ByteBuffer bb context]
  (merge (s/read-struct gdc/Item-core bb context)
         (gdc/read-item-tail-v13! bb context)
         ;; GD 1.3+ (container version >= 11) stores X/Y as encrypted ints
         ;; (same as personal stash items); older stash formats use raw floats.
         (if (>= (or (:stash-container-version @context) 0) 11)
           {:X (gdc/read-int!  bb context)
            :Y (gdc/read-int!  bb context)}
           {:X (.getFloat bb)
            :Y (.getFloat bb)})))

(defn- write-transfer-item!
  [^ByteBuffer bb data context]
  (s/write-struct gdc/Item-core bb data context)
  (gdc/write-item-tail-v13! bb data context)
  (if (>= (or (:stash-container-version @context) 0) 11)
    (do (gdc/write-int! bb (:X data) context)
        (gdc/write-int! bb (:Y data) context))
    (do (.putFloat bb (float (:X data)))
        (.putFloat bb (float (:Y data))))))

(def TransferStashItem
  (with-meta
    (into gdc/Item-core
          (s/struct-def
           :X :int32
           :Y :int32))
    {:struct/read  read-transfer-item!
     :struct/write write-transfer-item!}))

(def InventorySack
  (s/struct-def
   :width           :int32
   :height          :int32
   :inventory-items (s/array TransferStashItem)

   ;; GD 1.3 / FoA: per-tab border/symbol/label (same shape as character
   ;; stash Block4/Stash). Present at block18 container version >= 9.
   :border-index       (s/conditional (fn [_ context]
                                        (when (>= (or (:stash-container-version @context) 0) 9)
                                          :int32)))
   :border-color-index (s/conditional (fn [_ context]
                                        (when (>= (or (:stash-container-version @context) 0) 9)
                                          :int32)))
   :symbol-index       (s/conditional (fn [_ context]
                                        (when (>= (or (:stash-container-version @context) 0) 9)
                                          :int32)))
   :symbol-color-index (s/conditional (fn [_ context]
                                        (when (>= (or (:stash-container-version @context) 0) 9)
                                          :int32)))
   :button-name        (s/conditional (fn [_ context]
                                        (when (>= (or (:stash-container-version @context) 0) 9)
                                          (s/string :utf-16-le))))))

;; The stash array carries its own :int32 length prefix (handled by s/array),
;; followed by `stash-count` sub-blocks of id 0 (InventorySack). HEAD read only
;; up to the version here; keep the structured read but make sure the container
;; version is set on the context first so the item tail + per-tab conditionals
;; below can branch on it.
(def ^:private Block18-inner
  (s/struct-def
   :unknown   :int32-
   :mod       (s/string :ascii)
   :expansion-status :byte
   :stash     (s/array
               (struct-block {0 InventorySack}))))

(defn- read-block18!
  [^ByteBuffer bb context]
  (let [version (gdc/read-int! bb context)
        _ (swap! context assoc :stash-container-version version)]
    (assoc (s/read-struct Block18-inner bb context)
           :version version)))

(defn- write-block18!
  [^ByteBuffer bb data context]
  (let [version (:version data)]
    (gdc/write-int! bb version context)
    (swap! context assoc :stash-container-version version)
    (s/write-struct Block18-inner bb data context)))

(def Block18
  (s/struct-def
   {:struct/read  read-block18!
    :struct/write write-block18!}))

;; The transfer stash file seem to have a
(defn make-enc-context
  [& rest]
  (let [context (apply gdc/make-enc-context rest)]
    (swap! context update-in [:rw-fns] assoc
           :int32- [:int32- 4 read-int-no-update write-int-no-update])
    context))

(defn load-stash-file
  [filepath]

  (let [bb ^ByteBuffer (u/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) 1431655765)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table)

        magic-number (gdc/read-int! bb enc-context)]
    (when (not= magic-number 2)
      (throw (Throwable. "I don't understand this stash format!")))

    (merge
      (gdc/read-block bb enc-context {18 Block18})
      {:meta-stash-seed seed
       :meta-stash-loaded-from filepath})))

(defn write-stash-file
  [stash savepath]
  (let [bb (ByteBuffer/allocate (* 10 1024 1024))
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)

        seed (:meta-stash-seed stash)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table {:direction :write})]

    (.putInt bb (bit-xor seed 1431655765))  ;; enc key
    (gdc/write-int! bb 2 enc-context)       ;; magic number

    (gdc/write-block bb stash enc-context {18 Block18})

    ;; Dump everything to file
    (.flip bb)
    (gdc/write-to-file bb savepath)))


(comment
  (load-stash-file (dirs/get-transfer-stash))
  (with-bindings {#'gd-edit.io.gdc/*debug* true
                  #'gd-edit.structure/*debug* true}
    (load-stash-file "/Users/Odie/tmp/gd-stash.gst")
    )

  (load-stash-file "/Volumes/Untitled/Users/Odie/Documents/my games/Grim Dawn/save/transfer.gst")

  (let [stash (load-stash-file "/Volumes/Untitled/Users/Odie/Documents/my games/Grim Dawn/save/transfer.gst")]
    (write-stash-file stash "/Users/Odie/tmp/out.gst"))

  (load-stash-file "/Users/Odie/tmp/out.gst")

  )
