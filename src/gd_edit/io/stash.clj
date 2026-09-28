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

;;------------------------------------------------------------------------------
;; GD 1.3 目录容器（transmutes/reagents 等公共仓库附属页，magic=1）
;;
;; 结构：16B 头 + magic(1) + block-id + block-length + 内容 + 校验和。
;; 内容 = 版本 int + 二进制头 + 一串 {掩码物品路径} + {尾随二进制元数据} 记录。
;; 物品路径用「每条记录独立」的 XOR 掩码混淆；此处只做物品路径提取：
;; 逐字节判断该位置是否为首字节掩码一致的 "records/" 特征串，再按掩码解码路径。
;;------------------------------------------------------------------------------

(def ^:private catalog-codepoint-valid?
  (fn [c]
    (or (<= (int \0) c (int \9))
        (<= (int \a) c (int \z))
        (<= (int \A) c (int \Z))
        (= c (int \/))
        (= c (int \.))
        (= c (int \_))
        (= c (int \-)))))

(def ^:private catalog-needle
  (mapv int "records/"))

(defn- catalog-needle-at?
  [raw i k]
  (loop [j 0]
    (if (= j (count catalog-needle))
      true
      (and (= (bit-and 0xff (bit-xor (nth raw (+ i j)) k))
              (nth catalog-needle j))
           (recur (inc j))))))

(defn- catalog-decode-path
  [raw i k]
  (let [n (count raw)
        sb (java.lang.StringBuilder.)]
    (loop [j i]
      (let [c (bit-and 0xff (bit-xor (nth raw j) k))]
        (when (catalog-codepoint-valid? c)
          (.append sb (char c))
          (when (< (inc j) n) (recur (inc j))))))
    (let [s (str sb)
          idx (.indexOf s ".dbr")]
      (when (>= idx 0)
        (subs s 0 (+ idx 4))))))

(defn- extract-catalog-paths
  "从解密后内容中逐串自检出全部物品 dbr 路径。"
  [^bytes content]
  (let [raw (mapv #(bit-and 0xff (long %)) content)
        n (count raw)
        m (count catalog-needle)]
    (loop [i 0
           acc (transient [])]
      (if (>= i (- n m))
        (persistent! acc)
        (let [k (bit-xor (nth raw i) (nth catalog-needle 0))]
          (if (catalog-needle-at? raw i k)
            (if-let [path (catalog-decode-path raw i k)]
              (recur (+ i m 1) (conj! acc path))
              (recur (inc i) acc))
            (recur (inc i) acc)))))))

(defn load-catalog-file
  "读取 GD 1.3 magic=1 目录容器（如 transmutes.gst/reagents.gst），
   仅提取全部物品 dbr 路径。返回 {:item-paths [...] :items n :meta-catalog-id id}。"
  [filepath]
  (let [bb ^ByteBuffer (u/file-contents filepath)
        _ (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)
        seed (bit-xor (Integer/toUnsignedLong (.getInt bb)) 1431655765)
        enc-table (gdc/generate-encryption-table seed)
        enc-context (make-enc-context seed enc-table)
        magic-number (gdc/read-int! bb enc-context)]
    (when (not= magic-number 1)
      (throw (Throwable. (str "无法解析该目录容器格式（magic=" magic-number " ≠ 1）"))))
    (let [id (gdc/read-int! bb enc-context)
          length (gdc/decrypt-int (.getInt bb) (:enc-state @enc-context))
          ^bytes content (byte-array length)
          _ (.get bb content 0 length)
          final-state (gdc/decrypt-bytes! content {:enc-state (:enc-state @enc-context)
                                                   :enc-table  (:enc-table @enc-context)})
          _ (swap! enc-context assoc :enc-state final-state)
          ;; 注：内容中 int 字段按大端倒序推进状态、字符串字节按顺序推进，二者混合；
          ;; 批量按顺序解密会导致整体状态与真实读写偏离，故校验和仅作参考不强制。
          checksum (Integer/toUnsignedLong (.getInt bb))
          paths (extract-catalog-paths content)]
      {:meta-catalog-id id
       :meta-checksum-match (= checksum (:enc-state @enc-context))
       :item-paths paths
       :items (count paths)})))

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
