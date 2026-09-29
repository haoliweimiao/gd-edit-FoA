(ns gd-edit.web.main
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [gd-edit.app-util :as au]
            [gd-edit.db-utils :as dbu]
            [gd-edit.game-dirs :as dirs]
            [gd-edit.globals :as globals]
            [gd-edit.inventory :as inv]
            [gd-edit.io.arc :as arc]
            [gd-edit.item-summary :as isummary]
            [gd-edit.io.arz :as arz]
            [gd-edit.io.gdc :as gdc]
            [gd-edit.io.stash :as st]
            [gd-edit.commands.item :as item]
            [gd-edit.utils :as u])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress URLDecoder]
           [java.nio.charset StandardCharsets]))

(def ^:private port
  (try (Long/parseLong (or (System/getenv "GD_EDIT_PORT") "8787"))
       (catch Exception _ 8787)))
(def ^:private base-url (str "http://127.0.0.1:" port))
(def ^:private local-en (atom {}))
(def ^:private local-zh (atom {}))
(def ^:private local-zh-merged (atom {}))
(def ^:private page-html (atom nil))

(defn- load-lang-table!
  [relative-path]
  (try
    (->> (dirs/get-file-and-overrides relative-path)
         (map arc/load-localization-table)
         (apply merge {}))
    (catch Throwable _ {})))

(defn- load-txt-l10n-map
  "加载游戏 Settings/text_zh/*.txt 明文汉化覆盖表（游戏内实际生效的社区汉化）。
  条目格式 key=value（UTF-8，可含 BOM/CR），空值与 # 注释行跳过。"
  [root]
  (try
    (let [root-file (io/file root)]
      (when (.isDirectory root-file)
        (->> (file-seq root-file)
             (filter (fn [f] (and (.isFile f) (str/ends-with? (.getName f) ".txt"))))
             (reduce (fn [m f]
                       (into m
                             (keep (fn [ln]
                                     (let [l (str/trim (str/replace (str/replace ln #"^\uFEFF" "")
                                                                     #"\r$" ""))]
                                       (when (and (seq l)
                                                  (not (str/starts-with? l "#"))
                                                  (str/includes? l "="))
                                         (let [[k v] (str/split l #"=" 2)]
                                           [(str/trim k) (str/trim (or v ""))]))))
                                   (str/split-lines (slurp f :encoding "UTF-8")))))
                     {}))))
    (catch Throwable _ {})))

(defn- load-local-table!
  "载入游戏英/中词表用于解析名称 tag，失败时静默降级为空表。
  中文合并时跳过空串条目：中文包未翻译（空值）的词条回退英文。"
  []
  (reset! local-en (load-lang-table! "resources/Text_EN.arc"))
  (reset! local-zh (merge (load-lang-table! "resources/Text_ZH.arc")
                          (load-txt-l10n-map (str (io/file (dirs/get-game-dir)
                                                           "Settings/text_zh")))))
  (reset! local-zh-merged
          (merge @local-en
                 (into {} (remove (fn [[_ v]] (str/blank? v))) @local-zh))))

(defn- page-html!
  []
  (when (nil? @page-html)
    (reset! page-html (slurp (io/resource "web/index.html"))))
  @page-html)

(defn- resolve-tag
  [v table]
  (if (and (string? v) (str/starts-with? v "tag"))
    (get table (first (str/split v #"\|")) v)
    v))

(defn- resolve-tokens
  "把名称字符串里的 tag 词条逐词本地化（兜底：非 tag 原样保留）。"
  [s table]
  (if (and s (not (str/blank? s)))
    (->> (str/split s #"\s+")
         (map #(resolve-tag % table))
         (str/join " "))
    s))

(defn- lang-table
  "返回指定语言词表：zh 为中文（未翻译词条回退英文）；其余返回英文表。"
  [lang]
  (if (= lang "zh")
    @local-zh-merged
    @local-en))

(defn- fmt-playtime
  [secs]
  (let [h (quot secs 3600)
        m (quot (mod secs 3600) 60)
        s (mod secs 60)]
    (format "%02d:%02d:%02d" h m s)))

(def ^:private display-fields
  [["角色名" :character-name identity]
   ["职业" :player-class-name resolve-tag]
   ["性别" :male (fn [v] (if v "男" "女"))]
   ["等级" :character-level str]
   ["硬核模式" :hardcore-mode (fn [v] (if v "是" "否"))]
   ["金钱 (铁币)" :iron str]
   ["经验" :experience str]
   ["属性点" :attribute-points str]
   ["技能点" :skill-points str]
   ["虔诚点" :devotion-points str]
   ["体格" :physique pr-str]
   ["灵巧" :cunning pr-str]
   ["心智" :spirit pr-str]
   ["生命" :health pr-str]
   ["能量" :energy pr-str]
   ["游玩时间" :playtime-seconds fmt-playtime]
   ["死亡次数" :death-count str]
   ["击杀数" :kill-count str]])

(defn- characters-payload
  []
  {:characters
   (->> (au/character-list)
        (sort-by :character-name)
        (mapv (fn [c] {:name (:character-name c)
                       :path (:gdc-path c)})))})

(defn- character-payload
  [path lang]
  (let [c (gdc/load-character-file path)
        table (lang-table lang)
        attrs (mapv (fn [[label k fmt]]
                      (let [v (get c k)]
                        {:key (name k)
                         :label label
                         :value (if (nil? v) ""
                                  (if (= k :player-class-name)
                                    (resolve-tag v table)
                                    (fmt v)))}))
                    display-fields)]
    {:name (:character-name c)
     :path path
     :attributes attrs}))

(declare send-json!)

(def ^:private int32-max 2147483647)

(defn- valid-amount?
  [a]
  (and (integer? a) (<= 0 a int32-max)))

(defn- backup-copy!
  "按 CLI 约定把档案备份为 .bak{N}（递增），返回备份路径。"
  [filepath]
  (let [f (io/file filepath)
        filename (.getName f)
        siblings (->> (.listFiles (.getParentFile f))
                      (filter #(.isFile %))
                      (filter #(str/starts-with? (.getName %) filename))
                      (filter #(re-matches #"\.bak.*$" (subs (.getName %) (count filename)))))
        backup-path (str filepath ".bak" (inc (count siblings)))]
    (io/copy f (io/file backup-path))
    backup-path))

(def ^:private backup-suffix-re #"\.bak-?\d+$")

(defn- backup-files-of
  "该存档同目录下所有合法备份（.bak{N} 与 .bak-YYYYMMDD），按序号升序。
   不匹配后缀的忽略，绝不含存档本体。"
  [path]
  (let [f (io/file path)
        filename (.getName f)
        dir (.getParentFile f)]
    (->> (.listFiles dir)
         (filter #(.isFile %))
         (keep (fn [bf]
                 (let [nm (.getName bf)]
                   (when (and (str/starts-with? nm filename)
                              (re-matches backup-suffix-re (subs nm (count filename))))
                     {:file nm
                      :size (.length bf)
                      :mtime (.lastModified bf)
                      :index (Long/parseLong (str/replace (subs nm (+ (count filename) 4)) "-" ""))}))))
         (sort-by :index)
         (vec))))

(defn- char-backups-payload
  [path]
  {:path path :backups (backup-files-of path)})

(defn- delete-backs!
  "删除指定备份文件名（仅 .bak{N}/.bak-YYYYMMDD，纯文件名、须在存档同目录）。
   逐个尝试，不匹配/不存在记入 failed，不中断。"
  [path names]
  (let [f (io/file path)
        filename (.getName f)
        dir (.getParentFile f)
        good? (fn [nm]
                (and (string? nm)
                     (= nm (.getName (io/file nm)))
                     (str/starts-with? nm filename)
                     (re-matches backup-suffix-re (subs nm (count filename)))))
        files (map (fn [nm] [nm (io/file dir nm)]) names)]
    (when-not (every? (comp good? first) files)
      (throw (ex-info "存在非法的备份文件名" {:status 400})))
    (reduce (fn [acc [nm bf]]
              (if (and (.isFile bf) (.delete bf))
                (update acc :deleted conj nm)
                (update acc :failed conj nm)))
            {:ok true :deleted [] :failed []}
            files)))

(defn- write-money!
  [path amount]
  (let [backup (backup-copy! path)
        c (gdc/load-character-file path)]
    (gdc/write-character-file (assoc c :iron amount) path)
    (let [actual (get (gdc/load-character-file path) :iron)]
      (if (= actual amount)
        {:ok true :iron actual :backup backup}
        (throw (ex-info "写入校验失败" {:expected amount :actual actual}))))))

(defn- request-body
  [exchange]
  (slurp (.getRequestBody exchange)))

(defn- money-request-payload
  [exchange]
  (try
    (json/read-str (request-body exchange) :key-fn keyword)
    (catch Throwable _ nil)))

(defn- handle-money-post
  [exchange]
  (let [body (money-request-payload exchange)
        path (:path body)
        amount (:amount body)]
    (cond
      (nil? body)
      (send-json! exchange 400 {:ok false :error "请求体不是合法 JSON"})

      (not (and (string? path) (.isFile (io/file path))))
      (send-json! exchange 400 {:ok false :error "无效的角色档案路径"})

      (not (valid-amount? amount))
      (send-json! exchange 400 {:ok false :error "金额必须是 0 ~ 2147483647 的整数"})

      :else
      (try
        (send-json! exchange 200 (write-money! path amount))
        (catch Throwable e
          (send-json! exchange 500 {:ok false :error (str e)}))))))


;;------------------------------------------------------------------------------
;; 公共仓库（只读查看）
;;------------------------------------------------------------------------------

(defn- stash-save-root
  []
  (->> (dirs/get-save-dir-search-list)
       (map #(.getParentFile (io/file %)))
       (map str)
       first))

(def ^:private gdx3-loaded (atom false))

(defn- ensure-gdx3-loaded!
  "基础 DB/贴图覆盖目录不含 GDX3（FoA）。此处补充 GDX3 记录与贴图尺寸，
  使 gdx3 物品也能解析出名称、稀有度与真实占格。"
  []
  (when-not @gdx3-loaded
    (locking gdx3-loaded
      (when-not @gdx3-loaded
        (let [gdx3-dir (dirs/get-gdx3-dir)
              gdx3-db (io/file gdx3-dir "database/GDX3.arz")
              gdx3-tex (io/file gdx3-dir "resources/Items.arc")]
          ;; 等待基础 DB（含本地化）加载完成
          ;; globals/db 等根值为 future；全库用 @ 读取，须保持 IDeref(future) 语义，
          ;; 因此以「新 future 把 gdx3 合并进基础数据」替代根值。
          @globals/db-and-index
          (when (.isFile gdx3-db)
            (let [extra (arz/load-game-db (str gdx3-db) @globals/localization-table)]
              (when (seq extra)
                (let [extra-idx (au/build-db-index extra)]
                  (alter-var-root #'globals/db (fn [f] (future (concat @f extra))))
                  (alter-var-root #'globals/db-index (fn [f] (future (merge @f extra-idx))))
                  (alter-var-root #'globals/db-and-index (fn [f] (future (au/build-db-and-index))))))))
          (when (.isFile gdx3-tex)
            (alter-var-root #'inv/texture-dim-fns
                            (fn [fns] (concat fns (map inv/make-dims-lookup-fn [gdx3-tex]))))
            (reset! inv/texture-dim-cache (inv/make-cache))))
        (reset! gdx3-loaded true))))
  nil)

(defn- stash-file-path
  [mode]
  (str (io/file (stash-save-root)
                (if (= mode "hardcore") "transfer.gsh" "transfer.gst"))))

(defn- slot-coord
  "v11 公共仓库的 X/Y 为 IEEE 浮点位模式；合法浮点坐标按 float 解释，否则回退整数。"
  [bits]
  (let [f (Float/intBitsToFloat (int bits))]
    (if (and (>= f 0) (< f 64) (== f (Math/floor f)))
      f
      bits)))

(defn- item-rarity
  [basename]
  (when-let [rec (get (dbu/db-recordname-index) basename)]
    (get rec "itemClassification")))

(defn- item-level
  [basename]
  (when-let [rec (get (dbu/db-recordname-index) basename)]
    (let [v (get rec "itemLevel")]
      (when (and v (number? v)) v))))

(defn- stash-item-dims
  [item]
  (try
    (inv/item->dims item)
    (catch Throwable _ nil)))

(defn- affix-name
  "把词缀 dbr 路径映射为 DB 中记录的显示名（lootRandomizerName），
  与游戏内一致；无法解析时保留原值。"
  [path]
  (if (and path (not (str/blank? path)))
    (if-let [rec (get @globals/db-index path)]
      (let [nm (str (get rec "lootRandomizerName"))]
        (if (str/blank? nm) path nm))
      path)
    path))

(defn- baseitem-loottable-path
  [dbr]
  (as-> (str/split dbr #"/") $
        (nth $ 2)
        (str "records/items/loottables/" $ "/")))

(defn- all-loottable-records
  [db]
  (filter (fn [item]
            (str/starts-with? (:recordname item) "records/items/loottables/"))
          db))

(defn- loottable-records-with-mentions
  [loottables mentioned-value]
  (filter (fn [item]
            (some #(= (val %) mentioned-value) item))
          loottables))

(defn- baseitem-valid-affix-paths
  "给定基础件 dbr 路径，回溯其可合法 roll 的前缀/后缀集合（借鉴 commands/item.clj）。
   依据：引用该基础件的 loottable 里 prefix/suffix 表（含稀有变体）中的 randomizerName 全集。
   神话/苏醒/传奇等特殊档、以及 db-index 外记录不会被任何 loottable 引用，返回空集。"
  [baseitem-dbr-path]
  (try
    (let [db (dbu/db)
          db-idx (dbu/db-recordname-index)
          loottables (loottable-records-with-mentions
                      (all-loottable-records db) baseitem-dbr-path)
          collect (fn [key-prefixes]
                    (->> (mapcat (fn [k]
                                   (mapcat #(u/collect-values-with-key-prefix % k) loottables))
                                 key-prefixes)
                         (map #(u/collect-values-with-key-prefix (get db-idx %) "randomizerName"))
                         (apply concat)
                         (into #{})))]
      {:prefix (collect ["prefixTableName" "rarePrefixTableName"])
       :suffix (collect ["suffixTableName" "rareSuffixTableName"])})
    (catch Throwable _ {:prefix #{} :suffix #{}})))

(defn- strip-color-codes
  "去除 GD 文本颜色码（如 ^g/^y/^l）残留。"
  [s]
  (when s (str/replace s #"\^[A-Za-z]" "")))

(defn- stash-items-payload
  [sack table]
  (let [items (:inventory-items sack)]
    (mapv (fn [idx it]
            (let [nm (try (dbu/item-name it) (catch Throwable _ nil))]
              {:slot idx
               :name (strip-color-codes (resolve-tokens nm table))
               :baseName (:basename it)
               :stackCount (:stack-count it)
               :x (slot-coord (:X it))
               :y (slot-coord (:Y it))
               :w (:width (stash-item-dims it))
               :h (:height (stash-item-dims it))
               :rarity (item-rarity (:basename it))
               :level (item-level (:basename it))
               :prefixName (strip-color-codes (affix-name (:prefix-name it)))
               :suffixName (strip-color-codes (affix-name (:suffix-name it)))}))
           (range)
           items)))

(declare catalog-items catalog-kind-labels catalog-kinds)

(defn- ansi-runs
  "把带 Jansi ANSI SGR 色码的文本解析为 [text color] run 列表。
  仅关注 item-summary 用到的前景色：33 黄 / 31 红 / 32 绿 / 1 粗体 / 0 复位。"
  [s]
  (loop [m (re-matcher #"\u001b\[([0-9;]*)m" s)
         pos 0
         color nil
         runs (transient [])]
    (if (.find m)
      (let [start (.start m)
            end (.end m)
            text (subs s pos start)
            code (.group m 1)]
        (recur m end
               (case code
                 "0" nil
                 "31" :r
                 "32" :g
                 "33" :y
                 "1" color
                 color)
               (if (seq text)
                 (conj! runs [text color])
                 runs)))
      (let [text (subs s pos)]
        (persistent!
         (if (seq text)
           (conj! runs [text color])
           runs))))))

(defn- strip-gd-markup
  "去除 GD 文本标记：颜色码（^g/^F/^c 等）与技能引用的 <> 尖括号标记
  （游戏内不显示尖括号本体）。仅用于详情管线，不动网格名称的 strip-color-codes。"
  [s]
  (when s (-> s (str/replace #"\^[A-Za-z]" "") (str/replace #"[<>]" ""))))

(defn- item-detail-runs
  "item-summary 输出（去掉首行名称）逐行转为 [[text class] ...]；文本本地化并去 GD 标记。"
  [item table]
  (try
    (->> (rest (isummary/item-summary item))
         (mapv (fn [line]
                 (->> (ansi-runs line)
                      (mapv (fn [[text color]]
                              [(resolve-tokens (strip-gd-markup text) table)
                               (when color (name color))]))))))
    (catch Throwable _ [])))

(defn- affix-paths-payload
  "合法词缀集合 → [{path name level}]（名称本地化，按名称排序）。"
  [paths table]
  (->> paths
       (keep (fn [p]
               (when-let [rec (get @globals/db-index p)]
                 (let [nm (str (get rec "lootRandomizerName"))]
                   {:path p
                    :name (strip-color-codes
                           (resolve-tokens (if (str/blank? nm) p nm) table))
                    :level (get rec "itemLevel")}))))
       (sort-by (comp str/lower-case :name))
       (vec)))

(defn- item-detail-payload
  [it table]
  (let [nm (try (dbu/item-name it) (catch Throwable _ nil))
        dims (stash-item-dims it)
        va (baseitem-valid-affix-paths (:basename it))]
    {:name (strip-gd-markup (resolve-tokens nm table))
     :baseName (:basename it)
     :rarity (item-rarity (:basename it))
     :stackCount (:stack-count it)
     :w (:width dims)
     :h (:height dims)
     :lines (item-detail-runs it table)
     :prefixPath (:prefix-name it)
     :suffixPath (:suffix-name it)
     :validAffixes {:prefix (affix-paths-payload (:prefix va) table)
                    :suffix (affix-paths-payload (:suffix va) table)}}))

(defn- stash-item-detail-payload
  [sack slot table]
  (when-let [it (nth (:inventory-items sack) slot nil)]
    (item-detail-payload it table)))

(defn- stash-payload
  [mode]
  (let [path (stash-file-path mode)
        s (st/load-stash-file path)]
    {:exists true
     :mode mode
     :file path
     :has-hardcore (.isFile (io/file (stash-file-path "hardcore")))
      :extra (vec (keep (fn [kind]
                          (when-let [records (catalog-items kind mode)]
                            {:kind kind
                             :label (get catalog-kind-labels kind kind)
                             :itemCount (count records)}))
                        catalog-kinds))
     :pages (->> (:stash s)
                 (map-indexed
                  (fn [idx sack]
                    {:index idx
                     :label (if (str/blank? (:button-name sack))
                              (str "第 " (inc idx) " 页")
                              (:button-name sack))
                     :width (:width sack)
                     :height (:height sack)
                     :itemCount (count (:inventory-items sack))}))
                 (vec))}))

(defn- stash-page-payload
  [mode index lang]
  (ensure-gdx3-loaded!)
  (let [s (st/load-stash-file (stash-file-path mode))
        sack (nth (:stash s) index nil)
        table (lang-table lang)]
    (when sack
      {:width (:width sack)
       :height (:height sack)
       :items (stash-items-payload sack table)})))

;; GD 1.3 目录附页（magic=1 容器）：镶嵌物+材料 目录，无网格，只有物品列表。
(def ^:private catalog-kinds
  ^:export ["reagents"])

(def ^:private catalog-kind-labels
  {"reagents" "镶嵌物·材料"})

;; 角色装备槽位（GD 1.3 档案 block3 equipment 数组顺序，实测 _purple 验证）。
(def ^:private char-equipment-labels
  ["头盔" "项链" "胸甲" "腿甲" "鞋" "手套"
   "戒指1" "戒指2" "腰带" "肩甲" "勋章" "神器"])

(defn- catalog-file-path
  [mode kind]
  (str (io/file (stash-save-root) (str kind (if (= mode "hardcore") ".gsh" ".gst")))))

(def ^:private catalog-cache (atom {}))

(defn- catalog-items
  "解析并缓存目录附页的记录（按 [文件+mtime] 缓存），records 含 :path/:count。"
  [kind mode]
  (let [f (io/file (catalog-file-path mode kind))]
    (when (.isFile f)
      (let [key (str (catalog-file-path mode kind) ":" (.lastModified f))
            hit (get @catalog-cache key ::missing)]
        (if (identical? hit ::missing)
          (let [records (try (:records (st/load-catalog-file (.getPath f)))
                             (catch Throwable _ nil))]
            (swap! catalog-cache assoc key records)
            records)
          hit)))))

(defn- catalog-page-payload
  [kind mode lang]
  (ensure-gdx3-loaded!)
  (let [table (lang-table lang)]
    (when-let [records (catalog-items kind mode)]
      {:kind kind
       :label (get catalog-kind-labels kind kind)
       :itemCount (count records)
       :items (mapv (fn [{:keys [path count]}]
                      (let [it {:basename path}]
                        {:name (or (strip-color-codes
                                    (resolve-tokens (try (dbu/item-name it) (catch Throwable _ nil))
                                                    table))
                                   (last (str/split path #"/")))
                         :baseName path
                         :rarity (item-rarity path)
                         :count (or count 1)}))
                    records)})))

;;------------------------------------------------------------------------------
;; 角色背包（只读查看）：背包袋 + 穿戴装备 + 备用武器组
;;------------------------------------------------------------------------------

(defn- char-item-summary
  "物品摘要（装备/武器组列表用）：名称、稀有度、叠堆。"
  [item table]
  (let [nm (try (dbu/item-name item) (catch Throwable _ nil))]
    {:name (strip-color-codes (resolve-tokens nm table))
     :baseName (:basename item)
     :rarity (item-rarity (:basename item))
     :stackCount (:stack-count item)}))

(defn- char-sack-items-payload
  "角色背包袋物品：坐标是普通整数（勿用共享仓库的 slot-coord 浮点位解码）。"
  [items table]
  (mapv (fn [idx it]
          (let [nm (try (dbu/item-name it) (catch Throwable _ nil))]
            {:slot idx
             :name (strip-color-codes (resolve-tokens nm table))
             :baseName (:basename it)
             :stackCount (:stack-count it)
             :x (:X it)
             :y (:Y it)
             :w (:width (stash-item-dims it))
             :h (:height (stash-item-dims it))
             :rarity (item-rarity (:basename it))
             :level (item-level (:basename it))
             :prefixName (strip-color-codes (affix-name (:prefix-name it)))
             :suffixName (strip-color-codes (affix-name (:suffix-name it)))}))
        (range)
        items))

(defn- sack-slot-dims
  "背包袋格数：袋0 用 inventory_grid0.dbr，其余用 grid1（取自 DB）。"
  [idx]
  (try (inv/inv-slot-dims [:inventory-sacks idx])
       (catch Throwable _ nil)))

(defn- character-inventory-payload
  [path lang]
  (ensure-gdx3-loaded!)
  (let [c (gdc/load-character-file path)
        table (lang-table lang)
        sacks (mapv (fn [idx sack]
                      (let [dims (sack-slot-dims idx)]
                        {:index idx
                         :unused (:unused sack)
                         :itemCount (count (:inventory-items sack))
                         :width (:width dims)
                         :height (:height dims)}))
                    (range) (:inventory-sacks c))]
    {:name (:character-name c)
     :path path
     :sacks sacks
     :equipment (mapv (fn [idx item]
                        (merge (char-item-summary item table)
                               {:slot idx
                                :label (get char-equipment-labels idx
                                            (str "装备位 " (inc idx)))}))
                      (range) (:equipment c))
     :weaponSets (mapv (fn [set-idx ws]
                         {:set set-idx
                          :unused (:unused ws)
                          :items (mapv (fn [idx item]
                                         (merge (char-item-summary item table)
                                                {:slot idx
                                                 :label (if (zero? idx) "主手" "副手")}))
                                       (range) (:items ws))})
                       (range) (:weapon-sets c))}))

(defn- character-inv-page-payload
  [path sack-index lang]
  (when-let [sack (nth (:inventory-sacks (gdc/load-character-file path))
                       sack-index nil)]
    (ensure-gdx3-loaded!)
    (let [dims (sack-slot-dims sack-index)]
      {:width (:width dims)
       :height (:height dims)
       :items (char-sack-items-payload (:inventory-items sack) (lang-table lang))})))

(defn- has-item?
  [it]
  (and it (not (str/blank? (:basename it)))))

(defn- character-inv-item-payload
  [path source sack-index set-index slot-index lang]
  (let [c (gdc/load-character-file path)]
    (case source
      "sack" (let [sack (nth (:inventory-sacks c) sack-index nil)]
               (when-let [it (and sack (nth (:inventory-items sack) slot-index nil))]
                 (when (has-item? it)
                   (item-detail-payload it (lang-table lang)))))
      "equip" (when-let [it (nth (:equipment c) slot-index nil)]
                (when (has-item? it)
                  (item-detail-payload it (lang-table lang))))
      "weapon" (when-let [ws (nth (:weapon-sets c) set-index nil)]
                 (when-let [it (nth (:items ws) slot-index nil)]
                   (when (has-item? it)
                     (item-detail-payload it (lang-table lang)))))
      nil)))

;;------------------------------------------------------------------------------
;; 物品池 + 新增背包物品（写档）
;;------------------------------------------------------------------------------

(def ^:private items-pool (atom nil))

(defn- items-pool-build
  "全物品基础池：records/items/ 下、剔除 loottables 等目录，且能解析出显示名的记录路径。
   名称只存原始串（含 tag），解析交给 lang 表按需进行（全量解析约几十 ms，无需预热缓存）。"
  []
  (or @items-pool
      (let [idx (dbu/db-recordname-index)
            recs (->> (keys idx)
                      (filter #(and (str/starts-with? % "records/items/")
                                    (not (str/starts-with? % "records/items/loottables/"))))
                      (keep (fn [p]
                              (let [nm (try (dbu/item-name {:basename p}) (catch Throwable _ nil))]
                                (when (and nm (not (str/blank? nm)))
                                  {:baseName p :name nm}))))
                      (sort-by (comp str/lower-case :name))
                      (vec))]
        (reset! items-pool recs)
        recs)))

(defn- items-search-payload
  [q offset limit lang]
  (ensure-gdx3-loaded!)
  (let [table (lang-table lang)
        all (items-pool-build)
        q (when (seq q) (str/lower-case (str/trim q)))
        filtered (if (seq q)
                   (filter (fn [{:keys [baseName name]}]
                             (let [nm (str/lower-case (str (strip-color-codes (resolve-tokens name table))))]
                               (or (str/includes? (str/lower-case baseName) q)
                                   (str/includes? nm q))))
                           all)
                   all)
        total (count filtered)
        page (->> filtered (drop (or offset 0)) (take limit))]
    {:total total
     :offset (or offset 0)
     :limit limit
     :items (mapv (fn [{:keys [baseName name]}]
                    (let [dims (stash-item-dims {:basename baseName})]
                      {:baseName baseName
                       :name (strip-color-codes (resolve-tokens name table))
                       :rarity (item-rarity baseName)
                       :level (item-level baseName)
                       :w (:width dims)
                       :h (:height dims)}))
                  page)}))

(defn- sack-occupied?!
  "背包袋内被占用的格子集合（纯坐标几何，不依赖全局 character）。
   无法解析占格的物品按 1x1 计。"
  [items width height]
  (set (for [it items
             :let [dims (stash-item-dims it)
                   w (or (:width dims) 1)
                   h (or (:height dims) 1)]
             dx (range (max 1 w))
             dy (range (max 1 h))
             :let [cx (+ (int (:X it)) dx) cy (+ (int (:Y it)) dy)]
             :when (and (<= 0 cx) (< cx width) (<= 0 cy) (< cy height))]
         [cx cy])))

(defn- add-char-item!
  "在角色背包袋 (x,y) 新增一件 new-baseName，数量 qty（总是新增独立条目）。
   空间不足（越界/重叠）抛 :status 409，禁止写入。"
  [path sack-index x y new-baseName qty]
  (ensure-gdx3-loaded!)
  (let [c (gdc/load-character-file path)
        sack (nth (:inventory-sacks c) sack-index nil)
        idx (dbu/db-recordname-index)]
    (when (nil? sack)
      (throw (ex-info "背包袋不存在" {:status 400})))
    (when (:unused sack)
      (throw (ex-info "该背包袋未解锁，无法放入物品" {:status 400})))
    (when-not (contains? idx new-baseName)
      (throw (ex-info "物品不存在于数据库中" {:status 400})))
    (let [{dwidth :width dheight :height} (sack-slot-dims sack-index)
          dims (stash-item-dims {:basename new-baseName})
          w (:width dims) h (:height dims)]
      (when (or (nil? w) (nil? h) (not (pos? w)) (not (pos? h)))
        (throw (ex-info "无法确定该物品的占格大小，拒绝新增" {:status 400})))
      (when (or (nil? dwidth) (nil? dheight))
        (throw (ex-info "无法确定背包袋尺寸" {:status 500})))
      (when-not (and (<= 0 x) (<= 0 y) (<= (+ x w) dwidth) (<= (+ y h) dheight))
        (throw (ex-info "无法放置：位置超出背包范围，新增失败" {:status 409})))
      (let [occ (sack-occupied?! (:inventory-items sack) dwidth dheight)
            cells (for [dx (range w) dy (range h)] [(+ x dx) (+ y dy)])]
        (when-not (every? #(not (contains? occ %)) cells)
          (throw (ex-info "无法放置：该位置已被物品占用，新增失败" {:status 409}))))
      (let [backup (backup-copy! path)
            it (merge (item/blank-item)
                      {:basename new-baseName
                       :stack-count qty
                       :X x :Y y
                       ;; GD 1.3 v13 物品尾部必需字段，缺失会导致写档断言失败
                       :ascendant "" :ascendant2h ""
                       :materia-combines 0 :rerolls 0 :v11-extra 0})
            c2 (update-in c [:inventory-sacks sack-index :inventory-items] conj it)]
        (gdc/write-character-file c2 path)
        (let [items (get-in (gdc/load-character-file path)
                            [:inventory-sacks sack-index :inventory-items])
              last-it (last items)]
          (if (and (= (clojure.core/count items) (inc (clojure.core/count (:inventory-items sack))))
                   (= (:basename last-it) new-baseName)
                   (= (:X last-it) x)
                   (= (:Y last-it) y)
                   (= (:stack-count last-it) qty))
            {:ok true :backup backup
             :item {:baseName new-baseName :x x :y y :count qty}}
            (throw (ex-info "写入校验失败，数据未新增" {:status 500}))))))))

(defn- delete-char-item!
  "删除角色背包袋 slot 处的物品（先备份，写档后重载校验数量减一且被删物品三元组消失）。"
  [path sack-index slot]
  (let [c (gdc/load-character-file path)
        sack (nth (:inventory-sacks c) sack-index nil)
        items (:inventory-items sack)]
    (when (nil? sack)
      (throw (ex-info "背包袋不存在" {:status 400})))
    (when (or (nil? slot) (not (integer? slot)) (neg? slot) (>= slot (count items)))
      (throw (ex-info "无效的物品槽位" {:status 400})))
    (let [it (nth items slot nil)]
      (when (or (nil? it) (str/blank? (:basename it)))
        (throw (ex-info "该位置没有物品" {:status 400})))
      (let [removed {:basename (:basename it) :x (:X it) :y (:Y it)}
            backup (backup-copy! path)
            c2 (update-in c [:inventory-sacks sack-index :inventory-items]
                          (fn [v] (into (subvec v 0 slot) (subvec v (inc slot)))))]
        (gdc/write-character-file c2 path)
        (let [items' (get-in (gdc/load-character-file path)
                             [:inventory-sacks sack-index :inventory-items])]
          (if (and (= (clojure.core/count items') (dec (clojure.core/count items)))
                   (not-any? (fn [x] (and (= (:basename x) (:basename removed))
                                          (= (:X x) (:x removed))
                                          (= (:Y x) (:y removed))))
                             items'))
            {:ok true :backup backup :removed (:basename it)}
            (throw (ex-info "写入校验失败，物品未删除" {:status 500}))))))))

(def ^:private affix-recordname-re #"^records/items/lootaffixes/(prefix|suffix)/[A-Za-z0-9_.-]+\.dbr$")

(defn- char-item-loc
  "定位角色内物品：返回 [gdc-data keypath item]；参数无效/越界/空槽返回 nil。"
  [path source sack-idx set-idx slot-idx]
  (let [c (gdc/load-character-file path)]
    (case source
      "sack" (when (and (some? sack-idx) (some? slot-idx))
               (when-let [sack (nth (:inventory-sacks c) sack-idx nil)]
                 (when (and (<= 0 slot-idx) (< slot-idx (count (:inventory-items sack))))
                   (when-let [it (nth (:inventory-items sack) slot-idx nil)]
                     (when (has-item? it)
                       [c [:inventory-sacks sack-idx :inventory-items slot-idx] it])))))
      "equip" (when (and (some? slot-idx) (<= 0 slot-idx) (< slot-idx (count (:equipment c))))
                (when-let [it (nth (:equipment c) slot-idx nil)]
                  (when (has-item? it)
                    [c [:equipment slot-idx] it])))
      "weapon" (when (and (some? set-idx) (some? slot-idx))
                 (when-let [ws (nth (:weapon-sets c) set-idx nil)]
                   (when (and (<= 0 slot-idx) (< slot-idx (count (:items ws))))
                     (when-let [it (nth (:items ws) slot-idx nil)]
                       (when (has-item? it)
                         [c [:weapon-sets set-idx :items slot-idx] it]))))))))

(defn- set-char-item-affix!
  "修改角色指定槽位物品的前缀/后缀（affix-path 空串或 nil = 清除词缀）。
   写档前自动备份；重载校验 basename 不变且新字段值正确。"
  [path source sack-idx set-idx slot-idx kind affix-path]
  (ensure-gdx3-loaded!)
  (when-not (contains? #{"prefix" "suffix"} kind)
    (throw (ex-info "无效的词缀类型" {:status 400})))
  (when (and (seq affix-path) (not (re-matches affix-recordname-re affix-path)))
    (throw (ex-info "无效的词缀路径" {:status 400})))
  (let [[c kp it] (or (char-item-loc path source sack-idx set-idx slot-idx)
                      (throw (ex-info "物品不存在或定位参数无效" {:status 400})))
        base (:basename it)
        new-affix (if (seq affix-path) affix-path "")
        field (if (= kind "prefix") :prefix-name :suffix-name)
        other-field (if (= kind "prefix") :suffix-name :prefix-name)
        valid (baseitem-valid-affix-paths base)
        valid-set (get valid (keyword kind))]
    (when (and (seq affix-path) (not (contains? valid-set affix-path)))
      (throw (ex-info "该词缀不属于此装备的合法词缀" {:status 400})))
    (let [backup (backup-copy! path)
          c2 (assoc-in c (conj kp field) new-affix)]
      (gdc/write-character-file c2 path)
      (let [item' (get-in (gdc/load-character-file path) kp)]
        (if (and (= (:basename item') base)
                 (= (get item' field) new-affix))
          {:ok true
           :backup backup
           :prefix (if (= kind "prefix") new-affix (get item' other-field))
           :suffix (if (= kind "suffix") new-affix (get item' other-field))
           :name (try (strip-gd-markup (resolve-tokens (dbu/item-name item') (lang-table "zh")))
                      (catch Throwable _ nil))}
          (throw (ex-info "写入校验失败，词缀未修改" {:status 500})))))))

(defn- handle-inventory-item-delete
  [exchange]
  (let [body (money-request-payload exchange)
        path (:path body)
        sack (:sack body)
        slot (:slot body)]
    (cond
      (nil? body)
      (send-json! exchange 400 {:ok false :error "请求体不是合法 JSON"})

      (not (and (string? path) (.isFile (io/file path))))
      (send-json! exchange 400 {:ok false :error "无效的角色档案路径"})

      (not (and (integer? sack) (<= 0 sack)))
      (send-json! exchange 400 {:ok false :error "无效的背包袋序号"})

      (not (and (integer? slot) (<= 0 slot)))
      (send-json! exchange 400 {:ok false :error "无效的物品槽位"})

      :else
      (try
        (send-json! exchange 200 (delete-char-item! path sack slot))
        (catch clojure.lang.ExceptionInfo e
          (send-json! exchange (or (:status (ex-data e)) 500)
                      {:ok false :error (.getMessage e)}))
        (catch Throwable e
          (send-json! exchange 500 {:ok false :error (str e)}))))))

(defn- handle-inventory-item-post
  [exchange]
  (let [body (money-request-payload exchange)
        path (:path body)
        sack (:sack body)
        x (:x body) y (:y body)
        basename (:basename body)
        count (or (:count body) 1)]
    (cond
      (nil? body)
      (send-json! exchange 400 {:ok false :error "请求体不是合法 JSON"})

      (not (and (string? path) (.isFile (io/file path))))
      (send-json! exchange 400 {:ok false :error "无效的角色档案路径"})

      (not (and (integer? sack) (<= 0 sack)))
      (send-json! exchange 400 {:ok false :error "无效的背包袋序号"})

      (not (and (integer? x) (integer? y) (<= 0 x) (<= 0 y)))
      (send-json! exchange 400 {:ok false :error "无效的坐标"})

      (not (and (string? basename) (str/starts-with? basename "records/items/")))
      (send-json! exchange 400 {:ok false :error "无效的物品路径"})

      (not (and (integer? count) (<= 1 count 9999)))
      (send-json! exchange 400 {:ok false :error "数量必须是 1 ~ 9999 的整数"})

      :else
      (try
        (send-json! exchange 200 (add-char-item! path sack x y basename count))
        (catch clojure.lang.ExceptionInfo e
          (send-json! exchange (or (:status (ex-data e)) 500)
                      {:ok false :error (.getMessage e)}))
        (catch Throwable e
          (send-json! exchange 500 {:ok false :error (str e)}))))))

(defn- parse-query-param
  [query key]
  (->> (str/split (or query "") #"&")
       (keep (fn [kv]
               (let [[k v] (str/split kv #"=" 2)]
                 (when (= k key)
                   (URLDecoder/decode (or v "") "UTF-8")))))
        first))

(defn- parse-int-param
  [query key default]
  (let [raw (parse-query-param query key)]
    (if raw
      (try (Integer/parseInt raw) (catch Throwable _ nil))
      default)))

(defn- handle-inventory-item-affix
  "POST /api/character/inventory/item/affix {path q kind affixPath}
   q 为与物品详情端点相同的定位串（source&sack&set&slot），kind∈{prefix,suffix}，
   affixPath 空串/nil=清除该词缀。"
  [exchange]
  (let [body (money-request-payload exchange)
        path (:path body)
        q (:q body)
        kind (:kind body)
        affix-path (:affixPath body)]
    (cond
      (nil? body)
      (send-json! exchange 400 {:ok false :error "请求体不是合法 JSON"})

      (not (and (string? path) (.isFile (io/file path))))
      (send-json! exchange 400 {:ok false :error "无效的角色档案路径"})

      (not (and (string? q) (seq q)))
      (send-json! exchange 400 {:ok false :error "缺少物品定位参数(q)"})

      (not (contains? #{"prefix" "suffix"} kind))
      (send-json! exchange 400 {:ok false :error "无效的词缀类型"})

      :else
      (let [source (or (parse-query-param q "source") "sack")
            sack (parse-int-param q "sack" nil)
            set (parse-int-param q "set" nil)
            slot (parse-int-param q "slot" nil)]
        (cond
          (not (contains? #{"sack" "equip" "weapon"} source))
          (send-json! exchange 400 {:ok false :error "未知的 source（应为 sack/equip/weapon）"})

          (nil? slot)
          (send-json! exchange 400 {:ok false :error "缺少 slot 参数"})

          :else
          (try
            (send-json! exchange 200
                        (set-char-item-affix! path source sack set slot kind affix-path))
            (catch clojure.lang.ExceptionInfo e
              (send-json! exchange (or (:status (ex-data e)) 500)
                          {:ok false :error (.getMessage e)}))
            (catch Throwable e
              (send-json! exchange 500 {:ok false :error (str e)}))))))))

(defn- handle-char-backups
  [exchange query]
  (let [p (parse-query-param query "path")]
    (if (and (string? p) (.isFile (io/file p)))
      (send-json! exchange 200 (char-backups-payload p))
      (send-json! exchange 400 {:error "无效的角色档案路径"}))))

(defn- handle-backups-delete
  [exchange]
  (let [body (money-request-payload exchange)
        path (:path body)
        files (:files body)]
    (cond
      (nil? body)
      (send-json! exchange 400 {:ok false :error "请求体不是合法 JSON"})

      (not (and (string? path) (.isFile (io/file path))))
      (send-json! exchange 400 {:ok false :error "无效的角色档案路径"})

      (not (and (vector? files) (seq files) (every? string? files)))
      (send-json! exchange 400 {:ok false :error "缺少合法的备份文件列表"})

      :else
      (try
        (send-json! exchange 200 (delete-backs! path files))
        (catch clojure.lang.ExceptionInfo e
          (send-json! exchange (or (:status (ex-data e)) 500)
                      {:ok false :error (.getMessage e)}))
        (catch Throwable e
          (send-json! exchange 500 {:ok false :error (str e)}))))))

(defn- handle-items-search
  [exchange query]
  (let [q (parse-query-param query "q")
        offset (or (parse-int-param query "offset" 0) 0)
        limit (min (or (parse-int-param query "limit" 50) 50) 200)
        lang (or (parse-query-param query "lang") "zh")]
    (try
      (send-json! exchange 200 (items-search-payload q offset limit lang))
      (catch Throwable e
        (send-json! exchange 500 {:error (str e)})))))

(defn- send!
  [exchange status body content-type]
  (let [bytes (.getBytes body StandardCharsets/UTF_8)]
    (.sendResponseHeaders exchange status (long (alength bytes)))
    (when (pos? (alength bytes))
      (with-open [os (.getResponseBody exchange)]
        (.write os bytes)))))

(defn- send-json!
  [exchange status data]
  (send! exchange status (json/write-str data) "application/json; charset=utf-8"))

(defn- send-html!
  [exchange status html]
  (send! exchange status html "text/html; charset=utf-8"))

(defn- handle-request
  [exchange]
  (try
    (let [uri (.getRequestURI exchange)
          path (.getPath uri)
          query (.getRawQuery uri)
          method (.getRequestMethod exchange)]
      (cond
        (= path "/")
        (send-html! exchange 200 (page-html!))

        (= path "/api/characters")
        (send-json! exchange 200 (characters-payload))

        (and (= path "/api/character") query)
        (send-json! exchange 200
                    (character-payload (parse-query-param query "path")
                                       (or (parse-query-param query "lang") "zh")))

        (and (= path "/api/character/money") (= method "POST"))
        (handle-money-post exchange)

        (and (= path "/api/character/backups") query)
        (handle-char-backups exchange query)

        (and (= path "/api/character/backups/delete") (= method "POST"))
        (handle-backups-delete exchange)

        (= path "/api/character/money")
        (send-json! exchange 405 {:error "method not allowed"})

        (and (= path "/api/items") query)
        (handle-items-search exchange query)

        (and (= path "/api/character/inventory") query)
        (let [p (parse-query-param query "path")
              lang (or (parse-query-param query "lang") "zh")]
          (cond
            (not (and (string? p) (.isFile (io/file p))))
            (send-json! exchange 400 {:error "无效的角色档案路径"})

            :else
            (try
              (send-json! exchange 200 (character-inventory-payload p lang))
              (catch Throwable e
                (send-json! exchange 500 {:error (str e)})))))

        (and (= path "/api/character/inventory/page") query)
        (let [p (parse-query-param query "path")
              lang (or (parse-query-param query "lang") "zh")
              sack (parse-int-param query "sack" nil)]
          (cond
            (not (and (string? p) (.isFile (io/file p))))
            (send-json! exchange 400 {:error "无效的角色档案路径"})

            (nil? sack)
            (send-json! exchange 400 {:error "缺少合法的 sack 参数"})

            :else
            (try
              (if-let [data (character-inv-page-payload p sack lang)]
                (send-json! exchange 200 data)
                (send-json! exchange 404 {:error (str "背包袋不存在: " sack)}))
              (catch Throwable e
                (send-json! exchange 500 {:error (str e)})))))

        (and (= path "/api/character/inventory/item") (= method "POST"))
        (handle-inventory-item-post exchange)

        (and (= path "/api/character/inventory/item/delete") (= method "POST"))
        (handle-inventory-item-delete exchange)

        (and (= path "/api/character/inventory/item/affix") (= method "POST"))
        (handle-inventory-item-affix exchange)

        (and (= path "/api/character/inventory/item") query)
        (let [p (parse-query-param query "path")
              lang (or (parse-query-param query "lang") "zh")
              source (or (parse-query-param query "source") "sack")
              sack (parse-int-param query "sack" nil)
              set (parse-int-param query "set" nil)
              slot (parse-int-param query "slot" nil)]
          (cond
            (not (and (string? p) (.isFile (io/file p))))
            (send-json! exchange 400 {:error "无效的角色档案路径"})

            (not (contains? #{"sack" "equip" "weapon"} source))
            (send-json! exchange 400 {:error "未知的 source（应为 sack/equip/weapon）"})

            (= source "weapon")
            (do
              (if (and (some? set) (some? slot))
                (if-let [data (character-inv-item-payload p source sack set slot lang)]
                  (send-json! exchange 200 data)
                  (send-json! exchange 404 {:error "物品不存在"}))
                (send-json! exchange 400 {:error "缺少 set/slot 参数"})))

            (nil? slot)
            (send-json! exchange 400 {:error "缺少 slot 参数"})

            :else
            (if-let [data (character-inv-item-payload p source sack nil slot lang)]
              (send-json! exchange 200 data)
              (send-json! exchange 404 {:error "物品不存在"}))))

        (and (= path "/api/stash")
             (or (nil? query) (not (str/includes? query "index"))))
        (let [mode (or (parse-query-param query "mode") "standard")]
          (if (.isFile (io/file (stash-file-path mode)))
            (send-json! exchange 200 (stash-payload mode))
            (send-json! exchange 404 {:error (str "未找到共享仓库文件: " (stash-file-path mode))})))

        (and (= path "/api/stash/page") query)
        (let [mode (or (parse-query-param query "mode") "standard")
              index (parse-int-param query "index" nil)
              lang (or (parse-query-param query "lang") "zh")]
          (cond
            (nil? index)
            (send-json! exchange 400 {:error "缺少合法的 index 参数"})

            (not (.isFile (io/file (stash-file-path mode))))
            (send-json! exchange 404 {:error (str "未找到共享仓库文件: " (stash-file-path mode))})

            :else
            (if-let [data (stash-page-payload mode index lang)]
              (send-json! exchange 200 data)
              (send-json! exchange 404 {:error (str "页不存在: " index)}))))

        (and (= path "/api/stash/extra") query)
        (let [mode (or (parse-query-param query "mode") "standard")
              kind (parse-query-param query "kind")
              lang (or (parse-query-param query "lang") "zh")]
          (cond
            (not (some #(= % kind) catalog-kinds))
            (send-json! exchange 400 {:error "未知的目录页 kind"})

            :else
            (if-let [data (catalog-page-payload kind mode lang)]
              (send-json! exchange 200 data)
              (send-json! exchange 404 {:error (str "未找到 " kind " 仓库文件或解析失败")}))))

        (and (= path "/api/stash/item") query)
        (let [mode (or (parse-query-param query "mode") "standard")
              lang (or (parse-query-param query "lang") "zh")
              _ (ensure-gdx3-loaded!)
              table (lang-table lang)
              page (parse-int-param query "page" nil)
              slot (parse-int-param query "slot" nil)
              kind (parse-query-param query "kind")
              path (parse-query-param query "path")]
          (cond
            (not (.isFile (io/file (stash-file-path mode))))
            (send-json! exchange 404 {:error (str "未找到共享仓库文件: " (stash-file-path mode))})

            (and kind path)
            (if (some #(= % kind) catalog-kinds)
              (send-json! exchange 200 (item-detail-payload {:basename path} table))
              (send-json! exchange 400 {:error "未知的目录页 kind"}))

            (and (some? page) (some? slot))
            (let [s (st/load-stash-file (stash-file-path mode))
                  sack (nth (:stash s) page nil)]
              (if-let [data (and sack (stash-item-detail-payload sack slot table))]
                (send-json! exchange 200 data)
                (send-json! exchange 404 {:error (str "物品不存在: page=" page " slot=" slot)})))

            :else
            (send-json! exchange 400 {:error "缺少 page/slot 或 kind/path 参数"})))

        :else
        (send-json! exchange 404 {:error "not found"})))
    (catch Throwable e
      (send-json! exchange 500 {:error (str e)}))
    (finally
      (.close exchange))))

(defn- open-browser
  []
  (future
    (try
      (if (u/running-windows?)
        (shell/sh "cmd" "/c" "start" base-url)
        (shell/sh "xdg-open" base-url))
      (catch Throwable _
        nil))))

(defn -main
  [& _args]
  (au/load-settings-file)
  (load-local-table!)
  (au/load-db-in-background @local-zh-merged)
  (try
    (let [server (HttpServer/create (InetSocketAddress. port) 0)]
      (.setExecutor server (java.util.concurrent.Executors/newCachedThreadPool))
      (.createContext server "/"
                      (reify HttpHandler
                        (handle [_ exchange] (handle-request exchange))))
      (.start server)
      (println (str "gd-edit Web 已启动: " base-url))
      (println (str "角色数据目录: " (dirs/get-save-dir-search-list)))
      (open-browser)
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. (fn [] (try (.stop server 0) (catch Throwable _ nil)))))
      @(promise))
    (catch java.net.BindException _
      (binding [*out* *err*]
        (println (str "端口 " port " 已被占用，请先关闭占用进程或修改端口。"))))
    (catch Throwable e
      (binding [*out* *err*]
        (println (str "启动失败: " e))))))
