(ns gd-edit.game-dirs-test
  (:require [midje.sweet :refer :all]
            [clojure.java.io :as io]
            [gd-edit.game-dirs :as dirs]))

(def ^:private vdf-content
  (str "\"libraryfolders\"\n"
       "{\n"
       "  \"0\" {\n"
       "    \"path\" \"/mnt/games/SteamLibrary\"\n"
       "  }\n"
       "}\n"))

(fact "steam-library-folders-under parses a libraryfolders.vdf into its paths"
  (let [root (io/file (System/getProperty "java.io.tmpdir") (str "gdsteam-" (System/nanoTime)))
        vdf (io/file root "steamapps/libraryfolders.vdf")]
    (io/make-parents vdf)
    (spit vdf vdf-content)
    (let [result (vec (#'gd-edit.game-dirs/steam-library-folders-under root))]
      (result => ["/mnt/games/SteamLibrary"]))))

(fact "steam-library-folders-under returns nil for a root lacking the vdf file"
  (let [root (io/file (System/getProperty "java.io.tmpdir") (str "gdnone-" (System/nanoTime)))]
    (io/make-parents (io/file root "x"))
    (#'gd-edit.game-dirs/steam-library-folders-under root) => nil))

(fact "steam app id constant is Grim Dawn's"
  (#'gd-edit.game-dirs/steam-app-id) => "219990")

(fact "get-file-override-dirs includes the gdx3 dir entry"
  (->> (dirs/get-file-override-dirs)
       (map str)
       (some #(re-find #"gdx3$" %))
       (some?)) => true)

(fact "canonical-path collapses redundant path segments"
  (let [tmp (System/getProperty "java.io.tmpdir")
        dup (str (io/file tmp ".." "tmp"))]
    (#'gd-edit.game-dirs/canonical-path dup) => (str (io/file tmp))))

(fact "canonical-path survives missing input"
  (#'gd-edit.game-dirs/canonical-path nil) => nil)
