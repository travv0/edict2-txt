(ns edict2-txt.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn read-edict-rows
  "Read edict file into vector."
  [path & {:keys [encoding]
           :or {encoding "EUC-JP"}}]
  (->> (str/replace (slurp path :encoding encoding)
                    "(P)"
                    "")
       str/split-lines
       (drop 1)))

(defn parse-row
  "Parse edict-row into dictionary."
  [row]
  (let [split-row (str/split row #"\/")
        split-row (take (dec (count split-row)) split-row)
        exprs-readings (first split-row)
        exprs (first (str/split exprs-readings #" \["))
        readings (second (re-find #"\[(.*?)\]" exprs-readings))
        split-exprs (str/split exprs #";")
        split-readings (if (and readings (re-find #";" readings))
                         (str/split readings #";")
                         readings)
        defs (-> (str/join "; " (drop 1 split-row))
                 (str/replace #"[\(\{][^;]*?[\)\}] +" "")
                 (str/replace #"; $" ""))]
    {:expressions (map str/trim split-exprs)
     :readings (map str/trim split-readings)
     :definitions (str/trim defs)}))

(defn edict-rows-to-dictionary
  "Convert edict-rows to dictionary of data."
  [edict-rows]
  (map parse-row edict-rows))

(defn kindle-mate-from-parsed-row
  "Make Kindle Mate-compatible entries from parsed row."
  [row]
  (str/join "\n" (map #(format "%s\t%s" % (:definitions row)) (:expressions row))))

(defn kindle-mate-from-parsed-rows
  "Make Kindle Mate-compatible file from parsed rows."
  [rows]
  (str/join "\n" (map kindle-mate-from-parsed-row rows)))

(defn -main
  [& args]
  (let [args (into {} (map vec (partition 2 args)))
        input-path (get args "-i" "edict2")
        output-path (get args "-o" "kindlemate_edict.txt")]
    (printf "input-path: %s\noutput-path: %s\n" input-path output-path)
    (flush)
    (spit output-path
          (-> (read-edict-rows input-path)
              edict-rows-to-dictionary
              kindle-mate-from-parsed-rows))
    (println "Completed successfully")))
