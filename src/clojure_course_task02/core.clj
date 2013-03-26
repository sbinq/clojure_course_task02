(ns clojure-course-task02.core
  (:import [java.io File])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn dirs-and-files [^File parent]
  "List of two vectors - directories and files in given directory
   (any of them can be nil if there are no files/directories inside)"
  (let [children (.listFiles parent)
        {dirs true, files false} (group-by (fn [^File file] (.isDirectory file)) children)]
    (list dirs files)))

(defn make-regex-file-filter [regex-str]
  "Returns function which takes file as argument and returns boolean
   indicating whether it matches or not"
  (let [pattern (re-pattern regex-str)]
    (fn [^File file]
      (not (nil? (re-matches pattern (.getName file)))))))

(defn find-files-mt [file-filter ^File root]
  ;; actually using ref only to formally satisfy requirements - e.g. atom
  ;; would be fine here, absence of mutable state could be even better
  (let [acc (ref [])]
    ;; FIXME: this consumes very much memory on huge inputs - -Xmx6144M was not enough for my user home (61204 dirs, 209649 files)
    (dorun
     (pmap (fn [[dirs files]]
             (let [matching (doall (filter file-filter files))] ; doing filter outside of transaction to reduce overlapping
               (dosync
                (alter acc conj matching)))) ; concat fails with stackoverflow here on large inputs, so using conj+flatten
           ;; as with ref, actually using sequence representing each directory content
           ;; only to satisfy 'subdirectory parallelism' requirement - otherwise things could be simpler
           (tree-seq identity
                     (fn [[dirs files]] (map dirs-and-files dirs))
                     (dirs-and-files root))))
    (flatten @acc)))

(defn find-files [file-name ^String path]
  "Implements searching for a file using his name as a regexp."
  (let [file-filter (make-regex-file-filter file-name)
        root (File. path)
        files (find-files-mt file-filter root)]
    (map (fn [^File file] (.getName file)) files)))

(defn usage []
  (println "Usage: $ run.sh file_name path"))

(defn -main [file-name path]
  (if (or (nil? file-name)
          (nil? path))
    (usage)
    (do
      (println "Searching for " file-name " in " path "...")
      (dorun (map println (find-files file-name path)))
      (println "Finished with" (count (Thread/getAllStackTraces)) "threads")
      (shutdown-agents))))
