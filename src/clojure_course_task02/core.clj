(ns clojure-course-task02.core
  (:require [clojure.core.reducers :as r])
  (:import [java.io File])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn dirs-and-files [parent]
  "List of two vectors - directories and files in given directory
   (any of them can be nil if there are no files/directories inside)"
  (let [children (.listFiles ^File parent)
        {dirs true, files false} (group-by #(.isDirectory ^File %) children)]
    (list dirs files)))

(defn make-regex-file-filter [regex-str]
  "Returns function which takes file as argument and returns boolean
   indicating whether it matches or not"
  (let [pattern (re-pattern regex-str)]
    (fn [file]
      (not (nil? (re-matches pattern (.getName ^File file)))))))

(def thread-names (atom #{}))       ; keeping track of threads where solution was run just for info
(defn find-files-r [file-filter root]
  "Find files matching given filter fn starting from given root directory.
   Uses clojure reducers (and java fork/join framework behind the scenes)
   seems like achieving pretty good parallelism - http://i.imgur.com/9DvPphM.png "
  (let [[dirs files] (dirs-and-files root)
        matching (doall (filter file-filter files))
        child-matching (r/fold 1 r/cat
                               (fn [acc dir] (r/cat acc (find-files-r file-filter dir)))
                               dirs)]
    (swap! thread-names conj (.getName (Thread/currentThread))) ; see comment above
    (r/cat matching child-matching)))

(defn find-files [file-name path]
  "Implements searching for a file using his name as a regexp."
  (let [file-filter (make-regex-file-filter file-name)
        files (doall (seq (find-files-r file-filter (File. ^String path))))]
    (map #(.getName ^File %) files)))

(defn usage []
  (println "Usage: $ run.sh file_name path"))

(defn -main [file-name path]
  (if (or (nil? file-name)
          (nil? path))
    (usage)
    (do
      (println "Searching for " file-name " in " path "...")
      (dorun (map println (find-files file-name path)))
      (println "Reducers-based function was executing in" @thread-names))))
