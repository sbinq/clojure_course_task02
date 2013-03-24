(ns clojure-course-task02.core
  (:import [java.io File]
           [java.util.concurrent Executors])
  (:gen-class))

(set! *warn-on-reflection* true)
(def ^:dynamic *thread-pool-size* 6)

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

(defn find-files-mt [file-filter root]
  "Uses multiple workers in different threads synchronizing state with refs.
   Main thread uses java-style wait/notify to check if everything is finished
   which seems to be ok - https://groups.google.com/forum/?fromgroups=#!topic/clojure/foqaJ4DQoWM "
  ;; do not want to use pmap here since this would lead to many threads created at the same time;
  ;; plain clojure future does not allow to restrict threads count - so using java executors explicitly
  (let [pool (Executors/newFixedThreadPool *thread-pool-size*)
        matching-files-ref (ref [])
        submitted-workers-count-ref (ref 1) ; for initial worker handling root dir
        worker-finished-signal (Object.)
        worker (fn worker [^File parent]
                 (let [[dirs files] (dirs-and-files parent)
                       matching-files (filter file-filter files)]
                   (dosync
                    ;; don't actually need refs here - two independents atoms would do fine
                    ;; (latest two invocations would be united into one)
                    (alter matching-files-ref conj matching-files) ;TODO: if use concat here - getting StackOverflow on large input
                    (alter submitted-workers-count-ref + (count dirs))
                    (alter submitted-workers-count-ref dec)) ; for current worker
                   (doseq [dir dirs]
                     (.execute pool #(worker dir)))
                   (locking worker-finished-signal
                     (.notify worker-finished-signal))))]
    (locking worker-finished-signal
      (.execute pool #(worker root))
      (loop [submitted-workers-count @submitted-workers-count-ref]
        (if (> submitted-workers-count 0)
          (do
            (.wait worker-finished-signal)
            (recur @submitted-workers-count-ref))
          (do
            (.shutdown pool)
            (flatten @matching-files-ref))))))) ;TODO: flatten here because of concat workaround above

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
      (dorun (map println (find-files file-name path))))))
