(ns ^{:doc "org-file to markdown file converter

org2mds.clj parses org-file's top level headings and their content to separate notes
and writes them to separate files in current directory.
org-file's top level headings should be formatted like this:
'* some heading title <yyyy-MM-dd EEE kk:mm>'

usage:
% lein-exec org2mds.clj <org-file> "}
 org2mds
  (:require [clojure.string :as s])
  (:require [clojure.java.io :as io])
  (:import java.text.SimpleDateFormat))

(def re-title (re-pattern "^(.+)<(.+)>"))
(def datefmt (SimpleDateFormat. "yyyy-MM-dd EEE kk:mm"))

(defn parse-notes
  "parse org notes and return them as seq"
  [fname]
  (let [data (clojure.core/str "\n" (s/triml (slurp fname)))
        notes (s/split data #"\n\* ")]
    (for [note (rest notes)
          :let [[_ title date] (re-find re-title note)]]
      {:title (s/trim title) :date date :content (s/replace-first note #"^.*\n?" "")})))

(defn unique-fname
  "return unique filename for name by adding number to filename if needed: 'name-<number>.ext'"
  [name ext]
  (let [name (s/replace name "/" " ")]
    (->>
     (conj (for [n (rest (range))] (str name "-" n "." ext))
           (str name "." ext))
     (remove #(.exists (io/as-file %)))
     first)))

(defn write-note
  "write note to file"
  [note]
  (let [time (.getTime (.parse datefmt (:date note)))
        fname (unique-fname (:title note) "md")]
    (spit fname (str "# " (:title note) "\n" (:date note) "\n\n" (:content note)))
    (.setLastModified (io/file fname) time)))

(defn run [fname]
  (try
    (if-let [org-file fname]
      (let [notes (parse-notes org-file)]
        (println "converting" (count notes) "notes found in" org-file ":")
        (doseq [note notes]
          (println "  " (:title note))
          (write-note note)))
      (println "usage: give org file as argument"))
    (catch Exception e (println "Error:" (.getMessage e)))))

(run (second *command-line-args*))

(comment
  (run "notes.org"))
