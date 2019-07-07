(ns ^{:doc "org-file to markdown file converter

org2mds.clj parses org-file's top level headings and their content to separate notes 
and writes them to separate files in current directory.
org-file's top level headings should be formatted like this:
'* some heading title <yyyy-MM-dd EEE kk:mm>'

usage: 
% lein-exec org2mds.clj <org-file> "}
 org2mds
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:import java.text.SimpleDateFormat))

(def re-title (re-pattern "^\\* (.*) <(\\d{4}?-\\d\\d-\\d\\d ... \\d\\d:\\d\\d)>$"))
(def datefmt (SimpleDateFormat. "yyyy-MM-dd EEE kk:mm"))

(defn add-note
  "add note to notes and return update notes"
  [notes line content]
  (let [[_ title date] (re-matches re-title line)]
    (conj notes {:title title
                 :date date
                 :content content})))

(defn parse-notes
  "parse org notes and return them as vector"
  ([fname]
   (parse-notes nil [] [] (str/split-lines (slurp fname))))
  ([title content notes [line & lines]]
   (cond
     (nil? line)
     (if (nil? title)
       notes
       (add-note notes title content))

     (re-matches re-title line)
     (if (nil? title)
       (recur line content notes lines)
       (recur line [] (add-note notes title content) lines))

     :else
     (recur title (conj content line) notes lines))))

(defn unique-fname
  "return unique filename for name by adding number to filename if needed: 'name-<number>.ext'"
  [name ext]
  (let [name (str/replace name "/" " ")]
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
    (spit fname (apply str (map #(str % "\n") (:content note))))
    (.setLastModified (io/file fname) time)))

(defn run []
  (try
    (if-let [org-file (second *command-line-args*)]
      (let [notes (parse-notes org-file)]
        (println "converting" (count notes) "notes found in" org-file ":")
        (doseq [note notes]
          (println "  " (:title note))
          (write-note note)))
      (println "usage: give org file as argument"))
    (catch Exception e (println "Error:" (.getMessage e)))))

(run)