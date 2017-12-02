;;A port of an awk script that munges
;;code to .org mode documentation,
;;let's do it with spec!
(ns spectraining.doc
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]))


;;Given a source file, let's break it down
;;by lines and parse the sequence of lines into
;;a structure of code blocks.
;;Our goal is to also recognize legacy
;;Mardown headers (via =====) and
;;to produce syntax-highlighted code
;;examples in org.

;;Specs for Code and Comments
;;===========================
;;We'll be leveraging regexes a lot, but
;;we don't "have" too...We could use the
;;token-based charseq parsing example, but
;;particularly if we wanted testable specs
;;or to generate random inputs from the specs,
;;but that's a bridge too far for now!

;;line beginning with zero or more whitespace
(def +comment+ #"s*;+")
(defn comment? [x]  (re-find +comment+  x))
(s/def ::commented-text  comment?)

(defn blank-line? [x]
  (or (= x "")
      (re-find #"^\s+$"  x)))

(s/def ::blank-line blank-line? )

;;anything between begin-line and \; is valid source
(def +non-comment+ #"^[^;]+")

;;Couldn't figure out a regex to ignore inside of
;;string literals when checking for the comment
;;token, so I had to hack this up.  Apologies!
(defn non-comment [^String x]
  (let [n (count x)]
    (loop [idx 0
           mode :outside]
      (if (== idx n)
        x
        (let [c (.charAt x idx) ]
          (if (= c \") ;skip string literals
            (case mode
              :outside (recur (unchecked-inc idx) :inside)
              (recur (unchecked-inc idx) :outside))
            (if (= c \;) ;;comment-break
              (if (and (= mode :outside)
                       (not= (.charAt x (dec idx)) \\ ))
                  (subs x 0 idx)
                  (recur (unchecked-inc idx) mode))
            (recur (unchecked-inc idx) mode))))))))

(defn non-comment? [x]
  (when-let [x  (re-find +non-comment+ x)]
    (not (blank-line? x))))

(s/def ::code
  (s/and non-comment?
         (s/conformer (fn [ln]
                        (let [body (non-comment ln)]
                        [:code 
                         {:open   (count (re-seq #"\("  body))
                          :closed (count (re-seq #"\)"  body))
                          :line ln}])))))
(s/def ::non-code
  (s/or :blank ::blank-line
        :comment ::commented-text))

(s/def ::source-block
   (s/or :code-line    ::code
         :non-code ::non-code))

(s/def ::source-code (s/* ::source-block))

;;Source Code Parsing Pipeline
;;============================
(defn source->tree [path]
  (->> (slurp path)
       (clojure.string/split-lines)
       (s/conform ::source-code)))

;;once we have our tagged tree, we can
;;process it to determine which lines belong to
;;code blocks, based on lexical "depth."

(defn tree->depth [xs]
  (let [d (atom 0)]
    (for [[cl [kind nd]] xs
          ]
      (case cl
        :code-line
        (let [{:keys [open closed]} nd
              delta (- open closed)
              dnew (swap! d + delta)]
          (merge {:kind :code-line :depth dnew} nd ))
        {:kind :non-code :depth @d :line nd}))))

;;Since we have depth now, should be able to group
;;our lines by depth, and classify them in the
;;process.
(defn depth-grouping [{:keys [depth kind] :as nd}]
  (cond (and (zero? depth) (= kind :non-code))
            :top-level-comment
        (> depth 0) :multi-line-form
        (and (zero? depth) (= (:open nd) (:closed nd)))
          :top-level-form
        :else  :multi-line-form ))

(defn tree->groupings [tr]
  (->> tr
       (tree->depth)
       (map-indexed (fn [idx nd] (assoc nd :idx idx :scope (depth-grouping nd))))
       (partition-by :scope)))

;;Org Specific Transformations
;;============================
;;Detect markdown headers.
(defn header [x]  (re-find #"^=+\s*$" x))
(defn uncomment [x]
  (clojure.string/replace-first x  #"\s*;*" ""))

(defn mdheader->org-header [xs]
  (reduce (fn [acc ln]
            (if-let [h (header ln)]
              (let [x (last acc)]
                (assoc acc (dec (count acc))
                       (str "* " x)))
              (conj acc ln))) [] xs))

;;Multimethods allow us to define flexible
;;processing for different classes of tagged
;;blocks we might find.  So, we could expand
;;this if we have more sophisticated parsing
;;options.
(defmulti group->org (fn [xs] (:scope (first xs))))
(defmethod group->org :top-level-comment [xs]
  (->> xs
       (map :line)
       (map uncomment)
       (mdheader->org-header)
       (clojure.string/join \newline )))

(defmethod group->org :top-level-form [xs]
  (str (str "#+BEGIN_SRC clojure") \newline
       (clojure.string/join \newline (map :line xs)) \newline
       (str "#+END_SRC")))

(defmethod group->org :multi-line-form [xs]
  (str (str "#+BEGIN_SRC clojure") \newline
       (clojure.string/join \newline (map :line xs)) \newline
       (str "#+END_SRC")))

;;Entry Point
;;===========
;;This is the main function.  Currently it just
;;parses the source from path and creates a
;;similarly name path.org file colocated with
;;the original source.
(defn orgify-source [path]
  (->> path
       (source->tree)
       (tree->groupings)
       (map group->org)
       (clojure.string/join \newline)
       (spit (str path ".org"))
       ))
