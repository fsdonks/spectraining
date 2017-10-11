(ns spectraining.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


;;Given a spec, check to see if it's valid....
(s/valid? int? 10)        ;; true
(s/valid? int? nil)       ;; false
(s/valid? string? "abc")  ;; true
(s/valid? #{1 2 3} 3)     ;; true

;;tell how value conforms with conform...
(s/conform int? 10)  ;; 10

;;exercise specs, i.e. generative testing with random stuff.
(s/exercise int?)
(s/exercise string?)

(s/valid? #{1 2 3} 3)
(s/conform #{1 2 3} 3)
(s/exercise #{1 2 3})



;;Validate that "abc" is a valid string using spec.
(s/valid? string? "abc")



;;Write a spec that describes the number of pins knocked down in a bowling roll (0-10). Generate some sample rolls.
(def rolls (s/int-in 0 10))
(s/exercise rolls)
;;([0 0] [0 0] [1 1] [2 2] [3 3] [0 0] [6 6] [0 0] [6 6] [9 9])



;; Write a spec to describe a bowling scoring mark (a character) and
;; generate some samples:

;; - for 0 1-9 for a roll
;; / for a spare
;; X for a strike

(def mark? #{\- \/ \X})
(s/exercise mark?)

;; Write a spec that validates java.util.Date instances and check that
;; (java.util.Date.) is valid.

(s/valid? inst? (java.util.Date.))

;; Write a spec that accepts any double value except the special values
;; NaN, Infinity, and -Infinity and generate some samples.
(s/exercise (s/double-in :infinite? false :NaN? false))

;; Write a spec that validates whether a string matches the regex
;; #"SKU-[0-9]+" using re-matches and test with some values.

;; Does it generate when you exercise it? ;;nope!
(defn matches? [x]
  (re-matches #"SKU-[0-9]+" x))

;;composite specs

;; Create a spec that accepts any unqualified symbol EXCEPT &.
;; Does it gen automatically?

(defn not-&? [x]
  (not (= x '&)))
(s/def ::not& not-&?)
;;do better?
(s/def ::not& (s/and symbol? not-&?))


;; Create specs for privileged ports (1-1024) and unprivileged ports
;; (1025-65536). Create a spec that combines these specs for any
;; port. Conform values in both ranges and check the result.

(defn between? [v a b]
  (and (>= v a) (<= v b)))

(s/valid? (s/or  :priv #(between? % 1 1024)  :unpriv #(between? % 1025 65536)) 4)

(map (fn [x] (s/conform
       (s/or :priv #(between? % 1 1024)
             :unpriv #(between? % 1025 65536)) x))
     (range 100))

;;better?
(s/def ::priv (s/int-in 1 1024))
(s/def ::unpriv (s/int-in 1025 65536))
(s/def ::valid-port (s/or :priv   ::priv
                          :unpriv ::unpriv))




;; Given the prior exercise, what if you wished to conform not to a
;; tagged value but to the original value?

;; spec provides conformers to manipulate the conformed value. s/conform
;; applies an arbitrary function to its input and returns either the
;; modified output or :s/invalid. Often conformers are combined with
;; other predicates inside an s/and.

;; NOTE! Be careful using conformers, particularly with registered specs
;; - you are making decisions for all consumers of your spec. Modifying
;; the conformed value throws away information that you may wish to have
;; at some future point.

;; Use s/conformer to modify ::port to return the original value. Keep in
;; mind that s/or returns a map entry that works with key and val.


;;TBD;;;


;;The logic contained in conform can be run in reverse to produce the
;;original value from a conformed value using s/unform.

;;Run unform on both ::port and ::port-2 - what happens? 

;;TBD;;;;



