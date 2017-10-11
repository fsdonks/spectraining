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

#_(map (fn [x] (s/conform
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


(s/def ::valid-port-2 (s/and ::valid-port (s/conformer val)))

(s/conform ::valid-port-2 22)   ;; 22
(s/conform ::valid-port-2 8000) ;; 8000


;;The logic contained in conform can be run in reverse to produce the
;;original value from a conformed value using s/unform.

;;Run unform on both ::port and ::port-2 - what happens?
(comment 
(s/unform ::valid-port (s/conform ::valid-port 22))
(s/unform ::valid-port-2  (s/conform ::valid-port-2 22))

(s/unform ::valid-port [::privileged 22])
;;=> 22

(s/unform ::valid-port-2 22)
;; IllegalStateException no unform fn for conformer
)



;; Define the following specs and check that they conform and gen:

;;     Collection of distinct strings
(s/def ::strings (s/coll-of string? :distinct true))
;;     Vector of no more than 5 booleans
(s/def ::5-bools (s/coll-of boolean? :max-count 5))
;;     Set of collections of ints
(s/def ::coll-ints (s/coll-of (s/and coll? (s/every  int?)) :kind set))
;;     Map of strings to ints
(s/def ::string-int (s/map-of string? int?))

;; Write a spec for a binary tree. Branch nodes are collections containing either 1 or 2 children. Leaves are ints.
;; Conform a sample tree like:

;; [[1 2]
;;  [3 [4 5]]]

;;[l r] --> 2 two children
;; [x] --> 1 child
;; x --> integer
;; l,r --> either [x], [l r]

(def sd [[1 2]
         [3 [4 5]]])
(s/def ::leaf int?) 
(s/def ::tree
  (s/or :leaf ::leaf
        :branch (s/coll-of ::tree :max-count 2)))  

;; Write a spec for a recipe ingredient consisting of the ingredient
;; name, a quantity, and unit (a keyword). The following ingredients
;; should conform:


(def water #:ingredient{:name "water" :quantity 10 :unit :ounce})
(def butter #:ingredient{:name "butter" :quantity 1/2 :unit :tablespoon})

(s/def ::recipe (s/map-of keyword? (s/or :name string? :quantity number? :unit keyword?)))
;;approved solution!
(comment
(create-ns 'common)
(alias 'c 'common)
(s/def ::c/pos-num (s/and number? pos?))

(create-ns 'ingredient)
(alias 'i 'ingredient)
(s/def ::i/name string?)
(s/def ::i/quantity :common/pos-num)
(s/def ::i/unit keyword?)
(s/def ::i/ingredient (s/keys :req [::i/name ::i/quantity ::i/unit]))

(s/conform ::i/ingredient water)
(s/conform ::i/ingredient butter)
)

;; Write a spec for a recipe consisting of:

;;     name
;;     description
;;     ingredients - coll of ingredients
;;     steps - coll of strings
;;     servings - number of servings

;; Conform the following recipe to verify it works: 
(def toast
  #:recipe{
    :name "Buttered toast"
    :description "Like bread, but more tasty"
    :ingredients [
      #:ingredient{:name "bread" :quantity 2 :unit :slice}
      #:ingredient{:name "butter" :quantity 1 :unit :teaspoon}]
    :steps ["Toast two slice of bread in the toaster."
            "Spread butter on toast."]
    :servings 1})


;; Extend the event multi-spec in the slides to add another case for
;; events that look like this:
(def event-data
  {:event/type :event/quote
   :event/timestamp 1463970123001
   :quote/ticker "AAPL"})
;;setup
(s/def :event/type keyword?)  ;; used to indicate event type
(s/def :event/timestamp int?)

(defmulti event-type :event/type)
(s/def :event/event (s/multi-spec event-type :event/type))

;; "Hybrid" maps are mostly k-v containers, but also contain information
;; header properties. We can write specs for hybrid maps by creating a
;; merge of both map types.

;; For example, consider a map of names to scores that also contains some
;; annotations about the game rules in use:
(def names
  {"Amy" 200
   "Bill" 100
   :game/rule-set :cthulhu
   :game/dice-count 5})

;; This kind of data structure cannot be easily spec'ed with either
;; `s/map-of` or `s/keys` and instead is a combination of both.

;; First, write a spec `:game/opts` just for the game options.




;; Next, we want to create a spec that views the map entries as key-value
;; tuples. There are two kinds of tuples - "normal" map entries of string
;; keys / integer values and option entries that are keyword keys and the
;; option value. It's enough for us here to lump all option values
;; together under an `any?` predicate for now.

;; Write a spec `:game/tuple-kv` that matches a tuple like `["Amy"
;; 200]`. Write a spec `:game/tuple-option` that matches a tuple like
;; `[:game/rule-set :cthulhu]`.



;; Now that we have a way to describe tuples, and a way to describe an
;; options map, we can put them all together to spec the overall hybrid
;; map.

;; A map can be described as a collection of map entry tuples. For
;; example, this is an alternate way to describe a map with string keys
;; and int values:

#_(s/coll-of (s/tuple string? int?) :kind map? :into {})

;; To put everything together, first create a spec `:game/entries` for a
;; map as a collection of either `:game/tuple-kv` or
;; `:game/tuple-option`. Then create a spec that merges the collection
;; spec with the `:game/opts` spec into a single `:game/scores` spec.




;;Write a regex spec for any number of pairs of strings and numbers.

;;Example: 
(def numpairs ["a" 5 "b" 0.2])
(s/def ::numpair (s/cat :string string?  
                     :number number?))
(s/def ::numpairs (s/* ::numpair))



;;Write a regex spec for a line defined by two x y coordinates (ints).
;;Example: [0 0, 5 5]

(def line [0 0 5 5])
(s/def ::coord (s/cat :x int? :y int?))
(s/def ::line  (s/cat :one ::coord :two ::coord))
;;another way to parse lines...
(s/def ::line  (s/& (s/* ::coord) #(= (count %) 2)))

;; Write a regex spec for a polygon made of any number of x y
;; coordinates. Add a constraint to require at least 3 points.

;Example: [0 0, 5 5, 10 10]
(def poly [0 0, 5 5, 10 10])
(s/def ::polygon (s/& (s/+ ::coord) #(>= (count %) 3)))



;; The range function takes several arity versions: ([] [end] [start end]
;; [start end step]
(defn *n [n s]
  (s/& (s/* s) #(= (count %) n)))

(s/def ::range
  (s/alt ::arity-0 (*n 0 number?) 
         ::arity-1 (*n 1 number?)
         ::arity-2 (*n 2 number?)
         ::arity-3 (*n 3 number?))) 
;; Write a spec that accepts all of these choices as if they an
;; apply-able list of arguments. range accepts any kind of number for its
;; arguments.
;;
;;Tests:

(s/conform ::range [])
(s/conform ::range [10])
(s/conform ::range [5 10])
(s/conform ::range [0 5/2 1/2])

;; Sometimes it's useful to just create a generator that hard-codes a known value. Given the spec (s/def ::i int?), exercise ::i but override the generator to always return 42.

(s/def ::i int?)
;;failed...
#_(s/def ::myi
  (gen/fmap (fn [_] 42)
      (s/gen ::i)))

(s/exercise ::i 10 {::i #(gen/return 42)})

;; Extend the last example and exercise ::i with a custom generator that
;; returns 1, 2, or 3. Remember that you can produce a generator from any
;; spec! This is often the easiest way to make a generator (easier than
;; constructing one yourself).

(s/def ::onetwothree
  (s/with-gen int?
    #(s/gen #{1 2 3})))

;;works! why?!?
(s/exercise ::i 10
   {::i #(s/gen (s/with-gen int? (fn [] (s/gen #{1 2 3}))))})

;; Given a spec for keywords in the xyz namespace:

(s/def ::kwid (s/and qualified-keyword? #(= (namespace %) "xyz")))

;; Write a custom generator that creates keyword with the "xyz"
;; namespace and alphanumeric names.

;; Use gen/fmap to implement the generator, based on
;; gen/string-alphanumeric, then supply the generator to the spec with
;; s/with-gen. Remember that with-gen takes a no-arg function that
;; returns the generator!




;; Let's consider a keyword spec that's even more restricted - namespace
;; starts with foo and name starts with bar:

(require '[clojure.string :as str])
(s/def ::kwid2 
  (s/and qualified-keyword?
         #(str/starts-with? (namespace %) "foo")
         #(str/starts-with? (name %) "bar")))

(s/valid? ::kwid2 :foo5/bar10)

;; In this case we want to start with two random suffix strings and
;; generate a valid keyword. One useful pattern is to use s/tuple to
;; generate your random "parts", then construct the final result with
;; s/fmap. Try it!

