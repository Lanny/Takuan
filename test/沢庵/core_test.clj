(ns 沢庵.core-test
  (:require [clojure.test :refer :all]
            [沢庵.これ :refer :all]))

(deftest test-readline
  (let [[l1 r1] (readline (seq "this is\na test of\nthe readline fn."))
        [l2 r2] (readline r1)
        [l3 r3] (readline r2) ]
    (is (= l1 "this is"))
    (is (= r1 (seq "a test of\nthe readline fn.")))
    (is (= l2 "a test of"))
    (is (= r2 (seq "the readline fn.")))
    (is (= l3 "the readline fn."))
    (is (empty? r3))))

(deftest test-primatives
  (let [pint (pickle->data (seq "I42\n."))
        ptrue (pickle->data (seq "I01\n."))
        pfalse (pickle->data (seq "I00\n."))
        pstring1 (pickle->data (seq "S'Oh hai'\np0\n."))
        pstring2 (pickle->data (seq "S'Oh hai\\nthere'\np0\n."))
        pnone (pickle->data (seq "N."))
        pfloat (pickle->data (seq "F4.25\n."))]
    (is (= pint 42))
    (is (= ptrue true))
    (is (= pfalse false))
    (is (= pstring1 "Oh hai"))
    (is (= pstring2 "Oh hai\nthere"))
    (is (= pnone :None))
    (is (= pfloat 4.25))))

(deftest test-load-dict
  (let [[_ [result & stack] __] (load-dict '() '(2 "hi" "." "you" :mark) {})]
    (is (contains? result "hi"))
    (is (contains? result "you"))
    (is (empty? stack))
    (is (= result {"hi" 2 "you" "."}))))


(deftest test-lists
  (let [pickle "(lp0\nI1\naF4.25\naS'abc'\np1\na."
        result (pickle->data pickle)]
    (is (= result [1 4.25 "abc"]))))

(deftest test-dictionaries
  (let [pickle "(dp0\nS'you'\np1\nF4.25\nsS'hi'\np2\nI2\ns."
        result (pickle->data pickle)]
    (is (= result {"hi" 2 "you" 4.25}))))

(def simple-object-pickle (seq (str "ccopy_reg\n_reconstructor\np0\n(c__main__"
                                    "\nTestClass\np1\nc__builtin__\nobject\np2"
                                    "\nNtp3\nRp4\n(dp5\nS'string_val'\np6\nS'o"
                                    "h hai there'\np7\nsS'ivo'\np8\nI42\nsS'fl"
                                    "oat_val'\np9\nF3.5\nsb.")))

(deftest test-obj-pickle
  (let [result (pickle->data simple-object-pickle)]
    (is (= result {:__module__ "__main__"
                   :__name__ "TestClass"
                   :ivo 42
                   :string_val "oh hai there"
                   :float_val 3.5}))))

(deftest test-tweepy-obj
  (let [tweepyckle (seq (slurp "test/沢庵/tweepy.pickle"))
        tweepy-obj (pickle->data tweepyckle)]
    (is tweepy-obj)))

