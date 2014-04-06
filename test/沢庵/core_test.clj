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
  (let [pint (load-seq (seq "I42\n."))
        ptrue (load-seq (seq "I01\n."))
        pfalse (load-seq (seq "I00\n."))
        pstring1 (load-seq (seq "S'Oh hai'\np0\n."))
        pstring2 (load-seq (seq "S'Oh hai\\nthere'\np0\n."))
        pnone (load-seq (seq "N."))
        pfloat (load-seq (seq "F4.25\n."))]
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

(deftest test-dictionaries
  (let [pickle "(dp0\nS'you'\np1\nF4.25\nsS'hi'\np2\nI2\ns."
        result (load-seq pickle)]
    (is (= result {"hi" 2 "you" 4.25}))))
