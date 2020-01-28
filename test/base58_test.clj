(ns base58-test
  (:require [base58 :as base58]
            [clojure.test :refer [deftest are]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as generators]
            [clojure.test.check.properties :refer [for-all]])
  (:import [java.util Arrays]))

(set! *warn-on-reflection* true)

(deftest reversed-indices->str-test
  (are [x y z]
      (= x (base58/reversed-indices->str (byte-array y) z))
    ""    []       0
    ""    [0]      1
    "2"   [1 0]    2
    "2z"  [57 1 0] 3
    "21z" [57 0 1] 3
    "2"   [1 0]    1
    ""    [0 1]    1))

(defspec byte-array->str-roundtrip-spec
  (for-all [ba generators/bytes]
    (Arrays/equals ^bytes ba
                   ^bytes (base58/str->byte-array
                           (base58/byte-array->str ba)))))

(defspec estimate-str-size-spec
  (for-all [ba generators/bytes]
    (let [s (alength ^bytes ba)]
      (<= (count (base58/byte-array->str ba))
          (int (Math/ceil (* s (/ (Math/log 256) (Math/log 58)))))
          (base58/estimate-str-size s)))))

(deftest reversed-indices->byte-array-test
  (are [w x y z]
      (let [ba (base58/reversed-indices->byte-array (byte-array x)
                                                    y z)]
        (Arrays/equals (byte-array w) ^bytes ba))
    []               []       0 0
    []               [0]      1 0
    [0x01]           [1 0]    2 0
    [0x01 0x39]      [57 1 0] 3 0
    [0x01 0x00 0x39] [57 0 1] 3 0
    [0x01]           [1 0]    1 0
    []               [0 1]    1 0
    [0x00 0x01]      [1]      1 1))

(defspec estimate-byte-array-size-spec
  (for-all [ba generators/bytes]
    (let [b58s (base58/byte-array->str ba)
          s    (count b58s)]
      (<= (alength ^bytes (base58/str->byte-array b58s))
          (int (Math/ceil (* s (/ (Math/log 58) (Math/log 256)))))
          (base58/estimate-byte-array-size s)))))
