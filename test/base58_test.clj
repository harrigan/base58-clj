(ns base58-test
  (:require [base58 :as base58]
            [clojure.test :refer [deftest are]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as generators]
            [clojure.test.check.properties :refer [for-all]])
  (:import [java.util Arrays]))

(set! *warn-on-reflection* true)

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

(defspec estimate-byte-array-size-spec
  (for-all [ba generators/bytes]
    (let [b58s (base58/byte-array->str ba)
          s (count b58s)]
      (<= (alength ^bytes (base58/str->byte-array b58s))
          (int (Math/ceil (* s (/ (Math/log 58) (Math/log 256)))))
          (base58/estimate-byte-array-size s)))))
