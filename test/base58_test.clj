(ns base58-test
  (:require [base58 :as base58]
            [clojure.test :refer [deftest are]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as generators]
            [clojure.test.check.properties :refer [for-all]])
  (:import [java.util Arrays]))

(set! *warn-on-reflection* true)

(defspec byte-array->base58-str-roundtrip-spec
  (for-all [ba generators/bytes]
    (Arrays/equals ^bytes ba
                   ^bytes (base58/base58-str->byte-array
                           (base58/byte-array->base58-str ba)))))
