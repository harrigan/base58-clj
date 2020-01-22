(ns base58
  (:require [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def ^:const ^:private base58-alphabet
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(defn byte-array->base58-str
  [ba]
  (let [s  (alength ^bytes ba)
        sb (StringBuilder.)
        t  (int (Math/ceil (* s (/ (Math/log 256) (Math/log 58)))))
        bb (byte-array t)]
    (loop [i 0]
      (when (< i s)
        (if (zero? (bit-and (aget ^bytes ba i) 0xff))
          (do
            (.append sb (.charAt ^String base58-alphabet 0))
            (recur (inc i)))
          (loop [j i]
            (when (< j s)
              (loop [k 0 c (bit-and (aget ^bytes ba j) 0xff)]
                (when (or (< k t) (pos? c))
                  (let [d (bit-and (aget ^bytes bb k) 0xff)
                        n (+ c (* 256 d))]
                    (aset bb k (unchecked-byte (mod n 58)))
                    (recur (inc k) (quot n 58)))))
              (recur (inc j)))))))
    (loop [i (dec t)]
      (when (>= i 0)
        (if (zero? (aget ^bytes bb i))
          (recur (dec i))
          (loop [j i]
            (when (>= j 0)
              (.append sb (.charAt ^String base58-alphabet
                                   (bit-and (aget ^bytes bb j) 0xff)))
              (recur (dec j)))))))
    (.toString sb)))

(defn base58-str->byte-array
  [b58s]
  (let [s  (count b58s)
        t  (int (Math/ceil (* s (/ (Math/log 58) (Math/log 256)))))
        ba (byte-array t)
        z  (loop [i 0]
             (if (< i s)
               (if (zero? (string/index-of base58-alphabet
                                           (.charAt ^String b58s i)))
                 (recur (inc i))
                 (do
                   (loop [j i]
                     (when (< j s)
                       (loop [k 0 c (string/index-of
                                     base58-alphabet
                                     (.charAt ^String b58s j))]
                         (when (or (< k t) (pos? c))
                           (let [d (bit-and (aget ^bytes ba k) 0xff)
                                 n (+ c (* 58 d))]
                             (aset ba k (unchecked-byte (mod n 256)))
                             (recur (inc k) (quot n 256)))))
                       (recur (inc j))))
                   i))
               i))]
    (loop [i (dec t)]
      (if (>= i 0)
        (if (zero? (aget ^bytes ba i))
          (recur (dec i))
          (let [bb (byte-array (+ (inc i) z))]
            (loop [j i]
              (when (>= j 0)
                (aset bb (+ (- i j) z) (aget ^bytes ba j))
                (recur (dec j))))
            bb))
        (byte-array z)))))
