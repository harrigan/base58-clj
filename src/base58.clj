(ns base58)

(set! *warn-on-reflection* true)

(set! *unchecked-math* true)

(def ^:const ^:private alphabet
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(def ^:const ^:private reverse-alphabet
  (into {} (map-indexed #(vector %2 %1) alphabet)))

(defn reversed-indices->str
  "Converts a byte array where each byte represents an index into the
  Base58 alphabet, to a Base58 string by processing the array from
  right to left. Leading zero indices on the right side of the array
  are skipped. Only the first `sz` symbols on the left side of the
  array are processed."
  [ba sz]
  (with-out-str
    (loop [i (dec sz)]
      (when-not (neg? i)
        (if (zero? (aget ^bytes ba i))
          (recur (dec i))
          (loop [j i]
            (when-not (neg? j)
              (print (.charAt ^String alphabet
                              (bit-and (aget ^bytes ba j) 0xff)))
              (recur (dec j)))))))))

(defn estimate-str-size
  "Estimates the size of a Base58 string required to store a byte
  array of size `sz`. The estimate is an upper bound."
  [sz]
  (inc (quot (* sz 137) 100)))

(defn- byte-array->str*
  [ba sz zs]
  (let [t  (estimate-str-size (- sz zs))
        bb (byte-array t)]
    (loop [i zs l 0]
      (if (< i sz)
        (recur (inc i)
               (long (loop [k 0 c (bit-and (aget ^bytes ba i) 0xff)]
                       (if (and (or (pos? c) (< k l)) (< k t))
                         (let [d (bit-and (aget ^bytes bb k) 0xff)
                               n (+ c (* 256 d))]
                           (aset bb k (byte (rem n 58)))
                           (recur (inc k) (quot n 58)))
                         k))))
        (reversed-indices->str bb l)))))

(defn byte-array->str
  "Converts a byte array to a Base58 string. Leading zeros are
  preserved."
  [ba]
  (with-out-str
    (let [s (alength ^bytes ba)]
      (loop [i 0]
        (when (< i s)
          (if (zero? (aget ^bytes ba i))
            (do
              (print (.charAt ^String alphabet 0))
              (recur (inc i)))
            (print (byte-array->str* ba s i))))))))

(defn reversed-indices->byte-array
  "Converts a byte array where each byte represents an index into the
  Base58 alphabet, to a byte array by processing the array from right
  to left. Leading zero indices on the right side of the array are
  skipped. Only the first `sz` symbols on the left side of the array
  are processed. `zs` zeros are prepended to the output."
  [ba sz zs]
  (loop [i (dec sz)]
    (if (neg? i)
      (byte-array zs)
      (if (zero? (aget ^bytes ba i))
        (recur (dec i))
        (let [bb (byte-array (+ (inc i) zs))]
          (loop [j i]
            (if (neg? j)
              bb
              (do
                (aset bb (+ (- i j) zs) (aget ^bytes ba j))
                (recur (dec j))))))))))

(defn estimate-byte-array-size
  "Estimates the size of a byte array required to store a Base58
  string of size `sz`. The estimate is an upper bound."
  [sz]
  (inc (quot (* sz 74) 100)))

(defn- str->byte-array*
  [s sz zs]
  (let [t  (estimate-byte-array-size sz)
        ba (byte-array t)]
    (loop [j zs l 0]
      (if (< j sz)
        (recur (inc j)
               (long (loop [k 0 c (reverse-alphabet
                                   (.charAt ^String s j))]
                       (if (and (or (pos? c) (< k l)) (< k t))
                         (let [d (bit-and (aget ^bytes ba k) 0xff)
                               n (+ c (* 58 d))]
                           (aset ba k (byte (rem n 256)))
                           (recur (inc k) (quot n 256)))
                         k))))
        (reversed-indices->byte-array ba l zs)))))

(defn str->byte-array
  "Converts a Base58 string to a byte array. Leading zeros are
  preserved."
  [s]
  (let [t (count s)]
    (loop [i 0]
      (if (and (< i t) (= (.charAt ^String alphabet 0)
                          (.charAt ^String s i)))
        (recur (inc i))
        (str->byte-array* s t i)))))
