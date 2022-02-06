(ns marmatus.runtime)

(defn format-string [^Object v] (if (nil? v) "null" (.toString v)))

