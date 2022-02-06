(ns marmatus.core
  (:require [marmatus.impl :as impl]))

(defmacro mformat [fmt & args]
  (when-not (string? fmt)
    (throw (RuntimeException. (str (pr-str fmt) " is not a string literal"))))

  (let [argnames (mapv (fn [_] (gensym)) args)]
    `(let [~@(interleave argnames args)]
       (str ~@(impl/parse-fmt (impl/parser fmt argnames))))))

