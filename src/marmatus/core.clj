(ns marmatus.core
  (:import [java.util UnknownFormatConversionException MissingFormatArgumentException]))

(defprotocol Parser
  (parser-index [self])
  (parser-slice [self start end])
  (parser-args [self])

  (parser-peek [self])
  (parser-next! [self]))

(deftype FmtParser [string ^:unsynchronized-mutable index args]
  Parser
  (parser-index [_] index)
  (parser-slice [_ start end] (subs string start end))
  (parser-args [_] args)

  (parser-peek [_] (get string index))
  (parser-next! [_] (set! index (inc index))))

(defn- parser [s args] (FmtParser. s 0 args))

;; text ::= #"[^%]*"
(defn- parse-text [parser]
  (let [start (parser-index parser)]
    (loop [end start]
      (case (parser-peek parser)
        (\% nil) (parser-slice parser start end)

        (do
          (parser-next! parser)
          (recur (inc end)))))))

;; specifier ::= "%s"
(defn- parse-specifier [parser index]
  (assert (= (parser-peek parser) \%))
  (parser-next! parser)

  (let [args (parser-args parser)
        c (parser-peek parser)]
    (case c
      \s (do
           (parser-next! parser)

           (if (< index (count args))
             (let [arg (get args index)]
               [`(if (nil? ~arg) "null" (.toString ~arg))])
             (throw (MissingFormatArgumentException. "%s"))))

      nil (throw (UnknownFormatConversionException. \%))

      (throw (UnknownFormatConversionException. c)))))

;; fmt ::= text (specifier text)*
(defn- parse-fmt [parser]
  (loop [specifier-index 0
         forms [(parse-text parser)]]
    (case (parser-peek parser)
      \% (let [specifier-forms (parse-specifier parser specifier-index)
               text-form (parse-text parser)]
           (recur (inc specifier-index)
                  (-> forms
                      (into specifier-forms)
                      (conj text-form))))

      nil forms)))

(defmacro mformat [fmt & args]
  (when-not (string? fmt)
    (throw (RuntimeException. (str (pr-str fmt) " is not a string literal"))))

  (let [argnames (mapv (fn [_] (gensym)) args)]
    `(let [~@(interleave argnames args)]
       (str ~@(parse-fmt (parser fmt argnames))))))

