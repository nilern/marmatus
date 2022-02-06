(ns marmatus.parser
  (:require [marmatus.runtime :as rt])
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

(defn parser [s args] (FmtParser. s 0 args))

;;;;

;; text ::= #"[^%]*"
(defn- parse-text [parser]
  (let [start (parser-index parser)]
    (loop [end start]
      (case (parser-peek parser)
        (\% nil) (if (= start end)
                   []
                   [(parser-slice parser start end)])

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
               [`(rt/format-string ~arg)])
             (throw (MissingFormatArgumentException. "%s"))))

      nil (throw (UnknownFormatConversionException. \%))

      (throw (UnknownFormatConversionException. c)))))

;; fmt ::= text (specifier text)*
(defn parse-fmt [parser]
  (loop [specifier-index 0
         forms (parse-text parser)]
    (case (parser-peek parser)
      \% (let [specifier-forms (parse-specifier parser specifier-index)
               text-forms (parse-text parser)]
           (recur (inc specifier-index)
                  (-> forms
                      (into specifier-forms)
                      (into text-forms))))

      nil forms)))

