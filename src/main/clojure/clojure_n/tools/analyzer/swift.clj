(ns clojure-n.tools.analyzer.swift
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]))

;; TODO
(declare create-var)

;; TODO
(declare parse)

;; TODO
(declare -analyze)

;; TODO
(declare run-passes)

(defn global-env [] {})

(defn analyze [form env]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var create-var
            ana/parse parse
            ana/var? var?]
    (env/ensure (global-env)
      (run-passes (-analyze form env)))))
