(ns clojure-n.tools.analyzer.swift
  (:refer-clojure :exclude [macroexpand-1 var?])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.passes :refer [schedule]]))

(alias 'c.c 'clojure.core)

(defn macroexpand-1 [form env]
  ;; TODO: Implement
  (c.c/macroexpand-1 form))

(defn create-var
  "Creates a var map for sym and returns it."
  [sym {:keys [ns]}]
  (with-meta {:op   :var
              :name sym
              :ns   ns}
             (meta sym)))

(defn var?
  "Returns true if obj represent a var form as returned by create-var"
  [x]
  (= :var (:op x)))

(defmulti parse
  "Extension to tools.analyzer/-parse for cljn special forms"
  (fn [[op & rest] env] op))

(defmethod parse :default
  [form env]
  (ana/-parse form env))

(defn run-passes
  "Function that will be invoked on the AST tree immediately after it has been constructed,
   by default set-ups and runs the default passes declared in #'default-passes"
  [ast]
  ast)

(defn global-env [] {})

(defn analyze [form env]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var create-var
            ana/parse parse
            ana/var? var?]
    (env/ensure (global-env)
      (run-passes (ana/analyze form env)))))
