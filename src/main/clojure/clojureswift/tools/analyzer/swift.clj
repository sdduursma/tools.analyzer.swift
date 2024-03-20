(ns clojureswift.tools.analyzer.swift
  (:refer-clojure :exclude [macroexpand-1 var?])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.utils :refer [dissoc-env -source-info]]))

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

(defn analyze-method-impls
  [[method [this & params :as args] & body :as form] env]
  (when-let [error-msg (cond
                         (not (symbol? method))
                         (str "Method method must be a symbol, had: " (class method))
                         (not (vector? args))
                         (str "Parameter listing should be a vector, had: " (class args))
                         (not (first args))
                         (str "Must supply at least one argument for 'this' in: " method))]
    (throw (ex-info error-msg
                    (merge {:form     form
                            :in       (:this env)
                            :method   method
                            :args     args}
                           (-source-info form env)))))
  (let [meth        (cons (vec params) body) ;; this is an implicit arg
        this-expr   {:name  this
                     :env   env
                     :form  this
                     :op    :binding
                     :o-tag (:this env)
                     :tag   (:this env)
                     :local :this}
        env         (assoc-in (dissoc env :this) [:locals this] (dissoc-env this-expr))
        method-expr (ana/analyze-fn-method meth env)]
    (assoc (dissoc method-expr :variadic?)
      :op                   :method
      :form                 form
      :protocol-or-nsobject (:protocol-or-nsobject env)
      :this                 this-expr
      :name                 (symbol (name method))
      :children             (into [:this] (:children method-expr)))))

(defn analyze-type-spec [[protocol-or-nsobject & methods :as form] env]
  (let [methods-env (assoc env
                      :protocol-or-nsobject protocol-or-nsobject
                      :context :ctx/expr)
        methods (mapv #(analyze-method-impls % methods-env) methods)]
    {:op                   :type-spec
     :form                 form
     :protocol-or-nsobject protocol-or-nsobject
     :methods              methods
     :children             [:methods]
     :env                  env}))

(defmethod parse 'deftype*
  ;; TODO: Is class-name needed?
  [[op name class-name fields _ & specs :as form] env]
  (let [swift-protocols-or-nsobject (set (mapv first specs))
        ;; TODO: Refer to NSObject using its fully qualified name?
        swift-protocols (disj swift-protocols-or-nsobject 'NSObject)
        fields-expr  (mapv (fn [name]
                             {:env     env
                              :form    name
                              :name    name
                              :mutable (let [m (meta name)]
                                         (or (and (:unsynchronized-mutable m)
                                                  :unsynchronized-mutable)
                                             (and (:volatile-mutable m)
                                                  :volatile-mutable)))
                              :local   :field
                              :op      :binding})
                           fields)
        specs-env (assoc env
                    :locals  (zipmap fields (map dissoc-env fields-expr))
                    :this    class-name)
        specs-expr (mapv #(analyze-type-spec % specs-env) specs)
        ;; TODO: Add support for options:
        #_#_[opts methods] (parse-opts+methods methods)]
    (cond-> {:op              :deftype
             :env             env
             :form            form
             :name            name
             :class-name      class-name
             ;; TODO: Refer to NSObject using its fully qualified name?
             :swift-protocols swift-protocols
             :fields          fields-expr
             :specs           specs-expr
             :children        [:fields :specs]}

            (swift-protocols-or-nsobject 'NSObject)
            (assoc :nsobject (swift-protocols-or-nsobject 'NSObject)))))

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
