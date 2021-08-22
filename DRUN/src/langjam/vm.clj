(ns langjam.vm
  (:require [langjam.parser :as p]))

(defn fn-call-block->name [block]
  (-> block second second))

(defn fn-call-block->args [block]
  (drop 2 block))

(defn assignment-block->name [block]
  (-> block second second))

(defn assignment-block->val-block [block]
  (nth block 2))

(defn val-block->value [block]
  (-> block second second))

(defn val-block->type [block]
  (-> block second first))

(defn var-block->name [block]
  (second block))

(defn fn-def-block->name [block]
  (-> block second second))

(defn fn-def-block->args [block]
  (let [args (rest (nth block 2))]
    (if (empty? args)
      []
      (map second args))))

(defn fn-def-block->body-blocks [block]
  (drop 3 block))

(defn fn-def? [block]
  (= (first block) :FN-DEF))

(defn fn-call? [block]
  (= (first block) :FN-CALL))

(defn var? [block]
  (= (first block) :VAR))

(defn assignment? [block]
  (= (first block) :ASSIGNMENT))

(defn val? [block]
  (= (first block) :VAL))

(defn var->value [env var-name]
  (get-in env [:vars var-name :value]))

(defn add [env]
  [env (+ (var->value env "a")
          (var->value env "b"))])

(defn sub [env]
  [env (+ (var->value env "a")
          (var->value env "b"))])

(defn print-msg [env & args]
  (print (var->value env "val"))
  [env nil])

(defn env []
  {:fns
   {:ADD {:native? true
          :fn add
          :args ["a" "b"]}
    :SUB {:native? true
          :fn sub
          :args ["a" "b"]}
    :PRINT {:native? true
            :fn print-msg
            :args ["val"]}}
   :vars {}
   :modifiers {};;here will be modifiers from comments stored so they can be accesed by both code and interpreter
   })

(defn bind-variable [env var-name value]
  (assoc-in env [:vars var-name] {:value value
                                  :type "?"}))

(declare exec-fn-call)

(declare exec-return)

(declare exec-fn-body)

(defn exec-assignment-var [env assignment-block]
  (let [var-name (assignment-block->name assignment-block)
        val-block (assignment-block->val-block assignment-block)]
    (bind-variable env (var->value env (var-block->name val-block)))))

(defn exec-assignment-val [env assignment-block]
  (let [var-name (assignment-block->name assignment-block)
        val-block (assignment-block->val-block assignment-block)
        value (val-block->value val-block)]
    (bind-variable env var-name value)))

(defn exec-assignment-fn-call [env [_ var-block fn-call-block]]
  (let [[env value] (exec-fn-call env fn-call-block)
        var-name (var-block->name var-block)]
    (bind-variable env var-name value)))

(defn bind-fn-variables [env args arg-names]
  (loop [env env [arg & args] args [arg-name & arg-names] arg-names]
    (if (nil? arg)
      env
      (let [[env value] (cond
                          (fn-call? arg)
                          (exec-fn-call env arg)
                          (var? arg)
                          [env (var->value env (var-block->name arg))]
                          (val? arg)
                          [env (val-block->value arg)])]
        (recur (bind-variable env arg-name value) args arg-names)))))

(defn exec-fn-call [env fn-call-block]
  (let [fn-name (fn-call-block->name fn-call-block)
        args (fn-call-block->args fn-call-block)
        old-vars (:vars env)
        fn-meta (get-in env [:fns (keyword fn-name)])
        arg-names (:args fn-meta)
        env (bind-fn-variables env args arg-names)
        [nenv ret-val] (if (:native? fn-meta)
                         ((:fn fn-meta) env)
                         (exec-fn-body env (:fn fn-meta)))]
    [(assoc nenv :vars old-vars) ret-val]))

(defn exec-fn-def [env fn-def-block]
  (let [fn-name (fn-def-block->name fn-def-block)
        args (fn-def-block->args fn-def-block)
        body (fn-def-block->body-blocks fn-def-block)]
    (assoc-in env [:fns (keyword fn-name)]
              {:native? false
               :fn body
               :args args})))

(defn test-native-fn-call []
  (exec-fn-call (env) [:FN-CALL [:FN-NAME "ADD"] [:VAL [:NUMBER 2]] [:VAL [:NUMBER 3]]]))

(defn test-parse []
  (let [code-str "FN MAIN()
PRINT(\"test\")
END"
        env (env)
        code (p/parser code-str)
        env (loop [env env terms code]
              (if (nil? (first terms))
                env
                (recur (exec-fn-def env (first terms)) (rest terms))))]
    (exec-fn-call env [:FN-CALL [:FN-NAME "PRINT"] [:VAL [:STRING "TEST"]]])))
