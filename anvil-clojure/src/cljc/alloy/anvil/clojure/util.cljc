(ns alloy.anvil.clojure.util
	(:require [clojure.data :as data]
		#?(:clj [clojure.spec :as spec]
			 :cljs [cljs.spec :as spec])))

(defn pour
	"Wraps argument in a vector if it is not already a collection."
	[arg]
	(if (coll? arg)
		arg
		[arg]))

(def non-zero? (complement zero?))
(def non-nil? (complement nil?))
(def non-neg? (complement neg?))

(defn group-by-index [ccol]
	(loop [remainder ccol
				 result []]
		(if (every? empty? remainder)
			result
			(recur
				(map rest remainder)
				(conj result (map first remainder))))))

(defn coll-contains? [col v] (boolean (some #(= v %) col)))

(defn to-vec [col] (into [] col))

(defn fn-rest [delegate]
	(fn [& args] (apply delegate (rest args))))

(defn find-first
	[f coll]
	(first (filter f coll)))

(defn take-even [x]
	(take-nth 2 x))

(defn take-odd [x]
	(take-nth 2 (drop 1 x)))

(def rand-uuid #?(:clj  #(java.util.UUID/randomUUID)
						 			:cljs cljs.core/random-uuid))

(def key-identical? #?(:clj  identical?
											 :cljs cljs.core/keyword-identical?))

(def spec-invalid #?(:clj  :clojure.spec/invalid
										 :cljs :cljs.spec/invalid))

(defn spec-invalid-result? [result] (key-identical? result spec-invalid))

(defn gen-symbol
	([]
	 (gen-symbol "_"))
	([prefix]
	 (-> prefix (str (rand-uuid)) symbol)))

(defn countf [f c]
	(->> c (filter f) count))

(def curry-blank? #(= % '_))

(defn- curry-gather-args [args]
	(let [symbols (map #(if (curry-blank? %) (gen-symbol) nil) args)]
		[(filter (comp not nil?)  symbols)
		 (map (partial reduce (fn [arg symbol] (if (curry-blank? arg) symbol arg)))
					(map vector args symbols))]))

(defmacro curry [func & args]
	(let [gathered-args (curry-gather-args args)]
		(list 'fn (into [] (first gathered-args))
					(conj (second gathered-args) func))))

(defn assert-error [& messages]
	#?(:clj (throw (new AssertionError (apply pr-str messages)))
		 :cljs (throw (js/Error. (apply pr-str messages)))))

(defn position-merge-helper [values target result]
	(cond (empty? values) (concat result target)
				(empty? target) (concat result (map first values))
				:else
				(if (zero? (second (first values)))
					(recur (map #(vector (first %) (dec (second %))) (rest values))
								 target
								 (conj result (first (first values))))
					(recur (map #(vector (first %) (dec (second %))) values)
								 (rest target)
								 (conj result (first target))))))

;[["a" 0] ["c" 2]]
;["b" "d"]
;["a" "b" "c" "d"]
(defn position-merge [values target]
	(position-merge-helper (sort-by second values) target []))

(defn rand-between [left-bound right-bound]
	(+ (rand (- right-bound left-bound)) left-bound))

(defn seq-to-map [list]
	(into {} (map vec (partition 2 list))))

(defn flatten-1
	"Flattens only the first level of a given sequence, e.g. [[1 2][3]] becomes
	 [1 2 3], but [[1 [2]] [3]] becomes [1 [2] 3]."
	[seq]
	(if (or (not (seqable? seq)) (nil? seq))
		seq ; if seq is nil or not a sequence, don't do anything
		(loop [acc [] [elt & others] seq]
			(if (nil? elt) acc
										 (recur
											 (if (seqable? elt)
												 (apply conj acc elt) ; if elt is a sequence, add each element of elt
												 (conj acc elt))      ; if elt is not a sequence, add elt itself
											 others)))))

(defn map-to-seq [map] (flatten-1 (seq map)))
(defn map-to-vec [map] (into [] (map-to-seq map)))

(defn args-to-map [args] (if (map? args)
													 args
													 (seq-to-map args)))

(defn conform-or-throw [spec arg]
	(let [result (spec/conform spec arg)]
		(if (spec-invalid-result? result)
			(assert-error (spec/explain-str spec arg))
			result)))

(defn schema-field-to-key-pair [field]
	[(first field)
	 (:default (seq-to-map (rest field)))])

(defn defaults-map [schema]
	(into {}
				(map vec
						 (filter (fn [[_ value]] (some? value))
										 (map schema-field-to-key-pair (:fields schema))))))

(defn build-args
	([args schema] (build-args args {} schema))
	([args default-overrides schema]
	 (merge (defaults-map schema) (args-to-map args))))

(defn third
	"Same as (first (next x))"
	[coll]
	(first (next (next coll))))

(defn build-default-overrides [current-args previous-args previous-conformed-args]
	(map (fn [key] [key (key previous-conformed-args)])
			 (keys (third
							 (data/diff
								 (args-to-map current-args)
								 (args-to-map previous-args))))))

(defn conform-args
	([args schema]
	 (conform-args args {} schema))
	([args default-overrides schema]
	 (conform-or-throw (:spec schema) (build-args args default-overrides schema))))

(defn fn-metadata [func]
	(let [vec-func (to-vec func)]
		{:body (nth vec-func 2)
		 :args (nth vec-func 1)}))

(defn schema-fields [schema] ())

(defn build-let-vec [arg-symbol conformed-arg-symbol schema template-args]
	[conformed-arg-symbol `(conform-args (first ~arg-symbol) ~schema)
	 (first template-args) conformed-arg-symbol])

(defn build-fn-internal [arg-symbol conformed-arg-symbol schema template]
	(let [template-metadata (fn-metadata template)]
		`(let ~(build-let-vec arg-symbol conformed-arg-symbol schema (:args template-metadata))
			 ~(:body template-metadata))))

(defmacro build-fn
	([arg-symbol schema template]
	 (build-fn-internal arg-symbol (gen-symbol) schema template))
	([arg-symbol conformed-arg-symbol schema template]
	 (build-fn-internal arg-symbol conformed-arg-symbol schema template)))

(defmacro build-handler-fn [arg-symbol schema initializer-template handler-template]
	(let [initializer-metadata (fn-metadata initializer-template)
				handler-metadata (fn-metadata handler-template)
				outer-confomed-args-symbol (gen-symbol)
				initialization-result-symbol (gen-symbol)
				previous-args-atom-symbol (gen-symbol)
				inner-arg-symbol (gen-symbol)
				inner-confomed-args-symbol (gen-symbol)
				previous-args-symbol (gen-symbol)]
		(build-fn-internal arg-symbol outer-confomed-args-symbol schema
							`(fn ~(:args initializer-metadata)
								(let [~initialization-result-symbol ~(:body initializer-metadata)
											~previous-args-atom-symbol (atom [(first ~arg-symbol) ~outer-confomed-args-symbol])]
									(fn [& ~inner-arg-symbol]
										(let [~previous-args-symbol (deref ~previous-args-atom-symbol)
													~inner-confomed-args-symbol (conform-args (first ~inner-arg-symbol)
																																	 (build-default-overrides
																																		 (first ~inner-arg-symbol)
																																		 (first ~previous-args-symbol)
																																		 (second ~previous-args-symbol)) ~schema)
													~(first (:args handler-metadata)) (merge ~inner-confomed-args-symbol
																																	 ~initialization-result-symbol)]
											(do
												(reset! ~previous-args-atom-symbol [(first ~inner-arg-symbol) ~inner-confomed-args-symbol])
												~(:body handler-metadata)))))))))

(defn ratom? [val] (fn? val))
(defn hiccup? [val] (vector? val))
(defn hiccup-ratom-or-str? [val] (or (ratom? val) (hiccup? val) (string? val)))

(spec/def ::label hiccup-ratom-or-str?)
(spec/def ::content hiccup-ratom-or-str?)
(spec/def ::dropdown (spec/keys :req-un [::label ::content]))

(def dropdown-schema
	{:fields [[:label :description "This is the description"]
						[:content :description "This is the content"]]
	 :spec ::dropdown})

(defn dropdown [& args]
	(build-fn args dropdown-schema
								 (fn [{:keys [label content open-state]}]
									 [:div label])))

(defn dropdown2 [& args]
	(build-handler-fn args dropdown-schema
									 (fn [{:keys [label content open-state]}]
										 {:extra-param "Extra param!"})
									 (fn [args]
										 (println args))))

;(defn dropdown2-expanded [& args]
;	(let [conformed-args-rand-symbol (conform-args args dropdown-schema)
;				{:keys [label content open-state]} conformed-args-rand-symbol]
;		(let [initialized-results-rand-symbol
;					(do (print "blah")
;							label)
;					previous-args-atom-rand-symbol (atom [args conformed-args-rand-symbol])]
;			(fn [& args-rand-symbol]
;				(let [previous-args-rand-symbol @previous-args-atom-rand-symbol
;							sub-conformed-args-rand-symbol
;								(merge initialized-results-rand-symbol
;										 (conform-args args-rand-symbol
;																	 (build-default-overrides
;																		 args-rand-symbol
;																		 (first previous-args-rand-symbol)
;																		 (second previous-args-rand-symbol)) dropdown-schema))
;							{:keys [label content open-state]} sub-conformed-args-rand-symbol]
;					(do
;						(reset! previous-args-atom-rand-symbol [args-rand-symbol sub-conformed-args-rand-symbol])
;						[:div label]))))))