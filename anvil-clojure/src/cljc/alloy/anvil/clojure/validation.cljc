(ns alloy.anvil.clojure.validation)

(defn register
	[& {:keys [spec description]} ] ())

(register {:key ::vector :spec vector? :description "A vector"})