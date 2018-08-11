(ns cmptr.term)

(defn create [coef deg] {:coef coef :deg deg})

(defn get-coef [term] (get term :coef))
(defn get-deg [term] (get term :deg))
(defn set-coef [term v] (assoc term :coef v ))
(defn complement-coef-sign [term] (set-coef term (- (get-coef term))))
