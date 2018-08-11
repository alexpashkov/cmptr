(ns cmptr.term)

(defn create [coef deg] {:coef coef :deg deg})

(defn get-coef [term] (get term :coef))

(defn get-deg [term] (get term :deg))

(defn set-coef [term v] (assoc term :coef v))

(defn complement-coef-sign [term] (set-coef term (- (get-coef term))))

(defn same-deg? [terms]
  (= (count (distinct (map get-deg terms))) 1))

(defn sum-terms [terms]
  (when-not (empty? terms)
    (or (same-deg? terms) (throw (ex-info "Summed up terms must be of same deg" {})))
    (create (reduce #(+ % (get-coef %2)) 0 terms) (get-deg (first terms)))))
