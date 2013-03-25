(ns playground.core
  (:gen-class))

(def a (int \a))
(def z (int \z))

(defn inc-last[v]
  "increment the last number in the vec"
  (conj (pop v) (inc (last v))))

(defn init[size]
  "get the specified number of A's"
  (repeat (inc size) a))

(defn find-increment [c]
  "from the tail find the number to increment and return the sub sequence from
  head to the incremented number or an empty vector if nothing could be
  incremented further"
  (if (> z (last c))
    (inc-last c)
    (if (< 1 (count c)) 
      (recur (pop c)) 
      [])))

(defn increment [c]
  "spreadsheet column style increment, e.g. AA => AB"
  (let [v (vec (find-increment c))
        vcount (count v)
        ccount (count c)]
    (if (zero? vcount)
      (init ccount)
      (concat v (subvec c vcount ccount)))))

(defn prepare-input [args]
  "takes the first arg and converts it to an int vec"
  (vec (map int (first args))))

(defn prepare-output [c]
  "takes an int seq and converts it to a string"
  (reduce #(str %1 (char %2)) "" c))

(defn -main
  "takes a spreadsheet column id and increments it by one"
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (prepare-output (increment (prepare-input args)))))
