(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp prod]
                (if (zero? exp)
                  prod

                  (recur base
                         (dec exp)
                         (* base prod))))]

    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)

                   (recur (rest a-seq))))]

    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (if (and (= (first seq1)
                             (first seq2))
                          (or
                            (and
                             (not (empty? seq1))
                             (not (empty? seq2)))
                            (and
                             (empty? seq1)
                             (empty? seq2))))

                   (if (and (empty? (rest seq1))
                        (empty? (rest seq2)))
                     true

                     (recur (rest seq1) (rest seq2)))

                   false))]

    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         the-seq a-seq
         the-pred pred]
    (cond
     (empty? the-seq)
       nil
     (the-pred (first the-seq))
       index

     :else (recur (inc index)
                  (rest the-seq)
                  the-pred))))

(defn avg [a-seq]
    (loop [index 0
           the-seq a-seq
           sum 0]
    (cond
     (empty? the-seq)
       (cond
        (= 0 index)
          nil
        :else
          (/ sum index))

     :else (recur (inc index)
                  (rest the-seq)
                  (+ sum (first the-seq))))))

(defn parity [a-seq]
  (loop [the-set #{}
         toggle
         (fn [a-set elem]
           (if (contains? a-set elem)
             (disj a-set elem)
             (conj a-set elem)))
         the-seq a-seq]
    (cond
     (empty? the-seq)
       the-set
     :else (recur (toggle the-set (first the-seq))
                  toggle
                  (rest the-seq)))))

(defn fast-fibo [n]
  (loop [f_n-1 0
         f_n 1
         index n]
    (cond
     (zero? index)
       f_n-1
    :else
     (recur f_n (+ f_n-1 f_n) (dec index)))))

(defn cut-at-repetition [a-seq]
  [":("])




