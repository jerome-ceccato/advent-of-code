(require 
  hyrule [-> ->>]
  hyrule.destructure [let+ fn+]
  hyrule.anaphoric * :readers [%])

(defn parse-line [line]
  (let+ [[left right] (.split line ":")
        numbers (list (map int (filter None (.split right " "))))]
    [(int left) numbers]))

(with [file (open "input" "r")]
  (setv input-data (->> 
    file
    .read
    (#%(.split %1 "\n"))
    (map parse-line)
    list)))

(defn int-concat [a b]
 (int (+ (str a) (str b))))

(defn is-valid [target numbers acc allow-concat]
  (if (= numbers []) (= acc target)
    (let+ [[a :& rest] numbers]
        (or 
          (is-valid target rest (+ acc a) allow-concat) 
          (is-valid target rest (* acc a) allow-concat)
          (and allow-concat (is-valid target rest (int-concat acc a) allow-concat))))))

(defn count-valid [data allow-concat]
  (->> data
    (map (fn+ [[target numbers]] (if (is-valid target numbers 0 allow-concat) target 0)))
    sum))

(-> input-data (count-valid False) print)
(-> input-data (count-valid True) print)
