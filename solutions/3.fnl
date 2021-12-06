(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn multiply-epsilon-gamma [[gamma epsilon]]
  (* (tonumber gamma 2) (tonumber epsilon 2)))

(fn part-1 []
  (let [puzzle-input :./solutions/inputs/3.txt]
    (-> (accumulate [counts nil line (io.lines puzzle-input)]
          (if (or (= nil line) (= "" line))
              counts
              (let [characters (icollect [char _ (string.gmatch line ".")]
                                 char)]
                (if (= counts nil)
                    (icollect [_ char (ipairs characters)]
                      [(if (= char :0) 1 0) (if (= char :1) 1 0)])
                    (icollect [i char (ipairs characters)]
                      (let [[zero-count one-count] (. counts (tonumber i))]
                        [(if (= char :0) (+ zero-count 1) zero-count)
                         (if (= char :1) (+ one-count 1) one-count)]))))))
        (lume.reduce #(let [[gamma epsilon] $1
                            [zero-count one-count] $2]
                        [(.. gamma (if (> zero-count one-count) :0 :1))
                         (.. epsilon (if (> zero-count one-count) :1 :0))])
                     ["" ""])
        (multiply-epsilon-gamma))))

(fn part-2 []
  (print :hi))

{: part-1 : part-2}
