(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn part-1 []
  (let [puzzle-input :./solutions/inputs/2.txt]
    (lume.reduce (accumulate [position [0 0] line (io.lines puzzle-input)]
                   (if (or (= nil line) (= "" line))
                       position
                       (let [[action distance] (lume.split line)
                             [x y] position]
                         (match action
                           :forward [(+ x distance) y]
                           :down [x (+ y distance)]
                           :up [x (- y distance)]))))
                 #(* $1 $2))))

(fn part-2 []
  (let [puzzle-input :./solutions/inputs/2.txt]
    (let [[x y aim] (accumulate [position [0 0 0] line (io.lines puzzle-input)]
                      (if (or (= nil line) (= "" line))
                          position
                          (let [[action distance] (lume.split line)
                                [x y aim] position]
                            (match action
                              :forward [(+ x distance)
                                        (+ y (* distance aim))
                                        aim]
                              :down [x y (+ aim distance)]
                              :up [x y (- aim distance)]))))]
      (* x y))))

{: part-1 : part-2}
