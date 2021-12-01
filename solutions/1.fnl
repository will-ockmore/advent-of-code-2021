(local utils (require :solutions.utils))


(fn one []
  (let [puzzle-input "./solutions/inputs/1.txt"]
    (var prev-value math.huge)
    (var sum 0)
    (each [line (io.lines puzzle-input)]
      (let [current (tonumber line)]
        (if 
          (and 
            (not (= current nil))
            (> current prev-value))
          (set sum (+ sum 1)))
         (set prev-value current)))
    sum))

(print (one))
