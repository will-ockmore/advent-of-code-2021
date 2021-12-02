(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn part-1 []
  (let [puzzle-input :./solutions/inputs/1.txt]
    (var [prev-value sum] [math.huge 0])
    (each [line (io.lines puzzle-input)]
      (let [current (tonumber line)]
        (if (and (not (= current nil)) (> current prev-value))
            (set sum (+ sum 1)))
        (set prev-value current)))
    sum))

(fn part-2 []
  (let [puzzle-input :./solutions/inputs/1.txt]
    (var [prev-value sum prev-line prev-line-2 current-line]
         [math.huge 0 0 0 0])
    (each [line (io.lines puzzle-input)]
      (set current-line (tonumber line))
      (if (not (= current-line nil))
          (let [current-value (+ current-line prev-line prev-line-2)]
            ;; Depth should never be zero, so this just checks that at least three values have been read
            (if (not (lume.any [prev-line prev-line-2] #(= 0 $1)))
                (do
                  (if (> current-value prev-value)
                      (set sum (+ sum 1)))
                  (set prev-value current-value)))
            (set [prev-line prev-line-2] [current-line prev-line]))))
    sum))

{: part-1 : part-2}
