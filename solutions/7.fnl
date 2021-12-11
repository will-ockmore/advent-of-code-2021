(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (with-open [f (io.open :./solutions/inputs/7.txt)]
    (icollect [num _ (string.gmatch (f:read) "(%d+)")]
      (tonumber num))))

(fn distance [nums index]
  (accumulate [sum 0 _ num (ipairs nums)]
    (+ sum (math.abs (- num index)))))

(fn nth-triangular-number [n]
  (/ (* n (+ n 1)) 2))

(fn fuel-spent [nums index]
  (accumulate [sum 0 _ num (ipairs nums)]
    (+ sum (nth-triangular-number (math.abs (- num index))))))

(fn part-1 []
  (let [puzzle-input (read-input)]
    (accumulate [best math.huge i num (ipairs puzzle-input)]
      (math.min best (distance puzzle-input i)))))

(fn part-2 []
  (let [puzzle-input (read-input)]
    (accumulate [best math.huge i num (ipairs puzzle-input)]
      (math.min best (fuel-spent puzzle-input i)))))

{: part-1 : part-2}
