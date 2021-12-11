(local lume (require :lib.lume))
(local fennel (require :lib.fennel))

(local utils (require :solutions.utils))

(fn read-input []
  (with-open [f (io.open :./solutions/inputs/6.txt)]
    (icollect [num _ (string.gmatch (f:read) "(%d)")]
      (tonumber num))))

(fn progress-day [lanternfish]
  (let [new-fish []]
    (each [i fish (ipairs lanternfish)]
      (when (= 0 fish)
        (table.insert new-fish 8))
      (tset lanternfish i (if (= 0 fish) 6 (- fish 1))))
    (each [_ fish (ipairs new-fish)]
      (table.insert lanternfish fish)))
  lanternfish)

(fn get-days [lanternfish]
  "Transform the individual lanternfish to a day representation"
  (accumulate [days [0 0 0 0 0 0 0 0 0] _ num (ipairs lanternfish)]
    (do
      (tset days (+ 1 num) (+ 1 (. days (+ 1 num))))
      days)))

(fn progress-day [days]
  (let [spawning-fish (table.remove days 1)]
    ;; New fish should start on day 9 
    (table.insert days spawning-fish)
    ;; Fish that have spawned should start on day 7
    (tset days 7 (+ (. days 7) spawning-fish)))
  days)

(fn progress-days [number-of-days]
  (var lanternfish (read-input))
  (var days (get-days lanternfish))
  (for [i 1 number-of-days]
    (set days (progress-day days)))
  (accumulate [sum 0 _ day (ipairs days)]
    (+ sum day)))

(fn part-1 []
  (progress-days 80))

(fn part-2 []
  (progress-days 256))

{: part-1 : part-2}
