(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/11.txt)]
    (icollect [octopus _ (string.gmatch line "(%d)")]
      (tonumber octopus))))

(fn step [octopuses]
  (var flashes 0)
  (let [max-i (length octopuses)
        max-j (length (. octopuses 1))
        initial-flash-coords []]
    (each [i row (ipairs octopuses)]
      (each [j octopus (ipairs row)]
        (tset octopuses i j
              (if (= octopus 9) (do
                                  (table.insert initial-flash-coords [i j])
                                  :flash)
                  (+ 1 (. octopuses i j))))))

    (fn flash [i j]
      "Flash at coordinates"
      (tset octopuses i j :flash)
      (for [k (- i 1) (+ i 1)]
        (for [m (- j 1) (+ j 1)]
          (when (not (or (< k 1) (< m 1) (> k max-i) (> m max-j)
                         (= (. octopuses k m) :flash)))
            (if (= (. octopuses k m) 9) (flash k m)
                (tset octopuses k m (+ 1 (. octopuses k m))))))))

    (each [_ [i j] (ipairs initial-flash-coords)]
      (flash i j))
    (each [i row (ipairs octopuses)]
      (each [j octopus (ipairs row)]
        (when (= octopus :flash)
          (set flashes (+ flashes 1))
          (tset octopuses i j 0)))))
  flashes)

(local bold "\027[1m")
(local bold-off "\027[\021m")

(fn print-step [octopuses step-num]
  (if step-num
      (print (.. bold "Step " step-num bold-off))
      (print (.. bold "Initial State" bold-off)))
  (print "")
  (each [_ row (ipairs octopuses)]
    (var line "")
    (each [_ octopus (ipairs row)]
      (set line (.. line (if (= octopus 0) (.. bold octopus bold-off) octopus))))
    (set line (.. line "\n"))
    (io.write line))
  (print ""))

(fn run-steps-visualised [octopuses n]
  (var total-flashes 0)
  (print-step octopuses)
  (for [step-num 1 n]
    (set total-flashes (+ total-flashes (step octopuses)))
    (print-step octopuses step-num))
  total-flashes)

(fn run-steps [octopuses n]
  (var total-flashes 0)
  (for [step-num 1 n]
    (set total-flashes (+ total-flashes (step octopuses))))
  total-flashes)

(fn find-all-octopus-flash [octopuses n]
  (var every-octopus-flashed nil)
  (for [step-num 1 math.huge :until every-octopus-flashed]
    (step octopuses)
    (when (utils.all (icollect [_ row (ipairs octopuses)]
                       (utils.all (icollect [_ octopus (ipairs row)]
                                    (= octopus 0)))))
      (set every-octopus-flashed step-num)))
  every-octopus-flashed)

(fn part-1 []
  (-> (read-input) (run-steps 100)))

(fn part-2 []
  (-> (read-input) (find-all-octopus-flash)))

{: part-1 : part-2}
