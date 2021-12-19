(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (with-open [f (io.open :./solutions/inputs/13.txt)]
    (local dots [])
    (local folds [])
    (var line (f:read))
    (while (not (= line ""))
      (local [j i] (icollect [digit _ (string.gmatch line "(%d+)")]
                     (tonumber digit)))
      (table.insert dots [(+ i 1) (+ j 1)])
      (set line (f:read)))
    (set line (f:read))
    (while line
      (local [axis index] [(string.match line "(%l)=(%d+)")])
      (table.insert folds [axis (+ 1 (tonumber index))])
      (set line (f:read)))
    {: dots : folds}))

(fn print-matrix [{: dots}]
  (let [max-i (accumulate [max 0 _ [i j] (ipairs dots)]
                (math.max i max))
        max-j (accumulate [max 0 _ [i j] (ipairs dots)]
                (math.max j max))
        matrix []]
    (for [i 1 max-i]
      (tset matrix i [])
      (for [j 1 max-j]
        (tset matrix i j :empty)))
    (each [_ [i j] (ipairs dots)]
      (tset matrix i j :dot))
    (print "")
    (each [_ row (ipairs matrix)]
      (local line (accumulate [l "" _ entry (ipairs row)]
                    (.. l (if (= entry :dot) "#" "."))))
      (print line))))

(fn calculate-fold [dots axis index]
  (if (= axis :y)
      (icollect [_ [i j] (ipairs dots)]
        [(if (> i index) (- index (- i index)) i) j])
      (icollect [_ [i j] (ipairs dots)]
        [i (if (> j index) (- index (- j index)) j)])))

(fn remove-duplicates [dots]
  (let [seen {}
        result []]
    (each [_ coords (ipairs dots)]
      (when (not (. seen (lume.serialize coords)))
        (table.insert result coords))
      (tset seen (lume.serialize coords) true))
    result))

(fn get-first-fold [{: dots : folds}]
  (let [[axis index] (. folds 1)]
    {:dots (-> (calculate-fold dots axis index) (remove-duplicates)) : folds}))

(fn get-all-folds [{: dots : folds}]
  (var current-dots dots)
  (each [_ [axis index] (ipairs folds)]
    (set current-dots (calculate-fold current-dots axis index) ))
    {:dots current-dots : folds})

(fn number-of-dots [{: dots}]
  (length dots))

(fn part-1 []
  (-> (read-input)
      (get-first-fold)
      (number-of-dots)))

(fn part-2 []
  (-> (read-input)
      (get-all-folds)
      (print-matrix)))

{: part-1 : part-2}
