(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/9.txt)]
    (icollect [height _ (string.gmatch line "(%d)")]
      (tonumber height))))

(fn search-for-low-points [heightmap]
  (let [max-i (length heightmap)
        max-j (length (. heightmap 1))
        lowpoints []]
    (fn is-lowpoint [i j]
      (let [height (. heightmap i j)]
        (and (if (= i 1) true (> (. heightmap (- i 1) j) height))
             ;; Directly adjacent tiles 
             (if (= i max-i) true (> (. heightmap (+ i 1) j) height))
             (if (= j 1) true (> (. heightmap i (- j 1)) height))
             (if (= j max-j) true (> (. heightmap i (+ j 1)) height))
             ;; Diagonals / Corners
             (if (or (= i 1) (= j 1)) true
                 (> (. heightmap (- i 1) (- j 1)) height))
             (if (or (= i 1) (= j max-j)) true
                 (> (. heightmap (- i 1) (+ j 1)) height))
             (if (or (= i max-i) (= j 1)) true
                 (> (. heightmap (+ i 1) (- j 1)) height))
             (if (or (= i max-i) (= j max-j)) true
                 (> (. heightmap (+ i 1) (+ j 1)) height)))))

    (each [i row (ipairs heightmap)]
      (each [j value (ipairs row)]
        (when (is-lowpoint i j)
          (table.insert lowpoints [i j value]))))
    lowpoints))

(fn total-lowpoint-risk [lowpoints]
  (accumulate [sum 0 _ [_ _ height] (ipairs lowpoints)]
    (+ (+ 1 height) sum)))

(fn find-basins [heightmap lowpoints]
  (let [max-i (length heightmap)
        max-j (length (. heightmap 1))]
    (fn explore-basin [i j basin]
      "Recursively explore basin until a 9 level height is reached"
      (tset basin (fennel.view [i j]) true)
      ;; Down
      (when (not (or (?. basin (fennel.view [(+ i 1) j])) (= i max-i)
                     (= (. heightmap (+ i 1) j) 9)))
        (explore-basin (+ i 1) j basin))
      ;; Up
      (when (not (or (?. basin (fennel.view [(- i 1) j])) (= i 1)
                     (= (. heightmap (- i 1) j) 9)))
        (explore-basin (- i 1) j basin))
      ;; Right
      (when (not (or (?. basin (fennel.view [i (+ j 1)])) (= j max-i)
                     (= (. heightmap i (+ j 1)) 9)))
        (explore-basin i (+ j 1) basin))
      ;; Left
      (when (not (or (?. basin (fennel.view [i (- j 1)])) (= j 1)
                     (= (. heightmap i (- j 1)) 9)))
        (explore-basin i (- j 1) basin))
      basin)

    (let [basins (icollect [_ [i j _] (ipairs lowpoints)]
                   (explore-basin i j {(fennel.view [i j]) true}))]
      ;; Get the total size of largest 3 basins and multiply together
      (table.sort basins #(> (length (lume.keys $1)) (length (lume.keys $2))))
      (* (length (lume.keys (. basins 1))) (length (lume.keys (. basins 2)))
         (length (lume.keys (. basins 3)))))))

(fn part-1 []
  (-> (read-input)
      (search-for-low-points)
      (total-lowpoint-risk)))

(fn part-2 []
  (let [heightmap (read-input)]
    (->> heightmap
         (search-for-low-points)
         (find-basins heightmap))))

{: part-1 : part-2}
