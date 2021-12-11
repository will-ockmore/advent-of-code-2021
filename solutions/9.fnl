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
          (table.insert lowpoints value))))
    (accumulate [sum 0 _ height (ipairs lowpoints)] 
                (+ (+ 1 height) sum))))

(fn part-1 []
  (search-for-low-points (read-input)))

(fn part-2 []
  (print :hi))

{: part-1 : part-2}
