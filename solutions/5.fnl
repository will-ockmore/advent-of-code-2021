(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/5.txt)]
    (icollect [coord _ (string.gmatch line "(%d+,%d+)")]
      (icollect [dim _ (string.gmatch coord "(%d+)")]
        (tonumber dim)))))

(fn segment-is-vertical [segment]
  (let [[[x1 y1] [x2 y2]] segment]
    (= x1 x2)))

(fn segment-is-horizontal [segment]
  (let [[[x1 y1] [x2 y2]] segment]
    (= y1 y2)))

(fn coords-from-segment [segment]
  (let [[[x1 y1] [x2 y2]] segment]
    (if (segment-is-vertical segment)
        (icollect [_ num (ipairs (utils.range (math.min y1 y2) (math.max y1 y2)))]
          [x1 num])
        (segment-is-horizontal segment)
        (icollect [_ num (ipairs (utils.range (math.min x1 x2) (math.max x1 x2)))]
          [num y1])
        (icollect [i num (ipairs (utils.range (math.min x1 x2) (math.max x1 x2)))]
          [(+ x1 (if (> x1 x2) (- 1 i) (- i 1)))
           (+ y1 (if (> y1 y2) (- 1 i) (- i 1)))]))))

(fn part-1 []
  (let [points {}
        segments (icollect [_ segment (ipairs (read-input))]
                   (when (or (segment-is-vertical segment)
                             (segment-is-horizontal segment))
                     segment))]
    (each [_ segment (ipairs segments)]
      (each [_ coord (ipairs (coords-from-segment segment))]
        (tset points (lume.serialize coord)
              (+ 1 (or (. points (lume.serialize coord)) 0)))))
    (accumulate [sum 0 _ count (pairs points)]
                (if (> count 1) (+ sum 1) sum))))

(fn part-2 []
  (let [points {}
        segments (read-input)]
    (each [_ segment (ipairs segments)]
      (each [_ coord (ipairs (coords-from-segment segment))]
        (tset points (lume.serialize coord)
              (+ 1 (or (. points (lume.serialize coord)) 0)))))
    (accumulate [sum 0 _ count (pairs points)]
                      (if (> count 1) (+ sum 1) sum))))

{: part-1 : part-2}
