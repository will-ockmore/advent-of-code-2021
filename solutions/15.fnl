(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/15.txt)]
    (icollect [risk _ (string.gmatch line "(%d)")]
      (tonumber risk))))

(fn read-input-test []
  (icollect [line (io.lines :./15-test.txt)]
    (icollect [risk _ (string.gmatch line "(%d)")]
      (tonumber risk))))

;; Helper functions for priority queue
(fn get-distance [node]
  (. node :risk-distance))

(fn push [queue element-to-insert comparator]
  "Insert element in order"
  (local rank-function (or comparator get-distance))
  (var inserted false)
  (each [i el (ipairs queue) :until inserted]
    (when (and (>= (rank-function el) (rank-function element-to-insert))
               (or (= i 1)
                   (<= (rank-function (. queue (- i 1)))
                       (rank-function element-to-insert))))
      (table.insert queue i element-to-insert)
      (set inserted true)))
  (when (not inserted)
    (table.insert queue element-to-insert)))

(fn pop [queue index]
  (table.remove queue (or index 1)))

(fn find-node [queue i j]
  (var node nil)
  (var node-index nil)
  (each [index curr (ipairs queue) :until node]
    (let [[k m] (. curr :coords)]
      (when (and (= i k) (= j m))
        (set node curr)
        (set node-index index))))
  [node node-index])

(fn find-shortest-path [risk-map]
  "Dijkstra's algorithm to traverse from top left to bottom right of matrix"
  (let [q []
        visited {}
        max-i (length risk-map)
        max-j (length (. risk-map 1))]
    ;; Insert the start node
    (var current-node {:risk-distance 0 :coords [1 1] :from-coords nil})
    (table.insert q current-node)
    (while (not (. visited (fennel.view [max-i max-j])))
      (let [[i j] (. current-node :coords)
            nodes-to-visit (icollect [_ [k m] (ipairs [[(- i 1) j]
                                                       [i (- j 1)]
                                                       [(+ i 1) j]
                                                       [i (+ j 1)]])]
                             (when (and (< 0 k) (< 0 m) (>= max-i k)
                                        (>= max-j m)
                                        (not (. visited (fennel.view [k m]))))
                               [k m]))]
        (each [_ [k m] (ipairs nodes-to-visit)]
          (let [node {:risk-distance (+ (. current-node :risk-distance)
                                        (. risk-map k m))
                      :coords [k m]
                      :from-coords [i j]}
                [prev-entry prev-entry-index] (find-node q k m)]
            (when (> (or (?. prev-entry :risk-distance) math.huge)
                     (. node :risk-distance))
              (when prev-entry
                (pop q prev-entry-index))
              (push q node))))
        (tset visited (fennel.view [i j]) current-node))
      (set current-node (pop q)))
    ;; Walk path backwards to start node
    (local path [])
    (set current-node (. visited (fennel.view [max-i max-j])))
    (while current-node
      (table.insert path current-node)
      (set current-node (. visited (fennel.view (. current-node :from-coords)))))
    {: path : risk-map}))

(fn manhattan-distance [start end]
  (let [[i j] start [k m] end]
    (+ (- k i) (- m j))))

(fn a*-rank [node] 
  (+ (. node :risk-distance) (. node :distance-to-end)))

(fn find-shortest-path-a* [risk-map]
  "A* algorithm to traverse from top left to bottom right of matrix"
  (let [q []
        visited {}
        max-i (length risk-map)
        max-j (length (. risk-map 1))]
    ;; Insert the start node
    (var current-node {:risk-distance 0 :distance-to-end (manhattan-distance [1 1] [max-i max-j]) :coords [1 1] :from-coords nil})
    (table.insert q current-node)
    (while (not (. visited (fennel.view [max-i max-j])))
      (let [[i j] (. current-node :coords)
            nodes-to-visit (icollect [_ [k m] (ipairs [[(- i 1) j]
                                                       [i (- j 1)]
                                                       [(+ i 1) j]
                                                       [i (+ j 1)]])]
                             (when (and (< 0 k) (< 0 m) (>= max-i k)
                                        (>= max-j m)
                                        (not (. visited (fennel.view [k m]))))
                               [k m]))]
        (each [_ [k m] (ipairs nodes-to-visit)]
          (let [node {:risk-distance (+ (. current-node :risk-distance)
                                        (. risk-map k m))
                      :distance-to-end (manhattan-distance [k m] [max-i max-j])
                      :coords [k m]
                      :from-coords [i j]}
                [prev-entry prev-entry-index] (find-node q k m)]
            (when (> (or (?. prev-entry :risk-distance) math.huge)
                     (. node :risk-distance))
              (when prev-entry
                (pop q prev-entry-index))
              (push q node a*-rank))))
        (tset visited (fennel.view [i j]) current-node))
      (set current-node (pop q)))
    ;; Walk path backwards to start node
    (local path [])
    (set current-node (. visited (fennel.view [max-i max-j])))
    (while current-node
      (table.insert path current-node)
      (set current-node (. visited (fennel.view (. current-node :from-coords)))))
    {: path : risk-map}))

(local bold "\027[1m")
(local bold-off "\027[\021m")

(fn print-matrix-with-path [{: path : risk-map}]
  "Display path superimposed on risk map"
  (print "")
  (each [i row (ipairs risk-map)]
    (var line "")
    (each [j risk (ipairs row)]
      (local is-path (< 0 (length (find-node path i j))))
      (set line (.. line (if is-path (.. bold risk bold-off) risk))))
    (set line (.. line "\n"))
    (io.write line))
  (print "")
  {: risk-map : path})

(fn get-total-risk [{: path}]
  (. path 1 :risk-distance))

(fn repeat-grid [risk-map]
  "Expand the grid 5 times in x and y directions, incrementing risk each time"
  (let [initial-max-i (length risk-map)
        initial-max-j (length (. risk-map 1))]
    (for [i 1 (* 5 initial-max-i)]
      (when (not (. risk-map i))
        (tset risk-map i []))
      (for [j 1 (* 5 initial-max-j)]
        (let [initial-i (+ 1 (% (- i 1) initial-max-i))
              initial-j (+ 1 (% (- j 1) initial-max-j))
              initial-value (. risk-map initial-i initial-j)
              i-repeats (// (- i 1) initial-max-i)
              j-repeats (// (- j 1) initial-max-j)
              increased-value (+ 1 (% (+ (- initial-value 1) i-repeats
                                         j-repeats)
                                      9))]
          (tset risk-map i j increased-value)))))
  risk-map)

(fn part-1 []
  (-> (read-input)
      (find-shortest-path)
      (get-total-risk)))

(fn part-2 []
  (-> (read-input)
      (repeat-grid)
      (find-shortest-path-a*)
      (get-total-risk)))

{: part-1 : part-2}
