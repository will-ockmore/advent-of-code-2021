(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/12.txt)]
    (icollect [node _ (string.gmatch line "(%a+)")]
      node)))

(fn build-graph [edges]
  (let [nodes {}]
    (each [_ [start end] (ipairs edges)]
      (tset nodes start (or (do
                              (-?> nodes (. start) (table.insert end))
                              (. nodes start))
                            [end]))
      (tset nodes end (or (do
                            (-?> nodes (. end) (table.insert start))
                            (. nodes end)) [start])))
    nodes))

(fn can-visit-part-1 [node current-path]
  (or (not (= (string.lower node) node)) (not (lume.find current-path node))))

(fn small-caves-visited-once [current-path]
  (var more-than-single-visit false)
  (let [visits {}]
    (each [_ step (ipairs current-path) :until more-than-single-visit]
      (when (not (= (string.upper step) step))
        (tset visits step (+ (or (. visits step) 0) 1))
        (set more-than-single-visit (> (. visits step) 1)))))
  (not more-than-single-visit))

(fn can-visit-part-2 [node current-path]
  (or (not (= (string.lower node) node))
      (or (not (lume.find current-path node))
          (and (small-caves-visited-once current-path) (not (= node :start))))))

(fn find-paths [graph can-visit]
  "Recursively explore all paths on the graph according to the rules in part 1"
  (let [paths []]
    (fn find-path [start-node path]
      ;; Copy current path to avoid overwrites
      (each [_ node (ipairs (. graph start-node))]
        (let [current-path [(table.unpack path)]]
          (when (and (not (= node start-node)) (can-visit node current-path))
            (table.insert current-path node)
            (if (= node :end)
                (table.insert paths current-path)
                (find-path node current-path))))))

    (find-path :start [:start])
    paths))

(fn part-1 []
  (-> (read-input)
      (build-graph)
      (find-paths can-visit-part-1)
      (length)))

(fn part-2 []
  (-> (read-input)
      (build-graph)
      (find-paths can-visit-part-2)
      (length)))

{: part-1 : part-2}
