(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (with-open [f (io.open :./solutions/inputs/14.txt)]
    (var polymer-template (f:read))
    (local rules {})
    (var line (f:read))
    ;; skip blank line
    (set line (f:read))
    (while line
      (local [adjacent-elements element-to-insert]
             [(string.match line "(%u+).*(%u)")])
      (tset rules adjacent-elements element-to-insert)
      (set line (f:read)))
    {: polymer-template : rules}))

(fn run-step [template rules]
  (var result "")
  (let [characters (icollect [c _ (string.gmatch template "(%u)")]
                     c)]
    (each [i char (ipairs characters)]
      (local insertion (or (when (< i (length characters))
                             (. rules (.. char (. characters (+ i 1)))))
                           ""))
      (set result (.. result char insertion))))
  result)

(fn transform-rules [template-and-rules]
  (local pair-transformations {})
  (each [pair insertion (pairs (. template-and-rules :rules))]
    (tset pair-transformations pair
          [(.. (string.sub pair 1 1) insertion)
           (.. insertion (string.sub pair 2 2))]))
  (tset template-and-rules :pair-transformations pair-transformations)
  template-and-rules)

(fn run-step-2 [pairs-counts pair-transformations]
  (local temp-counts {})
  (each [pair count (pairs pairs-counts)]
    (each [_ new-pair (ipairs (. pair-transformations pair))]
      (tset temp-counts new-pair (+ count (or (. temp-counts new-pair) 0)))))
  temp-counts)

(fn run-steps [{: polymer-template : rules} n]
  (var result polymer-template)
  (for [i 1 n]
    (set result (run-step result rules)))
  result)

(fn run-steps-2 [{: polymer-template : pair-transformations} n]
  (var pairs-counts {})
  (let [characters (icollect [c _ (string.gmatch polymer-template "(%u)")]
                     c)]
    (each [i char (ipairs characters)]
      (when (< i (length characters))
        (local pair (.. char (. characters (+ i 1))))
        (tset pairs-counts pair (+ 1 (or (. pairs-counts pair) 0))))))
  (for [i 1 n]
    (set pairs-counts (run-step-2 pairs-counts pair-transformations)))
  {: pairs-counts : polymer-template})

(fn count-occurrences [polymer]
  (let [characters (icollect [c _ (string.gmatch polymer "(%u)")]
                     c)
        counts {}]
    (each [_ char (ipairs characters)]
      (tset counts char (+ 1 (or (. counts char) 0))))
    counts))

(fn count-occurrences-2 [{: pairs-counts : polymer-template}]
  (let [counts {}]
    (each [pair count (pairs pairs-counts)]
      (let [char (string.sub pair 1 1)]
        (tset counts char (+ count (or (. counts char) 0)))))
    ;; The first and last characters of the string are not double counted, like all others
    ;; so need an additional increment
    (let [first (string.sub polymer-template 1 1)
          last (string.sub polymer-template -1)]
      (tset counts first (+ 1 (. counts first)))
      (tset counts last (+ 1 (. counts last))))
    counts))

(fn subtract-least-from-greatest [counts]
  (var max 0)
  (var min math.huge)
  (each [k v (pairs counts)]
    (set max (math.max max v))
    (set min (math.min min v)))
  (- max min))

(fn part-1 []
  (-> (read-input)
      (run-steps 10)
      (count-occurrences)
      (subtract-least-from-greatest)))

(fn part-2 []
  (-> (read-input)
      (transform-rules)
      (run-steps-2 40)
      (count-occurrences-2)
      (subtract-least-from-greatest)
      (fennel.view)))

{: part-1 : part-2}
