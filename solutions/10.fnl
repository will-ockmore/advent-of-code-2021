(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/10.txt)]
    (icollect [char _ (string.gmatch line "(.)")]
      char)))

(fn line-is-corrupted [line]
  (let [stack []]
    (var score nil)
    (each [_ c (ipairs line) :until score]
      (match c
        (where (or "(" "<" "[" "{")) (table.insert stack c)
        ")" (if (= (. stack (length stack)) "(") (table.remove stack)
                (set score 3))
        "]" (if (= (. stack (length stack)) "[") (table.remove stack)
                (set score 57))
        "}" (if (= (. stack (length stack)) "{") (table.remove stack)
                (set score 1197))
        ">" (if (= (. stack (length stack)) "<") (table.remove stack)
                (set score 25137))))
    score))

(fn score-incomplete-line [line]
  (var score 0)
  (let [stack []]
    (each [_ c (ipairs line)]
      (match c
        (where (or "(" "<" "[" "{")) (table.insert stack c)
        _ (table.remove stack)))
    (for [i (length stack) 1 -1]
      (set score (+ (* 5 score) (match (. stack i)
                                  "(" 1
                                  "[" 2
                                  "{" 3
                                  "<" 4)))))
  score)

(fn find-middle-score [lines]
  (let [scores (icollect [_ line (ipairs lines)]
                 (score-incomplete-line line))]
    (table.sort scores)
    (. scores (. (+ 1 (// (length scores) 2))))))

(fn find-corrupted-lines [lines]
  (accumulate [sum 0 _ line (ipairs lines)]
    (let [score (line-is-corrupted line)]
      (+ sum (or score 0)))))

(fn filter-corrupted-lines [lines]
  (icollect [_ line (ipairs lines)]
    (when (not (line-is-corrupted line))
      line)))

(fn part-1 []
  (-> (read-input) (find-corrupted-lines)))

(fn part-2 []
  (-> (read-input) (filter-corrupted-lines) (find-middle-score)))

{: part-1 : part-2}
