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

(fn find-corrupted-lines [lines]
  (accumulate [sum 0 _ line (ipairs lines)]
    (let [score (line-is-corrupted line)]
      (+ sum (or score 0)))))

(fn part-1 []
  (-> (read-input) (find-corrupted-lines)))

(fn part-2 []
  (print :hi))

{: part-1 : part-2}
