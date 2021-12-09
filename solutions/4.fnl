(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn read-boards [f]
  (var line "")
  (var boards [])
  (var current-board [])
  (while (not (= line nil))
    (if (= line "") (if (> (length current-board) 0)
                        (do
                          (table.insert boards current-board)
                          (set current-board [])))
        (table.insert current-board
                      (icollect [num _ (string.gmatch line "(%d+)")]
                        (tonumber num))))
    (set line (f:read)))
  boards)

(fn rows-and-columns [boards]
  (icollect [_ board (ipairs boards)]
    {:rows (icollect [_ row (ipairs board)]
             (collect [_ value (ipairs row)]
               (values value false)))
     ;; Use standard matrix notation - i for column, j for row
     :columns (icollect [j _ (ipairs board)]
                (collect [i _ (ipairs (. board 1))]
                  (values (. board i j) false)))}))

(fn set-num [rows-cols num]
  (each [_ board-rows-cols (ipairs rows-cols)]
    (each [j row (ipairs (. board-rows-cols :rows))]
      (each [k _ (pairs row)]
        (if (= k num) (tset board-rows-cols :rows j num true))))
    (each [i col (ipairs (. board-rows-cols :columns))]
      (each [k _ (pairs col)]
        (if (= k num) (tset board-rows-cols :columns i num true))))))

(fn board-is-complete [board-rows-cols]
  (let [has-completed-row (utils.any (icollect [_ row (ipairs (. board-rows-cols
                                                                 :rows))]
                                       (utils.all (icollect [k v (pairs row)]
                                                    v))))
        has-completed-column (utils.any (icollect [_ col (ipairs (. board-rows-cols
                                                                    :columns))]
                                          (utils.all (icollect [k v (pairs col)]
                                                       v))))]
    (or has-completed-row has-completed-column)))

(fn which-board-complete [rows-cols]
  "Find the first complete board"
  (var winning-board nil)
  (each [i board-rows-cols (ipairs rows-cols) :until winning-board]
    (if (board-is-complete board-rows-cols) (set winning-board board-rows-cols)))
  winning-board)

(fn which-boards-complete [rows-cols]
  "Find all complete boards"
  (var winning-boards [])
  (each [i board-rows-cols (ipairs rows-cols)]
    (if (board-is-complete board-rows-cols)
        (table.insert winning-boards board-rows-cols)))
  winning-boards)

(fn read-input []
  (with-open [f (io.open :./solutions/inputs/4.txt)]
    {:numbers (icollect [num _ (string.gmatch (f:read) "(%d+),")]
                (tonumber num))
     :boards (read-boards f)}))

(fn part-1 []
  (var winning-board nil)
  (var winning-board-num nil)
  (let [{: numbers : boards} (read-input)]
    (let [rows-cols (rows-and-columns boards)]
      (each [_ num (ipairs numbers) :until winning-board]
        (set-num rows-cols num)
        (set winning-board (which-board-complete rows-cols))
        (set winning-board-num num))))
  (let [sum-of-unmarked-nums (accumulate [sum 0 _ row (ipairs (. winning-board
                                                                 :rows))]
                               (+ sum
                                  (accumulate [inner-sum 0 num is-found (pairs row)]
                                    (if (not is-found) (+ inner-sum num)
                                        inner-sum))))]
    (* sum-of-unmarked-nums winning-board-num)))

(fn part-2 []
  (var losing-board nil)
  (var losing-board-num nil)
  (let [{: numbers : boards} (read-input)]
    (let [rows-cols (rows-and-columns boards)]
      (each [_ num (ipairs numbers) :until losing-board]
        (set-num rows-cols num)
        (let [winning-boards (which-boards-complete rows-cols)]
          ;; When the final board has won
          (when (= (length winning-boards) (length rows-cols) 1)
            (do
              (set losing-board (. winning-boards (length winning-boards)))
              (set losing-board-num num))))
        ;; Remove any winning boards
        (each [i board-rows-cols (ipairs rows-cols)]
          (when (board-is-complete board-rows-cols)
            (table.remove rows-cols i))))))
  (let [sum-of-unmarked-nums (accumulate [sum 0 _ row (ipairs (. losing-board
                                                                 :rows))]
                               (+ sum
                                  (accumulate [inner-sum 0 num is-found (pairs row)]
                                    (if (not is-found) (+ inner-sum num)
                                        inner-sum))))]
    (* sum-of-unmarked-nums losing-board-num)))

{: part-1 : part-2}
