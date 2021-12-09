(local lume (require :lib.lume))

(local utils (require :solutions.utils))

(fn multiply-epsilon-gamma [[gamma epsilon]]
  (* (tonumber gamma 2) (tonumber epsilon 2)))

(fn part-1 []
  (let [puzzle-input :./solutions/inputs/3.txt]
    (-> (accumulate [counts nil line (io.lines puzzle-input)]
          (if (or (= nil line) (= "" line))
              counts
              (let [characters (icollect [char _ (string.gmatch line ".")]
                                 char)]
                (if (= counts nil)
                    (icollect [_ char (ipairs characters)]
                      [(if (= char :0) 1 0) (if (= char :1) 1 0)])
                    (icollect [i char (ipairs characters)]
                      (let [[zero-count one-count] (. counts (tonumber i))]
                        [(if (= char :0) (+ zero-count 1) zero-count)
                         (if (= char :1) (+ one-count 1) one-count)]))))))
        (lume.reduce #(let [[gamma epsilon] $1
                            [zero-count one-count] $2]
                        [(.. gamma (if (> zero-count one-count) :0 :1))
                         (.. epsilon (if (> zero-count one-count) :1 :0))])
                     ["" ""])
        (multiply-epsilon-gamma))))

(fn split-groups [numbers index]
  "Split the numbers by the value at index"
  (accumulate [character-groups [[] []] i number (ipairs numbers)]
    (if (or (= nil number) (= "" number)) character-groups
        (let [character (string.sub number index index)
              [zero-group one-group] character-groups]
          (if (= character :0) (table.insert zero-group number) zero-group)
          (if (= character :1) (table.insert one-group number) one-group)
          [zero-group one-group]))))

(fn satisfy-bit-criteria [comparator tiebreak-value]
  "Compare the groups and handle a tiebreak situation"
  (fn [character-groups]
    (let [[zero-group one-group] character-groups]
      (if (comparator (length zero-group) (length one-group)) zero-group
          (= (length zero-group) (length one-group)) (. character-groups
                                                        tiebreak-value)
          one-group))))

(local rating-criteria
       {:oxygen-generator-rating [#(> $1 $2) 2]
        :co2-scrubber-rating [#(< $1 $2) 1]})

(fn filter-rating [numbers index criteria]
  (-> (split-groups numbers index)
      ((satisfy-bit-criteria (table.unpack (. rating-criteria criteria))))
      ((fn [group]
         (if (= (length group) 1) (. group 1)
             (filter-rating group (+ 1 index) criteria))))))

(fn part-2 []
  (local input (utils.get-contents :./solutions/inputs/3.txt))
  (let [oxygen-generator-rating (filter-rating input 1 :oxygen-generator-rating)
        co2-scrubber-rating (filter-rating input 1 :co2-scrubber-rating)]
    (* (tonumber oxygen-generator-rating 2) (tonumber co2-scrubber-rating 2))))

{: part-1 : part-2}
