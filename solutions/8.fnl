(local fennel (require :lib.fennel))
(local lume (require :lib.lume))

(local utils (require :solutions.utils))

;; Implement sets and related functions
(fn toset [xs]
  "Takes a list and transforms to a set"
  (collect [_ v (ipairs xs)]
    (values v true)))

(fn tolist [xs]
  "Takes a set and transforms to a list"
  (icollect [k _ (pairs xs)]
    k))

(fn pop [xs]
  "Return single element from set"
  (. (tolist xs) 1))

(fn difference [first second]
  "Return elements in first that are not in second"
  (collect [elem _ (pairs first)]
    (when (not (. second elem))
      (values elem true))))

(fn intersection [first second]
  "Return only elements in first that are also in second"
  (collect [elem _ (pairs first)]
    (when (. second elem)
      (values elem true))))

(fn union [first second]
  "Return all elements in first and second"
  (let [result {}]
    (each [elem _ (pairs first)]
      (tset result elem true))
    (each [elem _ (pairs second)]
      (tset result elem true))
    result))

(fn set-equal [first second]
  "A set is equal to a second set if their elements are the same"
  (and (= 0 (length (tolist (difference first second))))
       (= 0 (length (tolist (difference second first))))))

(fn read-input []
  (icollect [line (io.lines :./solutions/inputs/8.txt)]
            (when (not (= line ""))
              (let [[p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 & output] (icollect [signal-pattern _ (string.gmatch line
                                                                                                 "(%a+)")]
                                                        signal-pattern)]
        {:patterns [p1 p2 p3 p4 p5 p6 p7 p8 p9 p10] : output}))))

(fn find-possible-digits [pattern]
  "Determine possible digits"
  (match (length pattern)
    2 (toset [:one])
    3 (toset [:seven])
    4 (toset [:four])
    5 (toset [:two :three :five])
    6 (toset [:zero :six :nine])
    7 (toset [:eight])
    _ :unknown))

(local true-patterns {:zero (toset [:a :b :c :e :f :g])
                      :one (toset [:c :f])
                      :two (toset [:a :c :d :e :g])
                      :three (toset [:a :c :d :f :g])
                      :four (toset [:b :c :d :f])
                      :five (toset [:a :b :d :f :g])
                      :six (toset [:a :b :d :f :e :g])
                      :seven (toset [:a :c :f])
                      :eight (toset [:a :b :c :d :e :f :g])
                      :nine (toset [:a :b :c :d :f :g])})

(local symbol-to-digit {:zero 0
                        :one 1
                        :two 2
                        :three 3
                        :four 4
                        :five 5
                        :six 6
                        :seven 7
                        :eight 8
                        :nine 9})

(fn find-digit [pattern-objs digit]
  (var m nil)
  (each [_ pattern-obj (pairs pattern-objs) :until m]
    (when (. pattern-obj :possible-digits digit)
      (set m pattern-obj)))
  m)

(fn determine-mapping [patterns]
  (let [pattern-objs (collect [_ pattern (ipairs patterns)]
                       (values pattern
                               {: pattern
                                :possible-digits (find-possible-digits pattern)
                                :elements (toset (icollect [p _ (string.gmatch pattern
                                                                               "(%a)")]
                                                   p))}))
        ;; A mapping which will be used to progressively narrow the digits that could map
        mapping {:a :unknown
                 :b :unknown
                 :c :unknown
                 :d :unknown
                 :e :unknown
                 :f :unknown
                 :g :unknown}]
    ;; Narrow the search using the known digits
    (let [one (find-digit pattern-objs :one)
          four (find-digit pattern-objs :four)
          seven (find-digit pattern-objs :seven)
          eight (find-digit pattern-objs :eight)]
      ;; Seven and one share two digits, so :a can be deduced immediately
      (tset mapping :a (difference (. seven :elements) (. one :elements)))
      ;; Seven and one share two digits, so we can narrow those also
      (let [shared-elements (intersection (. seven :elements) (. one :elements))]
        (tset mapping :c shared-elements)
        (tset mapping :f shared-elements))
      ;; One and four share two digits, so the other two can be narrowed
      (let [shared-elements (difference (. four :elements) (. one :elements))]
        (tset mapping :b shared-elements)
        (tset mapping :d shared-elements))
      ;; The remaining digits are found in eight
      (let [remaining-elements (difference (. eight :elements)
                                           (union (. seven :elements)
                                                  (. four :elements)))]
        (tset mapping :e remaining-elements)
        (tset mapping :g remaining-elements))
      ;; The only element present in every pattern that we can't determine is :g
      (let [undetermined-patterns (icollect [_ pattern-obj (pairs pattern-objs)]
                                    (when (not (or (. pattern-obj
                                                      :possible-digits :one)
                                                   (. pattern-obj
                                                      :possible-digits :four)
                                                   (. pattern-obj
                                                      :possible-digits :seven)
                                                   (. pattern-obj
                                                      :possible-digits :eight)))
                                      pattern-obj))
            undetermined-elements (difference (. eight :elements)
                                              (. mapping :a))]
        (tset mapping :g
              (accumulate [acc undetermined-elements _ {: elements} (ipairs undetermined-patterns)]
                (intersection elements acc))))
      ;; Now that we know what :g maps to, we can remove it as a possibility for other elements
      (each [wire possible-elements (pairs mapping)]
        (when (and (not (= wire :g)) (. possible-elements (pop (. mapping :g))))
          (tset mapping wire (difference possible-elements (. mapping :g)))))
      ;; The only 5 element digit which contains all of :a, :g and :e wires is 2
      (let [two (. (icollect [_ pattern-obj (pairs pattern-objs)]
                     (when (and (. pattern-obj :elements (pop (. mapping :a)))
                                (. pattern-obj :elements (pop (. mapping :g)))
                                (. pattern-obj :elements (pop (. mapping :e)))
                                (= (length (. pattern-obj :pattern)) 5))
                       pattern-obj)) 1)]
        ;; At this point, we can narrow for :d and :c
        (tset mapping :d (intersection (. two :elements) (. mapping :d)))
        (tset mapping :c (intersection (. two :elements) (. mapping :c))))
      ;; And remove them as possibilities for the other elements
      ;; This gives the full mapping
      (each [wire possible-elements (pairs mapping)]
        (when (and (not (= wire :c)) (. possible-elements (pop (. mapping :c))))
          (tset mapping wire (difference possible-elements (. mapping :c))))
        (when (and (not (= wire :d)) (. possible-elements (pop (. mapping :d))))
          (tset mapping wire (difference possible-elements (. mapping :d))))))
    ;; Transform from a set representation to a direct mapping
    ;; of lit element to true wire
    (collect [wire element-set (pairs mapping)]
      (values (pop element-set) wire))))

(fn transform-pattern [pattern mapping]
  (toset (icollect [letter (string.gmatch pattern "(%l)")]
           (. mapping letter))))

(fn find-digit-from-pattern [pattern]
  (var result nil)
  (each [digit true-pattern (pairs true-patterns) :until result]
    (when (set-equal pattern true-pattern)
      (set result (. symbol-to-digit digit))))
  result)

(fn get-display [entry]
  (let [mapping (determine-mapping (. entry :patterns))]
    (local digits
           (icollect [_ pattern (ipairs (. entry :output))]
             (find-digit-from-pattern (transform-pattern pattern mapping))))
    (var number-string "")
    (each [_ digit (ipairs digits)]
      (set number-string (.. number-string digit)))
    (tonumber number-string)))

(fn part-1 []
  (accumulate [sum 0 _ {: output} (ipairs (read-input))]
    (+ sum (accumulate [inner-sum 0 _ pattern (ipairs output)]
             (if (or (. (find-possible-digits pattern) :one)
                     (. (find-possible-digits pattern) :four)
                     (. (find-possible-digits pattern) :seven)
                     (. (find-possible-digits pattern) :eight))
                 (+ inner-sum 1)
                 inner-sum)))))

(fn part-2 []
  (accumulate [sum 0 _ entry (ipairs (read-input))]
    (+ sum (get-display entry))))

{: part-1 : part-2}
