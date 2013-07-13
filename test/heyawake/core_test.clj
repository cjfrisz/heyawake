(ns heyawake.core-test
  (:use clojure.test
        heyawake.core))

(defn get-square-by-color
  [board color]
  (filter (comp (partial = color) square-get-color) 
    (board-get-grid board)))

(defmacro test-by-color
  [pred board color]
  `(doseq [square# (get-square-by-color ~board ~color)]
     (is (~pred ~board square#))))

(let [room (new-room 0 2 [[0 0] [0 1] [0 2]])
		     room-id (room-get-id room)
		     square1 (new-square 0 0 :black room-id)
		     square2 (new-square 0 1 :white room-id)
		     square3 (new-square 0 2 :black room-id)
		     board (new-board 1 3 [square1 square2 square3] [room])]
  (deftest adjacent-black-tests
    (is (every? false? 
          (mapv (partial adjacent-black? board)
            [square1 square2 square3]))))
  (deftest split-board-tests
    (is (every? false?
          (mapv (partial split-board? board)
            [square1 square2 square3]))))
  (deftest long-white-line-test
    (is (every? false?
          (mapv (partial long-white-line? board)
            [square1 square2 square3])))))

(let [room (new-room 0 2 [[0 0] [0 1] [0 2]])
      room-id (room-get-id room)
      square1 (new-square 0 0 :black room-id)
      square2 (new-square 0 1 :white room-id)
      square3 (new-square 0 2 :black room-id)
      square4 (new-square 1 0 :white room-id)
      square5 (new-square 1 1 :black room-id)
      square6 (new-square 1 2 :white room-id)
      board (new-board 3 2 [square1 square2 square3 square4 square5 square6]
      [room])]
(deftest split-board-tests2
  (is (every? true?
              (mapv (partial split-board? board)
                    [square1 square3 square5])))
  (is (every? false?
              (mapv (partial split-board? board)
                    [square2 square4 square6])))))

(deftest verify-basic-board
  (is (valid-solution? (make-puzzle 1 3 [1 3 0 0 2 :black :white :black]))))

(deftest adjacent-black-test3
  (let [puzzle (make-puzzle 2 1 [2 1 0 0 2 :black :black])]
    (is (adjacent-black? puzzle (get-square puzzle 0 0)))
    (is (adjacent-black? puzzle (get-square puzzle 1 0)))))

(deftest long-white-line-test
  (let [puzzle (make-puzzle 2 1
                 [1 1 0 0 0 :white]
                 [1 1 1 0 0 :white])]
    (doseq [x (range 2)]
      (is (not (long-white-line? puzzle (get-square puzzle x 0))))))
  (let [puzzle (make-puzzle 3 1
                 [1 1 0 0 0 :white]
                 [1 1 1 0 0 :white]
                 [1 1 2 0 0 :white])]
    (doseq [x (range 3)]
      (is (long-white-line? puzzle (get-square puzzle x 0))))))

(deftest adjacent-black
  (let [puzzle (make-puzzle 1 2 [1 2 0 0 2 :black :black])]
    (is (adjacent-black? puzzle (get-square puzzle 0 0)))
    (is (adjacent-black? puzzle (get-square puzzle 0 1))))
  (let [puzzle (make-puzzle 3 3 
                 [3 3 0 0 5
                  :black :white :black
                  :white :black :white
                  :black :white :black])]
    (test-by-color (comp not adjacent-black?) puzzle :black))
  (let [puzzle (make-puzzle 2 2 
                 [1 2 0 0 1
                  :black
                  :white]
                 [1 2 1 0 1
                  :white 
                  :black])]
    (test-by-color (comp not adjacent-black?) puzzle :black))
  (let [puzzle (make-puzzle 2 2
                 [1 2 0 0 1
                  :black
                  :white]
                 [1 2 1 0 1
                  :black
                  :white])]
    (test-by-color adjacent-black? puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [2 2 0 0 2
                  :black :white
                  :white :black]
                 [1 2 2 0 1
                  :black
                  :white]
                 [3 1 0 2 2
                  :black :whate :black])]
    (test-by-color (comp not adjacent-black?) puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 5
                  :white :black :white
                  :black :black :black
                  :white :black :white])]
    (test-by-color adjacent-black? puzzle :black)))

(deftest split-board
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 4
                  :white :black :white
                  :black :white :black
                  :white :black :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 3
                  :white :black :white
                  :white :white :black
                  :white :black :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 2
                  :white :white :white
                  :white :white :black
                  :white :black :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 2
                  :white :black :white
                  :black :white :white
                  :white :white :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 2
                  :white :black :white
                  :white :white :black
                  :white :white :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 2
                  :white :black :white
                  :white :white :white
                  :white :black :white])]
    (test-by-color (comp not split-board?) puzzle :black))
  (let [puzzle (make-puzzle 3 3
                 [3 3 0 0 2
                  :white :white :white
                  :black :white :black
                  :white :white :white])]
    (test-by-color (comp not split-board?) puzzle :black))
  (let [puzzle (make-puzzle 2 3
                 [2 3 0 0 3
                  :black :white
                  :white :black
                  :black :white])]
    (test-by-color split-board? puzzle :black))
  ;; NB: this puzzle creates a long white line
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :white]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :black]
                 [1 3 2 1 0
                  :white
                  :white
                  :white]
                 [3 1 0 4 0
                  :white :white :white])]
    (test-by-color (comp not split-board?) puzzle :black))
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :white]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :black]
                 [1 3 2 1 0
                  :white
                  :black
                  :white]
                 [3 1 0 4 0
                  :white :white :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :black]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :black]
                 [1 3 2 1 0
                  :white
                  :white
                  :white]
                 [3 1 0 4 0
                  :white :white :white])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :white]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :black]
                 [1 3 2 1 0
                  :white
                  :white
                  :white]
                 [3 1 0 4 0
                  :white :white :black])]
    (test-by-color split-board? puzzle :black))
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :black]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :white]
                 [1 3 2 1 0
                  :white
                  :white
                  :white]
                 [3 1 0 4 0
                  :white :white :black])]
    (doseq [x (range 3)
            :let [y (- 2 x)]]
      (is (split-board? puzzle (get-square puzzle x y))))
    (is (not (split-board? puzzle (get-square puzzle 4 2))))))

(deftest long-white-lines
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :white]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :black]
                 [1 3 2 1 0
                  :white
                  :white
                  :white]
                 [3 1 0 4 0
                  :white :white :white])]
    (doseq [y (range 5)]
      (is (long-white-line? puzzle (get-square puzzle 2 y))))
    (is (not (long-white-line? puzzle (get-square puzzle 0 0)))))
  (let [puzzle (make-puzzle 3 5
                 [3 1 0 0 0
                  :white :white :white]
                 [2 3 0 1 3
                  :white :black
                  :black :white
                  :white :white]
                 [1 3 2 1 0
                  :white
                  :white
                  :white]
                 [3 1 0 4 0
                  :white :white :black])]
    (test-by-color (comp not long-white-line?) puzzle :white))
  (let [puzzle (make-puzzle 9 1
                 [3 1 0 0 0
                  :white :white :white]
                 [3 1 3 0 1
                  :white :black :white]
                 [3 1 6 0 0
                  :white :white :white])]
    (test-by-color (comp not long-white-line?) puzzle :white))
  (let [puzzle (make-puzzle 9 1
                 [3 1 0 0 0
                  :white :white :white]
                 [3 1 3 0 1
                  :white :white :white]
                 [3 1 6 0 0
                  :white :white :white])]
    (test-by-color long-white-line? puzzle :white)))

(deftest room-fill-count
  (let [puzzle (make-puzzle 1 1 [1 1 0 0 0 :white])]
    (is (valid-solution? puzzle)))
  (let [puzzle (make-puzzle 1 1 [1 1 0 0 1 :black])]
    (is (valid-solution? puzzle)))
  (let [puzzle (make-puzzle 1 1 [1 1 0 0 0 :black])]
    (is (not (valid-solution? puzzle))))
  (let [puzzle (make-puzzle 1 1 [1 1 0 0 1 :white])]
    (is (not (valid-solution? puzzle))))
  (let [puzzle (make-puzzle 2 1
                 [1 1 0 0 0 :white]
                 [1 1 1 0 1 :black])]
    (is (valid-solution? puzzle)))
  (let [puzzle (make-puzzle 2 1
                 [1 1 0 0 1 :black]
                 [1 1 1 0 0 :white])]
    (is (valid-solution? puzzle))))

(deftest book-puzzles
  ;; build up the first book puzzle by bits
  (let [puzzle (make-puzzle 3 3
                 [1 3 0 0 2
                  :black
                  :white
                  :black]
                 [1 3 1 0 nil
                  :white
                  :white
                  :white]
                 [1 3 2 0 nil
                  :white
                  :black
                  :white])]
    (is (valid-solution? puzzle)))
  (let [puzzle (make-puzzle 3 8
                 [1 3 0 0 2
                  :black
                  :white
                  :black]
                 [1 3 1 0 nil
                  :white
                  :white
                  :white]
                 [1 3 2 0 nil
                  :white
                  :black
                  :white]
                 [1 5 0 3 1
                  :white
                  :black
                  :white
                  :white
                  :white]
                 [2 3 1 3 2
                  :white :black
                  :white :white
                  :white :black]
                 [2 2 1 6 2
                  :black :white
                  :white :black])]
    (is (not (valid-solution? puzzle)))
    (is (split-board? puzzle (get-square puzzle 2 5)))
    (is (split-board? puzzle (get-square puzzle 1 6)))
    (is (split-board? puzzle (get-square puzzle 2 7)))
    (is (not (adjacent-black? puzzle (get-square puzzle 2 5))))
    (is (not (adjacent-black? puzzle (get-square puzzle 1 6))))
    (is (not (adjacent-black? puzzle (get-square puzzle 2 7))))
    (is (not (long-white-line? puzzle (get-square puzzle 2 5))))
    (is (not (long-white-line? puzzle (get-square puzzle 1 6))))
    (is (not (long-white-line? puzzle (get-square puzzle 2 7))))
    (doseq [x (range 3)
            y (range 8)
            :when (not (or (and (= x 2) (or (= y 5) (= y 7)))
                           (and (= x 1) (= y 6))))]
      (is (not ((some-fn (partial adjacent-black? puzzle)
                  (partial split-board? puzzle)
                  (partial long-white-line? puzzle))
                 puzzle (get-square puzzle x y))))))
  (let [puzzle (make-puzzle 5 10
                 [1 3 0 0 2
                  :black
                  :white
                  :black]
                 [1 3 1 0 nil
                  :white
                  :white
                  :white]
                 [1 3 2 0 nil
                  :white
                  :black
                  :white]
                 [1 5 0 3 1
                  :white
                  :black
                  :white
                  :white
                  :white]
                 [2 3 1 3 2
                  :white :black
                  :white :white
                  :white :black]
                 [2 2 1 6 2
                  :black :white
                  :white :black]
                 [2 2 0 8 2
                  :white :black
                  :black :white]
                 [3 1 2 8 nil
                  :white :black :white]
                 [3 1 2 9 nil
                  :white :white :white]
                 [2 4 3 4 nil
                  :white :white
                  :white :white
                  :white :white
                  :white :black]
                 ; NOT FROM ORIGINAL BOARD
                 [2 2 3 2 2
                  :black :white
                  :white :black]
                 [2 2 3 0 1
                  :black :white
                  :white :white])]
    (is (not (valid-solution? puzzle)))
    (doseq [x (range 5)
            y (range 10)
            :let [square (get-square puzzle x y)]]
      (if (or (and (= x 2) (or (= y 1) (= y 3)))
              (and (= x 3) (or (= y 0) (= y 2)))
              (and (= x 4) (= y 3)))
          (is ((every-pred (comp not (partial adjacent-black? puzzle))
                 (comp not (partial long-white-line? puzzle))
                 (partial split-board? puzzle))
                square))
          (is ((every-pred (comp not (partial adjacent-black? puzzle))
                 (comp not (partial long-white-line? puzzle)))
                 (comp not (partial split-board? puzzle)))
                square))))
  (let [puzzle (make-puzzle 7 10
                 [1 3 0 0 2
                  :black
                  :white
                  :black]
                 [1 3 1 0 nil
                  :white
                  :white
                  :white]
                 [1 3 2 0 nil
                  :white
                  :black
                  :white]
                 [1 5 0 3 1
                  :white
                  :black
                  :white
                  :white
                  :white]
                 [2 3 1 3 2
                  :white :black
                  :white :white
                  :white :black]
                 [2 2 1 6 2
                  :black :white
                  :white :black]
                 [2 2 0 8 2
                  :white :black
                  :black :white]
                 [3 1 2 8 nil
                  :white :black :white]
                 [3 1 2 9 nil
                  :white :white :white]
                 [2 4 3 4 nil
                  :white :white
                  :white :white
                  :white :white
                  :white :black]
                 [3 2 3 2 3
                  :black :white :black
                  :white :black :white]
                 [3 2 3 0 2
                  :black :white :black
                  :white :white :white]
                 [1 4 6 0 nil
                  :white
                  :white
                  :white
                  :white]
                 [2 4 5 4 nil
                  :black :white
                  :white :white
                  :black :white
                  :white :white]
                 [2 2 5 8 2
                  :white :black
                  :black :white])]
    (is (not (valid-solution? puzzle)))
    (doseq [x (range 7)
            y (range 10)
            :let [square (get-square puzzle x y)]]
      (if (or (and (= x 5) (= y 9))
              (and (= x 6) (= y 8)))
          (is ((every-pred (partial split-board? puzzle)
                 (comp not (partial adjacent-black? puzzle))
                 (comp not (partial long-white-line? puzzle)))
                square))
          (is (not ((some-fn (partial adjacent-black? puzzle)
                      (partial split-board? puzzle)
                      (partial long-white-line? puzzle))
                     square))))))
  (let [puzzle (make-puzzle 10 10
                 [1 3 0 0 2
                  :black
                  :white
                  :black]
                 [1 3 1 0 nil
                  :white
                  :white
                  :white]
                 [1 3 2 0 nil
                  :white
                  :black
                  :white]
                 [1 5 0 3 1
                  :white
                  :black
                  :white
                  :white
                  :white]
                 [2 3 1 3 2
                  :white :black
                  :white :white
                  :white :black]
                 [2 2 1 6 2
                  :black :white
                  :white :black]
                 [2 2 0 8 2
                  :white :black
                  :black :white]
                 [3 1 2 8 nil
                  :white :black :white]
                 [3 1 2 9 nil
                  :white :white :white]
                 [2 4 3 4 nil
                  :white :white
                  :white :white
                  :white :white
                  :white :black]
                 [3 2 3 2 3
                  :black :white :black
                  :white :black :white]
                 [3 2 3 0 2
                  :black :white :black
                  :white :white :white]
                 [1 4 6 0 nil
                  :white
                  :white
                  :white
                  :white]
                 [2 4 5 4 nil
                  :black :white
                  :white :white
                  :black :white
                  :white :white]
                 [2 2 5 8 2
                  :white :black
                  :black :white]
                 [1 3 7 7 0
                  :white
                  :white
                  :white]
                 [2 2 8 8 nil
                  :white :white
                  :black :white]
                 [2 1 8 7 nil
                  :black :white]
                 [3 1 7 6 nil
                  :white :white :black]
                 [3 1 7 5 1
                  :black :white :white]
                 [2 4 7 1 4
                  :black :white
                  :white :black
                  :black :white
                  :white :black]
                 [1 4 9 1 1
                  :black
                  :white
                  :white
                  :white]
                 [3 1 7 0 nil
                  :white :white :white])]
    (is (valid-solution? puzzle))
    ;; check that changing the color of any piece invalidates the solution
    (doseq [x (range 10)
            y (range 10)
            :let [square (get-square puzzle x y)
                  mangled (assoc puzzle :grid
                            (assoc (board-get-grid puzzle) (+ (* y 10) x)
                              (assoc square :color 
                                (case (square :color)
                                  :black :white
                                  :white :black))))]]
      (is (not (valid-solution? mangled))))))
