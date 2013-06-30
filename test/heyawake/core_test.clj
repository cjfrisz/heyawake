(ns heyawake.core-test
  (:use clojure.test
        heyawake.core))

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
    (for [x (range 2)]
      (is (not (long-white-line? puzzle (get-square puzzle x 0))))))
  (let [puzzle (make-puzzle 3 1
                 [1 1 0 0 0 :white]
                 [1 1 1 0 0 :white]
                 [1 1 2 0 0 :white])]
    (for [x (range 3)]
      (is (long-white-line? puzzle (get-square puzzle x 0))))))
