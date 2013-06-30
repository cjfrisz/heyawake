;;----------------------------------------------------------------------
;; File core.clj
;; Written by Chris Frisz
;; 
;; Created 23 Jun 2013
;; Last modified 25 Jun 2013
;; 
;; Simple heyawake puzzle verifier
;;----------------------------------------------------------------------

(ns heyawake.core)

;;------------------------------
;; Board constructors/accessors
;;------------------------------
(defn new-board [w h grid room*] 
  {:width w,
   :height h,
   :grid grid,
   :room* room*})

(def board-get-width :width)

(def board-get-height :height)

(def board-get-grid :grid)

(def board-get-room* :room*)

;;------------------------------
;; Room constructors/accessors
;;------------------------------
(defn new-room
  [id num-black square*]
  {:id id,
   :num-black num-black,
   :square* square*})

(def room-get-id :id)

(def room-get-num-black :num-black)

(def room-get-square* :square*)

;;------------------------------
;; Square constructors/accessors
;;------------------------------
(defn new-square [x y color room] 
  {:x x, :y y, :color color, :room room})

(def square-get-x :x)

(def square-get-y :y)

(def square-get-color :color)

(def square-get-room :room)

;;------------------------------
;; Board traversal
;;------------------------------
(defn get-square 
  [board x y]
  (let [width (board-get-width board)]
    (when (and (>= x 0) (< x width) (>= y 0) (< y (board-get-height board)))
      (nth (board-get-grid board) (+ (* width y) x)))))

(defn get-adjacent-square
  [board square dir]
  (let [x (square-get-x square)
        y (square-get-y square)]
    (case dir
      :left (get-square board (dec x) y)
      :right (get-square board (inc x) y)
      :up (get-square board x (dec y))
      :down (get-square board x (inc y))
      :up-left (get-square board (dec x) (dec y))
      :up-right (get-square board (inc x) (dec y))
      :down-right (get-square board (inc x) (inc y))
      :down-left (get-square board (dec x) (inc y)))))

(defn get-adjacent-squares
  [board square]
  (remove nil?
    (mapv (partial get-adjacent-square board square) 
      [:left :right :up :down])))

;; NB: bug in getting diagonal squares?
(defn get-diagonal-squares
  [board square]
  (remove nil?
    (mapv (partial get-adjacent-square board square) 
      [:up-left :up-right :down-right :down-left])))

;;------------------------------
;; Rule enforcement
;;------------------------------
(defn adjacent-black?
  [board square]
  (and (= (square-get-color square) :black)
       (not (nil? (some #{:black} 
                    (mapv square-get-color 
                          (get-adjacent-squares board square)))))))

(defn split-board?
  [board square]
  (and (= (square-get-color square) :black)
       (loop [worklist [square]
              seen []
              edge-hit? false]
         (and (not (nil? (seq worklist)))
              (let [cur-square (first worklist)
                    cur-square-x (square-get-x cur-square)
                    cur-square-y (square-get-y cur-square)
                    ;; NB: similar logic used in get-square; should lift into predicate
                    edge-square? (or (= cur-square-x 0)
                                     (= cur-square-y 0)
                                     (= cur-square-x
                                        (dec (board-get-width board)))
                                     (= (square-get-y cur-square)
                                        (dec (board-get-height board))))]
                (or (contains? seen cur-square)
                    (and edge-hit? edge-square?)
                    (recur (concat (rest worklist) 
                                   (filter #(= (square-get-color %) :black)
                                     (get-diagonal-squares board cur-square)))
                           (conj seen cur-square)
                           (or edge-hit? edge-square?))))))))

(defn long-white-line?
  [board square]
  (and (= (square-get-color square) :white)
       (or (some true?
             (for [dir [:left :right :up :down]]
               (loop [square square
                      room* #{(room-get-id (square-get-room square))}]
                 (let [cur-room (room-get-id (square-get-room square))]
                   (or (and (= (count room*) 2) 
                            (not (contains? room* cur-room)))
                       (let [next-square (get-adjacent-square board square dir)]
                         (and (not (nil? next-square))
                              (= (square-get-color next-square) :white)
                              (recur next-square (conj room* cur-room)))))))))
           false)))

(defn valid-solution?
  [board]
  (and (not-any? false? (for [room (board-get-room* board)
                              :let [num-black (room-get-num-black room)]]
                          (or (nil? num-black)
                              (= (count (filter #(= (square-get-color %) :black) 
                                          (map (partial apply get-square board) 
                                              (room-get-square* room))))
                                 num-black))))
       (not-any? true? (for [x (range (board-get-width board))
                             y (range (board-get-height board))]
                          ((some-fn 
                            (partial adjacent-black? board)
                            (partial split-board? board)
                            (partial long-white-line? board))
                           (get-square board x y))))))

;;------------------------------
;; Puzzle definitions
;;------------------------------
(defn make-puzzle
  [w h & roomdef*]
  (loop [roomdef* roomdef*
         grid (vector (repeat (* w h) nil))
         room* []]
    (if (nil? (seq roomdef*))
        (new-board w h grid room*)
        (let [[room-w room-h tlx tly num-black & color*] (first roomdef*)]
          (when-not (= (* room-w room-h) (count color*))
            (throw (Exception. (str "mismatched room dimensions and colors for "
                                    (first roomdef*)))))
          (let [room-id (count room*)
                [GRID new-room] (loop [color* color*
                                       cur-x tlx
                                       cur-y tly
                                       grid grid
                                       coord* []]
                                  (if (nil? (seq color*))
                                      [grid (new-room room-id num-black coord*)]
                                      (let [next-x (mod (inc cur-x) room-w)]
                                        (recur (rest color*)
                                               next-x
                                               (if (zero? next-x) 
                                                   (inc cur-y)
                                                   cur-y)
                                               (assoc grid (+ (* cur-y w) cur-x)
                                                      (new-square cur-x cur-y 
                                                                  (first color*)
                                                                  room-id))
                                               (conj coord* [cur-x cur-y])))))]
            (recur (rest roomdef*) GRID (conj room* new-room)))))))
                                 
