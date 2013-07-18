;;----------------------------------------------------------------------
;; File core.cljs
;; Written by Chris
;; 
;; Created 13 Jul 2013
;; Last modified 17 Jul 2013
;; 
;; 
;;----------------------------------------------------------------------

(ns cljs.core.cljs
  (:require [domina :refer (append! by-id log set-text!)]))

(log "Loading ClojureScript...")

(def game-fps 60)

(defn init-game [& args])
(defn game-loop [& args])

;; Start the game when the window loads
(set! (.-onload js/window) init-game)
(js/setInterval game-loop (/ 1000 game-fps))
