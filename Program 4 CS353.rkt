#lang racket

(struct location (des exts items search?) #:transparent)
(struct item (name desc visible?) #:transparent)
(struct current-state (location-n locations inventory) #:transparent)

(define (create-item name des)
  (item name des #f))

(define (visible-items items) (filter item-visible? items))

(define cave-entrance
  (location "You are standing at the entrance to a cave, facing North. To the East, there is a narrow tunnel."
            '((north . deep-cave) (east . tunnel))
            (list (create-item 'key "a small rusty key"))
            #f))

(define cave-inside
  (location "You are inside the cave. There is something shiny buried in the ground."
            '((south . cave-entrance) (west . hidden-room))
            (list (create-item 'gold "a shiny piece of gold"))
            #f))

(define tunnel
  (location "You are in a humid tunnel with mildew dripping walls. A rocky path leads downward to the South."
            '((west . cave-entrance) (south . lower-level))
            (list (create-item 'map "a old ripped up map"))
            #f))

(define cave-lower-level
  (location "You're at the lower level of the cave. There is a slight breeze and a rope on the wall heading up."
            '((north . tunnel))
            (list (create-item 'lantern "an old busted lantern"))
            #f))

(define secret-room
  (location "You found a secret room with a locked chest. Maybe you need a key."
            '((east . deep-cave))
            empty
            #f))

(define initial-state
  (current-state 'cave-entrance
              (hash 'cave-entrance cave-entrance
                    'cave-inside cave-inside
                    'tunnel tunnel
                    'cave-lower-level cave-lower-level
                    'secret-room secret-room)
              empty))

(define (curr-location state)
  (hash-ref (current-state-locations state) (current-state-location-n state)))

(define (updt-location state new-loc)
  (let* ([locs (current-state-locations state)]
         [location-name (current-state-location-n state)])
    (current-state location-name (hash-set locs location-name new-loc) (current-state-inventory state))))

(define (desc-location state)
  (let* ([loc (curr-location state)])
    (if (location-search? loc)
        (string-append "You are currently at " (symbol->string (current-state-location-n state)) ".")
        (location-des loc))))

(define (list-items items)
  (if (null? items)
      "No items here."
      (string-join (map (λ (i) (symbol->string (item-name i))) items) ", ")))

(define (get-location direction state)
  (let* ([loc (curr-location state)]
         [exit (assoc direction (location-exts loc))])
    (if exit
        (current-state (cdr exit) (current-state-locations state) (current-state-inventory state))
        (begin
          (printf "You can't go ~a from here.\n" direction)
          state))))

(define (get-search state)
  (let* ([loc (curr-location state)]
         [new-items (map (λ (i) (item (item-name i) (item-desc i) #t)) (location-items loc))]
         [new-loc (location (location-des loc) (location-exts loc) new-items #t)])
    (begin
      (printf "You just found: ~a\n" (list-items new-items))
      (updt-location state new-loc))))

(define (get-item item-name state)
  (let* ([loc (curr-location state)]
         [visible (filter (λ (i) (and (eq? (item-name i) item-name) (item-visible? i)))
                          (location-items loc))])
    (if (null? visible)
        (begin (printf "You can't take this.\n") state)
        (let* ([item (car visible)]
               [new-loc (location (location-des loc)
                                  (location-exts loc)
                                  (remove item (location-items loc))
                                  (location-search? loc))]
               [new-inventory (cons item (current-state-inventory state))])
          (begin
            (printf "You take the ~a.\n" item-name)
            (current-state (current-state-location-n state)
                        (hash-set (current-state-locations state)
                                  (current-state-location-n state) new-loc)
                        new-inventory))))))

(define (get-drop item-name state)
  (let* ([invent (current-state-inventory state)]
         [match (filter (λ (i) (eq? (item-name i) item-name)) invent)])
    (if (null? match)
        (begin (printf "You currently do not possess this item.\n") state)
        (let* ([item (car match)]
               [loc (curr-location state)]
               [new-loc (location (location-des loc)
                                  (location-exts loc)
                                  (cons item (location-items loc))
                                  (location-search? loc))]
               [new-inv (remove item invent)])
          (begin
            (printf "You get rid of ~a.\n" item-name)
            (current-state (current-state-location-n state)
                        (hash-set (current-state-locations state)
                                  (current-state-location-n state) new-loc)
                        new-inv))))))

(define (get-inventory state)
  (let ([inv (current-state-inventory state)])
    (if (null? inv)
        (printf "Inventory is currently empty.\n")
        (printf "Inventory: ~a\n" (list-items inv))))
  state)

(define (get-desc state)
  (printf "~a\n" (location-des (curr-location state)))
  state)

(define (get-help state)
  (printf "Current commands available: north, south, east, west, search, take [item], drop [item], inventory, describe, help, quit\n")
  (get-inventory state)
  (get-desc state)
  state)

(define (parse-inp input state)
  (match (string-split input)
    [(list "north") (get-location 'north state)]
    [(list "south") (get-location 'south state)]
    [(list "east")  (get-location 'east state)]
    [(list "west")  (get-location 'west state)]
    [(list "search") (get-search state)]
    [(list "describe") (get-desc state)]
    [(list "inventory") (get-inventory state)]
    [(list "help") (get-help state)]
    [(list "quit") (begin (printf "Thank you for playing the game!\n") (exit))]
    [(list "take" name) (get-item (string->symbol name) state)]
    [(list "drop" name) (get-drop (string->symbol name) state)]
    [_ (begin (printf "Unknown command.\n") (get-desc state)) state]))

(define (begin-game state)
  (printf "\n> ")
  (flush-output)
  (let* ([input (read-line)]
         [new-state (parse-inp input state)])
    (begin-game new-state)))

(printf "You are about to start an Adventure through my cave!\n")
(printf "~a\n" (desc-location initial-state))
(begin-game initial-state)
