; CS-353 Project 4 / Ryan Lee / 5-5-24 / Made in conjuncture with ChatGPT for the creation, explanation, and debugging of code
#lang racket

; Define initial player inventory and grocery items
(define player-inventory '())
(define grocery-items '(("Produce" ("apple" "banana" "tomato"))
                        ("Snack Aisle" ("chips" "cookies" "candy"))
                        ("Meat & Dairy" ("milk" "cheese" "chicken"))))

; Function to display checkout section
(define (checkout player-inventory)
  (displayln "You are at the Checkout.")
  (displayln "You see the cashier and some magazines.")
  ; Check if the player has collected enough items to win
  (if (>= (length player-inventory) 4)
      (begin
        (displayln "Congratulations! You've successfully completed your shopping!")
        (displayln "You win!")
        (exit))
      ; If not, display options to continue shopping
      (display-options '("Produce" "Snack Aisle" "Meat & Dairy" "Search" "Take [Item]" "Drop [Item]" "Check Inventory" "Describe Area" "Help") "Checkout" player-inventory)))

; Function to display produce section
(define (produce player-inventory)
  (displayln "You are in the Produce section.")
  (displayln "You see lots of fruits and vegetables.")
  ; Display options for the produce section
  (display-options '("Checkout" "Snack Aisle" "Meat & Dairy" "Search" "Take [Item]" "Drop [Item]" "Check Inventory" "Describe Area" "Help") "Produce" player-inventory))

; Function to display snack aisle section
(define (snack-aisle player-inventory)
  (displayln "You are in the Snack Aisle.")
  (displayln "You see chips, cookies, and candy.")
  ; Display options for the snack aisle section
  (display-options '("Checkout" "Produce" "Meat & Dairy" "Search" "Take [Item]" "Drop [Item]" "Check Inventory" "Describe Area" "Help") "Snack Aisle" player-inventory))

; Function to display meat & dairy section
(define (meat-dairy player-inventory)
  (displayln "You are in the Meat & Dairy section.")
  (displayln "You see milk, cheese, and various meats.")
  ; Display options for the meat & dairy section
  (display-options '("Checkout" "Produce" "Snack Aisle" "Search" "Take [Item]" "Drop [Item]" "Check Inventory" "Describe Area" "Help") "Meat & Dairy" player-inventory))

; Function to display player inventory
(define (check-inventory player-inventory)
  (display "Inventory: ")
  ; Check if the inventory is empty
  (if (empty? player-inventory)
      (displayln "Your inventory is empty.")
      ; If not, display items in the inventory
      (begin
        (for-each (lambda (item)
                    (display "[")(display item)(display "] "))
                  player-inventory)
        (newline))))

; Function to take an item and add it to the player inventory
(define (take-item item location player-inventory)
  (displayln (string-append "You took the " item "."))
  ; Add the item to the inventory
  (cons item player-inventory))

; Function to drop an item from the player inventory
(define (drop-item item player-inventory)
  ; Check if the item is in the inventory
  (if (member item player-inventory)
      ; If found, remove the item from the inventory
      (begin
        (displayln (string-append "You dropped the " item "."))
        (filter (lambda (x) (not (equal? x item))) player-inventory))
      ; If not found, display a message
      (begin
        (displayln (string-append "You don't have " item " in your inventory."))
        player-inventory)))

; Function to search a location for items
(define (search-location location player-inventory)
  (displayln "You search the area...")
  ; Check if the location is the checkout
  (if (equal? location "Checkout")
      (displayln "You find nothing interesting.")
      ; If not, display items found in the location
      (begin
        (displayln "You find the following items:")
        (display-items location)
        player-inventory))
  (displayln "You finish searching.")
  location)

; Function to display items in a section
(define (display-items section)
  ; Retrieve items for the specified section
  (let ((items (cdr (assoc section grocery-items))))
    ; Display each item
    (for-each (lambda (item)
                (displayln (string-append "- " (format "~a" item))))
              items)))

; Function to describe a location
(define (describe-area location)
  ; Display description based on location
  (case location
    (("Produce") (displayln "Produce section - a land of greens, beans, and no teens."))
    (("Snack Aisle") (displayln "Snack Aisle - where all your sugary salty cravings lie."))
    (("Meat & Dairy") (displayln "Meat & Dairy - muscle gain and tummy pain."))
    (("Checkout") (displayln "Checkout - return with at least four items and win!"))))

; Function to display options for the player
(define (display-options options location player-inventory)
  (display "What do you want to do? ")
  ; Display available options
  (for-each (lambda (option)
              (display "[")(display option)(display "] "))
            options)
  (newline)
  ; Read player choice
  (let ((choice (string-downcase (read-line))))
    ; Process player choice
    (case choice
      (("checkout") (checkout player-inventory))
      (("produce") (produce player-inventory))
      (("snack aisle") (snack-aisle player-inventory))
      (("meat & dairy") (meat-dairy player-inventory))
      (("meat and dairy") (meat-dairy player-inventory))
      (("check inventory") (check-inventory player-inventory) (display-options '("Continue Exploring") location player-inventory))
      (("search") (display-options options (search-location location player-inventory) player-inventory))
      (("describe area") (describe-area location) (display-options '("Continue Exploring") location player-inventory))
      (("help") (show-help location player-inventory))
      (("continue exploring") (checkout player-inventory))
      ; Handle item actions
      (("take apple") (let ((new-inventory (take-item "apple" location player-inventory)))
                        (display-options options location new-inventory)))
      (("take banana") (let ((new-inventory (take-item "banana" location player-inventory)))
                          (display-options options location new-inventory)))
      (("take tomato") (let ((new-inventory (take-item "tomato" location player-inventory)))
                          (display-options options location new-inventory)))
      (("take chips") (let ((new-inventory (take-item "chips" location player-inventory)))
                        (display-options options location new-inventory)))
      (("take cookies") (let ((new-inventory (take-item "cookies" location player-inventory)))
                          (display-options options location new-inventory)))
      (("take candy") (let ((new-inventory (take-item "candy" location player-inventory)))
                        (display-options options location new-inventory)))
      (("take milk") (let ((new-inventory (take-item "milk" location player-inventory)))
                        (display-options options location new-inventory)))
      (("take cheese") (let ((new-inventory (take-item "cheese" location player-inventory)))
                          (display-options options location new-inventory)))
      (("take chicken") (let ((new-inventory (take-item "chicken" location player-inventory)))
                          (display-options options location new-inventory)))
      (("drop apple") (display-options options location (drop-item "apple" player-inventory)))
      (("drop banana") (display-options options location (drop-item "banana" player-inventory)))
      (("drop tomato") (display-options options location (drop-item "tomato" player-inventory)))
      (("drop chips") (display-options options location (drop-item "chips" player-inventory)))
      (("drop cookies") (display-options options location (drop-item "cookies" player-inventory)))
      (("drop candy") (display-options options location (drop-item "candy" player-inventory)))
      (("drop milk") (display-options options location (drop-item "milk" player-inventory)))
      (("drop cheese") (display-options options location (drop-item "cheese" player-inventory)))
      (("drop chicken") (display-options options location (drop-item "chicken" player-inventory)))
      (else (displayln "Invalid choice. Try again.") (display-options options location player-inventory)))))

; Function to display help information
(define (show-help location player-inventory)
  (displayln (string-append "Location: " location))
  (check-inventory player-inventory)
  (display-options '("Continue Exploring") location player-inventory))

; Function to start the game
(define (start)
  (displayln "Welcome to the Grocery Store Adventure!")
  ; Start the game loop
  (let loop ((player-inventory player-inventory))
    (let ((next-action (checkout player-inventory)))
      (loop next-action))))

; Start the game
(start)
