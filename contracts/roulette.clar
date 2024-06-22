;; title: roulette
;; version: 1.0.0
;; summary: A decentralized roulette game on the Stacks blockchain.
;; description: This smart contract implements a simple roulette game where users can place bets and spin the wheel to win or lose based on the outcome.

;; traits
;;

;; token definitions
;;

;; constants
(define-constant MIN_BET_AMOUNT u10) ;; Minimum bet amount in microSTX
(define-constant MAX_BET_AMOUNT u1000) ;; Maximum bet amount in microSTX
(define-constant WHEEL_SIZE u37) ;; Size of the roulette wheel (0-36)

;; error codes
(define-constant ERR_INVALID_BET_AMOUNT u100)
(define-constant ERR_INVALID_BET_NUMBER u101)
(define-constant ERR_NO_BETS_PLACED u102)
(define-constant ERR_TRANSFER_FAILED u103)

;; data vars
(define-data-var pot uint u0) ;; The total pot of the game
(define-data-var last-spin uint u0) ;; The result of the last spin

;; data maps
(define-map bets principal (tuple (amount uint) (number uint))) ;; Map to store bets placed by users

;; public functions

;; Function to place a bet
(define-public (place-bet (bet-amount uint) (number uint))
  (begin
    ;; Ensure the bet amount is within the allowed range
    (asserts! (and (>= bet-amount MIN_BET_AMOUNT) (<= bet-amount MAX_BET_AMOUNT)) (err ERR_INVALID_BET_AMOUNT))
    ;; Ensure the bet number is within the wheel size
    (asserts! (< number WHEEL_SIZE) (err ERR_INVALID_BET_NUMBER))
    ;; Transfer the bet amount to the contract
    (try! (stx-transfer? bet-amount tx-sender (as-contract tx-sender)) (err ERR_TRANSFER_FAILED))
    ;; Store the bet
    (map-insert bets tx-sender (tuple (amount bet-amount) (number number)))
    ;; Update the pot
    (var-set pot (+ (var-get pot) bet-amount))
    (ok (tuple (bet-amount bet-amount) (number number)))
  )
)

;; Function to spin the wheel
(define-public (spin-wheel)
  (begin
    ;; Ensure there are bets placed
    (asserts! (> (var-get pot) u0) (err ERR_NO_BETS_PLACED))
    ;; Generate a pseudo-random number using block data and user input
    (let ((winning-number (mod (+ (block-height) (block-header-hash)) WHEEL_SIZE)))
      ;; Update the last spin result
      (var-set last-spin winning-number)
      ;; Calculate and distribute payouts
      (try! (distribute-payouts winning-number))
      (ok winning-number)
    )
  )
)

;; read only functions

;; Function to get the current pot
(define-read-only (get-pot)
  (ok (var-get pot))
)

;; Function to get the last spin result
(define-read-only (get-last-spin)
  (ok (var-get last-spin))
)

;; private functions

;; Function to distribute payouts based on the winning number
(define-private (distribute-payouts (winning-number uint))
  (let ((total-payout u0))
    (map-iter
      (lambda (user bet)
        (let ((bet-amount (get amount bet))
              (bet-number (get number bet))
              (payout (if (== winning-number bet-number)
                          (* bet-amount u36) ;; Payout 36x the bet amount for a win
                          u0)))
          ;; Transfer the payout to the user
          (try! (stx-transfer? payout (as-contract tx-sender) user) (err ERR_TRANSFER_FAILED))
          ;; Update the total payout
          (set total-payout (+ total-payout payout))
        )
      )
      bets
    )
    ;; Update the pot after payouts
    (var-set pot (- (var-get pot) total-payout))
    (ok total-payout)
  )
)