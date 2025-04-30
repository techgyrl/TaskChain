;; title: escrow-payment
;; version: 1.0.0
;; summary: A smart contract for managing escrow payments between clients and freelancers
;; description: This contract facilitates secure transactions between clients and freelancers,
;;              ensuring that payment is only released after the client confirms the task completion.

;; Error codes
(define-constant ERR-NOT-INITIALIZED (err u100))
(define-constant ERR-ALREADY-INITIALIZED (err u101))
(define-constant ERR-UNAUTHORIZED (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-WRONG-STATE (err u104))
(define-constant ERR-TRANSFER-FAILED (err u105))

;; Contract states
(define-constant STATE-EMPTY u0)
(define-constant STATE-INITIALIZED u1)
(define-constant STATE-FUNDED u2)
(define-constant STATE-SUBMITTED u3)
(define-constant STATE-COMPLETED u4)

;; Define the data structures
(define-data-var contract-state uint STATE-EMPTY)
(define-data-var client principal tx-sender)
(define-data-var freelancer principal tx-sender)
(define-data-var payment uint u0)
(define-data-var contract-start-block uint u0)
(define-data-var deadline-blocks uint u0)

;; Getters for contract data
(define-read-only (get-contract-state)
  (var-get contract-state))

(define-read-only (get-client)
  (var-get client))

(define-read-only (get-freelancer)
  (var-get freelancer))

(define-read-only (get-payment-amount)
  (var-get payment))

(define-read-only (get-contract-start-block)
  (var-get contract-start-block))

(define-read-only (get-deadline-blocks)
  (var-get deadline-blocks))

(define-read-only (get-deadline)
  (+ (var-get contract-start-block) (var-get deadline-blocks)))

(define-read-only (is-past-deadline)
  (> stacks-block-height (get-deadline)))

;; Function to initialize the contract
(define-public (initialize (client-principal principal) (freelancer-principal principal) (amount uint) (duration uint))
  (begin
    (asserts! (is-eq (var-get contract-state) STATE-EMPTY) (err ERR-ALREADY-INITIALIZED))
    (asserts! (> amount u0) (err ERR-INVALID-AMOUNT))
    (asserts! (> duration u0) (err ERR-INVALID-AMOUNT))
    (asserts! (not (is-eq client-principal freelancer-principal)) (err ERR-INVALID-AMOUNT))
    
    (var-set client client-principal)
    (var-set freelancer freelancer-principal)
    (var-set payment amount)
    (var-set contract-start-block stacks-block-height)
    (var-set deadline-blocks duration)
    (var-set contract-state STATE-INITIALIZED)
    
    (ok true)
  ))

;; Function for the client to deposit the payment
(define-public (deposit-payment)
  (let ((amount (var-get payment)))
    (begin
      (asserts! (is-eq (var-get contract-state) STATE-INITIALIZED) (err ERR-WRONG-STATE))
      (asserts! (is-eq tx-sender (var-get client)) (err ERR-UNAUTHORIZED))
      (asserts! (> amount u0) (err ERR-INVALID-AMOUNT))
      
      (match (stx-transfer? amount tx-sender (as-contract tx-sender))
        success (begin
          (var-set contract-state STATE-FUNDED)
          (ok true)
        )
        error (err ERR-TRANSFER-FAILED)
      )
    ))
)

;; Function for the freelancer to submit the task
(define-public (submit-task)
  (begin
    (asserts! (> (var-get contract-state) STATE-EMPTY) (err ERR-NOT-INITIALIZED))
    (asserts! (is-eq (var-get contract-state) STATE-FUNDED) (err ERR-WRONG-STATE))
    (asserts! (is-eq tx-sender (var-get freelancer)) (err ERR-UNAUTHORIZED))
    (asserts! (not (is-past-deadline)) (err ERR-WRONG-STATE))
    
    (var-set contract-state STATE-SUBMITTED)
    (ok true)
  ))

;; Function for the client to confirm task completion
(define-public (confirm-task-completion)
  (begin
    (asserts! (> (var-get contract-state) STATE-EMPTY) (err ERR-NOT-INITIALIZED))
    (asserts! (is-eq (var-get contract-state) STATE-SUBMITTED) (err ERR-WRONG-STATE))
    (asserts! (is-eq tx-sender (var-get client)) (err ERR-UNAUTHORIZED))
    
    (var-set contract-state STATE-COMPLETED)
    (release-payment)
  ))

;; Function to release the payment to the freelancer
(define-private (release-payment)
  (let ((amount (var-get payment))
        (freelancer-addr (var-get freelancer)))
    (begin
      (asserts! (is-eq (var-get contract-state) STATE-COMPLETED) (err ERR-WRONG-STATE))
      
      (match (as-contract (stx-transfer? amount tx-sender freelancer-addr))
        success (begin
          (var-set contract-state STATE-EMPTY)
          (var-set payment u0)
          (ok true)
        )
        error (err ERR-TRANSFER-FAILED)
      )
    ))
)

;; Function for the client to cancel the contract and get refunded before submission
(define-public (cancel-by-client)
  (let ((amount (var-get payment))
        (client-addr (var-get client)))
    (begin
      (asserts! (> (var-get contract-state) STATE-EMPTY) (err ERR-NOT-INITIALIZED))
      (asserts! (< (var-get contract-state) STATE-SUBMITTED) (err ERR-WRONG-STATE))
      (asserts! (is-eq tx-sender client-addr) (err ERR-UNAUTHORIZED))
      
      (match (as-contract (stx-transfer? amount tx-sender client-addr))
        success (begin
          (var-set contract-state STATE-EMPTY)
          (var-set payment u0)
          (ok true)
        )
        error (err ERR-TRANSFER-FAILED)
      )
    ))
)

;; Function for the freelancer to claim payment if deadline has passed without client action
(define-public (claim-payment-after-deadline)
  (let ((amount (var-get payment))
        (freelancer-addr (var-get freelancer)))
    (begin
      (asserts! (is-eq (var-get contract-state) STATE-SUBMITTED) (err ERR-WRONG-STATE))
      (asserts! (is-eq tx-sender freelancer-addr) (err ERR-UNAUTHORIZED))
      (asserts! (is-past-deadline) (err ERR-WRONG-STATE))
      
      (match (as-contract (stx-transfer? amount tx-sender freelancer-addr))
        success (begin
          (var-set contract-state STATE-EMPTY)
          (var-set payment u0)
          (ok true)
        )
        error (err ERR-TRANSFER-FAILED)
      )
    ))
)