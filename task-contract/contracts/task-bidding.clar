
;; title: task-bidding
;; version:
;; summary:
;; description:


;; title: task-bidding
;; version:
;; summary:
;; description:

;; title: task-bidding
;; version:
;; summary:
;; description:

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-task-not-found (err u101))
(define-constant err-bidding-closed (err u102))
(define-constant err-invalid-bid (err u103))
(define-constant err-no-bids (err u104))

;; Define data maps
(define-map tasks
  { task-id: uint }
  {
    client: principal,
    description: (string-ascii 256),
    budget: uint,
    deadline: uint,
    status: (string-ascii 20),
    winning-bid: (optional uint)
  }
)

(define-map bids
  { task-id: uint, bidder: principal }
  {
    amount: uint,
    estimated-time: uint
  }
)

;; Define functions

;; Create a new task
(define-public (create-task (description (string-ascii 256)) (budget uint) (deadline uint))
  (let ((task-id (+ (var-get task-counter) u1)))
    (map-set tasks
      { task-id: task-id }
      {
        client: tx-sender,
        description: description,
        budget: budget,
        deadline: deadline,
        status: "open",
        winning-bid: none
      }
    )
    (var-set task-counter task-id)
    (ok task-id)
  )
)

;; Submit a bid for a task
(define-public (submit-bid (task-id uint) (amount uint) (estimated-time uint))
  (let ((task (unwrap! (map-get? tasks { task-id: task-id }) err-task-not-found)))
    (asserts! (is-eq (get status task) "open") err-bidding-closed)
    (asserts! (<= amount (get budget task)) err-invalid-bid)
    (map-set bids
      { task-id: task-id, bidder: tx-sender }
      { amount: amount, estimated-time: estimated-time }
    )
    (ok true)
  )
)

;; Select a winning bid (only the task client can do this)
(define-public (select-winning-bid (task-id uint) (winning-bidder uint) (bidder principal))
  (let (
    (task (unwrap! (map-get? tasks { task-id: task-id }) err-task-not-found))
    (winning-bid (unwrap! (map-get? bids { task-id: task-id, bidder: bidder }) err-no-bids))
  )
    (asserts! (is-eq tx-sender (get client task)) err-not-authorized)
    (asserts! (is-eq (get status task) "open") err-bidding-closed)
    (map-set tasks
      { task-id: task-id }
      (merge task {
        status: "in-progress",
        winning-bid: (some winning-bidder)
      })
    )
    (ok true)
  )
)

;; Release funds to the winning bidder (only the task client can do this)
(define-public (release-funds (task-id uint))
  (let (
    (task (unwrap! (map-get? tasks { task-id: task-id }) err-task-not-found))
    (winning-bidder (get client task))
    (winning-bid (unwrap! (map-get? bids { task-id: task-id, bidder: winning-bidder }) err-no-bids))
  )
    (asserts! (is-eq tx-sender (get client task)) err-not-authorized)
    (asserts! (is-eq (get status task) "in-progress") err-not-authorized)
    (try! (stx-transfer? (get amount winning-bid) tx-sender winning-bidder))
    (map-set tasks
      { task-id: task-id }
      (merge task { status: "completed" })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get task details
(define-read-only (get-task (task-id uint))
  (map-get? tasks { task-id: task-id })
)

;; Get bid details
(define-read-only (get-bid (task-id uint) (bidder principal))
  (map-get? bids { task-id: task-id, bidder: bidder })
)

;; Initialize task counter
(define-data-var task-counter uint u0)
