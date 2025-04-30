
;; title: Task-creation
;; version:
;; summary:
;; description:

;; Task Creation Smart Contract

;; Define constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-TASK-NOT-FOUND (err u101))
(define-constant ERR-INVALID-TASK (err u102))
(define-constant ERR-TASK-ALREADY-EXISTS (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))

;; Define task status
(define-constant TASK-STATUS-CREATED u0)
(define-constant TASK-STATUS-ASSIGNED u1)
(define-constant TASK-STATUS-IN-PROGRESS u2)
(define-constant TASK-STATUS-COMPLETED u3)
(define-constant TASK-STATUS-DISPUTED u4)

;; Task structure
(define-map tasks
  {task-id: uint}
  {
    creator: principal,
    description: (string-utf8 500),
    price: uint,
    deadline: uint,
    status: uint,
    assigned-provider: (optional principal)
  }
)

;; Track task IDs
(define-data-var next-task-id uint u0)

;; Create a new task
(define-public (create-task 
  (description (string-utf8 500))
  (price uint)
  (deadline uint)
)
  (let 
    (
      (task-id (var-get next-task-id))
      (new-task {
        creator: tx-sender,
        description: description,
        price: price,
        deadline: deadline,
        status: TASK-STATUS-CREATED,
        assigned-provider: none
      })
    )
    ;; Validate inputs
    (asserts! (> price u0) ERR-INVALID-TASK)
    (asserts! (> deadline stacks-block-height) ERR-INVALID-TASK)
    
    ;; Ensure sufficient funds are available
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    
    ;; Store the task
    (map-set tasks {task-id: task-id} new-task)
    
    ;; Increment task ID
    (var-set next-task-id (+ task-id u1))
    
    ;; Return the task ID
    (ok task-id)
))

;; Assign a task to a service provider
(define-public (assign-task 
  (task-id uint)
)
  (let 
    (
      (task (unwrap! (map-get? tasks {task-id: task-id}) ERR-TASK-NOT-FOUND))
    )
    ;; Ensure only unassigned tasks can be assigned
    (asserts! 
      (is-eq (get status task) TASK-STATUS-CREATED) 
      ERR-UNAUTHORIZED
    )
    
    ;; Update task with assigned provider
    (map-set tasks 
      {task-id: task-id} 
      (merge task {
        status: TASK-STATUS-ASSIGNED,
        assigned-provider: (some tx-sender)
      })
    )
    
    (ok true)
))

;; Mark task as in progress
(define-public (start-task 
  (task-id uint)
)
  (let 
    (
      (task (unwrap! (map-get? tasks {task-id: task-id}) ERR-TASK-NOT-FOUND))
    )
    ;; Ensure only assigned provider can start the task
    (asserts! 
      (and 
        (is-eq (get status task) TASK-STATUS-ASSIGNED)
        (is-eq (get assigned-provider task) (some tx-sender))
      )
      ERR-UNAUTHORIZED
    )
    
    ;; Update task status
    (map-set tasks 
      {task-id: task-id} 
      (merge task {status: TASK-STATUS-IN-PROGRESS})
    )
    
    (ok true)
))

;; Complete a task
(define-public (complete-task 
  (task-id uint)
)
  (let 
    (
      (task (unwrap! (map-get? tasks {task-id: task-id}) ERR-TASK-NOT-FOUND))
      (task-price (get price task))
    )
    ;; Ensure only assigned provider can complete the task
    (asserts! 
      (and 
        (is-eq (get status task) TASK-STATUS-IN-PROGRESS)
        (is-eq (get assigned-provider task) (some tx-sender))
      )
      ERR-UNAUTHORIZED
    )
    
    ;; Transfer payment to the provider
    (try! 
      (as-contract 
        (stx-transfer? 
          task-price 
          tx-sender 
          (unwrap! (get assigned-provider task) ERR-UNAUTHORIZED)
        )
      )
    )
    
    ;; Update task status
    (map-set tasks 
      {task-id: task-id} 
      (merge task {status: TASK-STATUS-COMPLETED})
    )
    
    (ok true)
))

;; Dispute a task
(define-public (dispute-task 
  (task-id uint)
)
  (let 
    (
      (task (unwrap! (map-get? tasks {task-id: task-id}) ERR-TASK-NOT-FOUND))
    )
    ;; Ensure only task creator or assigned provider can dispute
    (asserts! 
      (or 
        (is-eq tx-sender (get creator task))
        (is-eq (get assigned-provider task) (some tx-sender))
      )
      ERR-UNAUTHORIZED
    )
    
    ;; Update task status
    (map-set tasks 
      {task-id: task-id} 
      (merge task {status: TASK-STATUS-DISPUTED})
    )
    
    (ok true)
))

;; Resolve a disputed task (can only be called by contract owner)
(define-public (resolve-dispute 
  (task-id uint)
  (provider-refund bool)
)
  (let 
    (
      (task (unwrap! (map-get? tasks {task-id: task-id}) ERR-TASK-NOT-FOUND))
      (task-price (get price task))
      (provider (unwrap! (get assigned-provider task) ERR-UNAUTHORIZED))
    )
    ;; Ensure only contract owner can resolve disputes
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    
    ;; Ensure task is in disputed state
    (asserts! (is-eq (get status task) TASK-STATUS-DISPUTED) ERR-UNAUTHORIZED)
    
    ;; Resolve dispute by refunding or paying provider
    (if provider-refund
      ;; Refund to task creator
      (try! 
        (as-contract 
          (stx-transfer? 
            task-price 
            tx-sender 
            (get creator task)
          )
        )
      )
      ;; Pay provider
      (try! 
        (as-contract 
          (stx-transfer? 
            task-price 
            tx-sender 
            provider
          )
        )
      )
    )
    
    ;; Update task status to completed
    (map-set tasks 
      {task-id: task-id} 
      (merge task {status: TASK-STATUS-COMPLETED})
    )
    
    (ok true)
  ))

;; Read task details
(define-read-only (get-task-details 
  (task-id uint)
)
  (map-get? tasks {task-id: task-id})
)