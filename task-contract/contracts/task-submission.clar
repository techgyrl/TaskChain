;; Task Completion & Submission Smart Contract
;; Handles freelancer task submissions, client reviews, and payment escrow

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_TASK_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u103))
(define-constant ERR_ALREADY_REVIEWED (err u104))
(define-constant ERR_REVIEW_PERIOD_EXPIRED (err u105))
(define-constant ERR_TASK_ALREADY_SUBMITTED (err u106))

;; Task status definitions
(define-constant STATUS_CREATED u0)
(define-constant STATUS_IN_PROGRESS u1)
(define-constant STATUS_SUBMITTED u2)
(define-constant STATUS_APPROVED u3)
(define-constant STATUS_REJECTED u4)
(define-constant STATUS_DISPUTED u5)

;; Review period (in blocks) - approximately 7 days
(define-constant REVIEW_PERIOD u1008)

;; Data structures
(define-map tasks
  { task-id: uint }
  {
    client: principal,
    freelancer: principal,
    payment-amount: uint,
    status: uint,
    submission-data: (string-utf8 500),
    submission-block: uint,
    review-deadline: uint,
    created-at: uint
  }
)

(define-map task-reviews
  { task-id: uint }
  {
    reviewer: principal,
    approved: bool,
    feedback: (string-utf8 500),
    reviewed-at: uint
  }
)

;; Data variables
(define-data-var next-task-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%

;; Read-only functions
(define-read-only (get-task (task-id uint))
  (map-get? tasks { task-id: task-id })
)

(define-read-only (get-task-review (task-id uint))
  (map-get? task-reviews { task-id: task-id })
)

(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)

(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u10000)
)

(define-read-only (is-review-period-active (task-id uint))
  (match (get-task task-id)
    task-data 
      (and 
        (is-eq (get status task-data) STATUS_SUBMITTED)
        (<= block-height (get review-deadline task-data))
      )
    false
  )
)

;; Private functions
(define-private (is-task-client (task-id uint) (user principal))
  (match (get-task task-id)
    task-data (is-eq (get client task-data) user)
    false
  )
)

(define-private (is-task-freelancer (task-id uint) (user principal))
  (match (get-task task-id)
    task-data (is-eq (get freelancer task-data) user)
    false
  )
)

;; Public functions

;; Create a new task with payment escrow
(define-public (create-task (freelancer principal) (payment-amount uint))
  (let 
    (
      (task-id (var-get next-task-id))
    )
    (asserts! (> payment-amount u0) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer payment to contract for escrow
    (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
    
    ;; Create task record
    (map-set tasks
      { task-id: task-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        payment-amount: payment-amount,
        status: STATUS_CREATED,
        submission-data: u"",
        submission-block: u0,
        review-deadline: u0,
        created-at: block-height
      }
    )
    
    ;; Increment task ID counter
    (var-set next-task-id (+ task-id u1))
    
    (ok task-id)
  )
)

;; Start working on a task
(define-public (start-task (task-id uint))
  (let 
    (
      (task-data (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND))
    )
    (asserts! (is-task-freelancer task-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status task-data) STATUS_CREATED) ERR_INVALID_STATUS)
    
    ;; Update task status to in progress
    (map-set tasks
      { task-id: task-id }
      (merge task-data { status: STATUS_IN_PROGRESS })
    )
    
    (ok true)
  )
)

;; Submit completed task for review
(define-public (submit-task (task-id uint) (submission-data (string-utf8 500)))
  (let 
    (
      (task-data (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND))
      (review-deadline (+ block-height REVIEW_PERIOD))
    )
    (asserts! (is-task-freelancer task-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status task-data) STATUS_IN_PROGRESS) ERR_INVALID_STATUS)
    
    ;; Update task with submission details
    (map-set tasks
      { task-id: task-id }
      (merge task-data 
        {
          status: STATUS_SUBMITTED,
          submission-data: submission-data,
          submission-block: block-height,
          review-deadline: review-deadline
        }
      )
    )
    
    ;; Print event for client notification
    (print {
      event: "task-submitted",
      task-id: task-id,
      freelancer: tx-sender,
      client: (get client task-data),
      submission-data: submission-data,
      review-deadline: review-deadline
    })
    
    (ok true)
  )
)

;; Client reviews and approves/rejects the task
(define-public (review-task (task-id uint) (approved bool) (feedback (string-utf8 500)))
  (let 
    (
      (task-data (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND))
      (platform-fee (calculate-platform-fee (get payment-amount task-data)))
      (freelancer-payment (- (get payment-amount task-data) platform-fee))
    )
    (asserts! (is-task-client task-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status task-data) STATUS_SUBMITTED) ERR_INVALID_STATUS)
    (asserts! (is-none (get-task-review task-id)) ERR_ALREADY_REVIEWED)
    (asserts! (<= block-height (get review-deadline task-data)) ERR_REVIEW_PERIOD_EXPIRED)
    
    ;; Record the review
    (map-set task-reviews
      { task-id: task-id }
      {
        reviewer: tx-sender,
        approved: approved,
        feedback: feedback,
        reviewed-at: block-height
      }
    )
    
    (if approved
      (begin
        ;; Approve: Release payment to freelancer
        (try! (as-contract (stx-transfer? freelancer-payment tx-sender (get freelancer task-data))))
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
        
        ;; Update task status
        (map-set tasks
          { task-id: task-id }
          (merge task-data { status: STATUS_APPROVED })
        )
        
        (print {
          event: "task-approved",
          task-id: task-id,
          freelancer: (get freelancer task-data),
          payment: freelancer-payment
        })
      )
      (begin
        ;; Reject: Update status for potential dispute resolution
        (map-set tasks
          { task-id: task-id }
          (merge task-data { status: STATUS_REJECTED })
        )
        
        (print {
          event: "task-rejected",
          task-id: task-id,
          freelancer: (get freelancer task-data),
          feedback: feedback
        })
      )
    )
    
    (ok approved)
  )
)

;; Auto-approve task if review period expires without review
(define-public (auto-approve-task (task-id uint))
  (let 
    (
      (task-data (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND))
      (platform-fee (calculate-platform-fee (get payment-amount task-data)))
      (freelancer-payment (- (get payment-amount task-data) platform-fee))
    )
    (asserts! (is-eq (get status task-data) STATUS_SUBMITTED) ERR_INVALID_STATUS)
    (asserts! (> block-height (get review-deadline task-data)) (err u107)) ;; Review period not expired
    (asserts! (is-none (get-task-review task-id)) ERR_ALREADY_REVIEWED)
    
    ;; Auto-approve and release payment
    (try! (as-contract (stx-transfer? freelancer-payment tx-sender (get freelancer task-data))))
    (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
    
    ;; Update task status
    (map-set tasks
      { task-id: task-id }
      (merge task-data { status: STATUS_APPROVED })
    )
    
    ;; Record auto-approval
    (map-set task-reviews
      { task-id: task-id }
      {
        reviewer: (get client task-data),
        approved: true,
        feedback: u"Auto-approved due to review timeout",
        reviewed-at: block-height
      }
    )
    
    (print {
      event: "task-auto-approved",
      task-id: task-id,
      freelancer: (get freelancer task-data),
      payment: freelancer-payment
    })
    
    (ok true)
  )
)

;; Initiate dispute resolution
(define-public (initiate-dispute (task-id uint) (dispute-reason (string-utf8 500)))
  (let 
    (
      (task-data (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND))
    )
    (asserts! 
      (or 
        (is-task-client task-id tx-sender)
        (is-task-freelancer task-id tx-sender)
      ) 
      ERR_UNAUTHORIZED
    )
    (asserts! (is-eq (get status task-data) STATUS_REJECTED) ERR_INVALID_STATUS)
    
    ;; Update task status to disputed
    (map-set tasks
      { task-id: task-id }
      (merge task-data { status: STATUS_DISPUTED })
    )
    
    (print {
      event: "dispute-initiated",
      task-id: task-id,
      initiator: tx-sender,
      reason: dispute-reason
    })
    
    (ok true)
  )
)

;; Emergency function to cancel task and refund (only before submission)
(define-public (cancel-task (task-id uint))
  (let 
    (
      (task-data (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND))
    )
    (asserts! (is-task-client task-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! 
      (or 
        (is-eq (get status task-data) STATUS_CREATED)
        (is-eq (get status task-data) STATUS_IN_PROGRESS)
      ) 
      ERR_INVALID_STATUS
    )
    
    ;; Refund payment to client
    (try! (as-contract (stx-transfer? (get payment-amount task-data) tx-sender (get client task-data))))
    
    ;; Remove task (or could set to cancelled status)
    (map-delete tasks { task-id: task-id })
    
    (print {
      event: "task-cancelled",
      task-id: task-id,
      refunded-amount: (get payment-amount task-data)
    })
    
    (ok true)
  )
)

;; Admin function to update platform fee
(define-public (set-platform-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-percentage u1000) (err u108)) ;; Max 10%
    (var-set platform-fee-percentage new-fee-percentage)
    (ok true)
  )
)