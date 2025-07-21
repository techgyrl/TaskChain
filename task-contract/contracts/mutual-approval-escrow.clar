;; Mutual Approval & Payment Release Smart Contract
;; Handles escrow, dual approval, and automatic payment release

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_TASK_NOT_FOUND (err u101))
(define-constant ERR_TASK_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_TASK_ALREADY_COMPLETED (err u104))
(define-constant ERR_TASK_NOT_FUNDED (err u105))
(define-constant ERR_ALREADY_APPROVED (err u106))
(define-constant ERR_CANNOT_APPROVE_OWN_TASK (err u107))

;; Data Variables
(define-data-var next-task-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (250 basis points)

;; Task Status Enum
(define-constant TASK_STATUS_CREATED u0)
(define-constant TASK_STATUS_FUNDED u1)
(define-constant TASK_STATUS_COMPLETED u2)
(define-constant TASK_STATUS_DISPUTED u3)

;; Data Maps
(define-map tasks
  { task-id: uint }
  {
    client: principal,
    freelancer: principal,
    amount: uint,
    description: (string-utf8 500),
    status: uint,
    created-at: uint,
    client-approved: bool,
    freelancer-approved: bool,
    client-approval-time: (optional uint),
    freelancer-approval-time: (optional uint)
  }
)

(define-map escrow-balances
  { task-id: uint }
  { amount: uint }
)

(define-map user-stats
  { user: principal }
  {
    tasks-completed: uint,
    total-earned: uint,
    total-spent: uint,
    reputation-score: uint
  }
)

;; Read-only functions
(define-read-only (get-task (task-id uint))
  (map-get? tasks { task-id: task-id })
)

(define-read-only (get-escrow-balance (task-id uint))
  (map-get? escrow-balances { task-id: task-id })
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { tasks-completed: u0, total-earned: u0, total-spent: u0, reputation-score: u100 }
    (map-get? user-stats { user: user })
  )
)

(define-read-only (get-platform-fee-percentage)
  (var-get platform-fee-percentage)
)

(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u10000)
)

(define-read-only (get-next-task-id)
  (var-get next-task-id)
)

(define-read-only (is-task-ready-for-completion (task-id uint))
  (match (get-task task-id)
    task-data (and 
                (get client-approved task-data)
                (get freelancer-approved task-data)
                (is-eq (get status task-data) TASK_STATUS_FUNDED))
    false
  )
)

;; ;; Private functions
;; (define-private (update-user-stats-completion (client principal) (freelancer principal) (amount uint))
;;   (let (
;;     (client-stats (get-user-stats client))
;;     (freelancer-stats (get-user-stats freelancer))
;;     (platform-fee (calculate-platform-fee amount))
;;     (freelancer-amount (- amount platform-fee))
;;   )
;;     (map-set user-stats 
;;       { user: client }
;;       (merge client-stats { 
;;         tasks-completed: (+ (get tasks-completed client-stats) u1),
;;         total-spent: (+ (get total-spent client-stats) amount)
;;       })
;;     )
;;     (map-set user-stats 
;;       { user: freelancer }
;;       (merge freelancer-stats { 
;;         tasks-completed: (+ (get tasks-completed freelancer-stats) u1),
;;         total-earned: (+ (get total-earned freelancer-stats) freelancer-amount),
;;         reputation-score: (min u1000 (+ (get reputation-score freelancer-stats) u10))
;;       })
;;     )
;;   )
;; )

;; Private functions
(define-private (update-user-stats-completion (client principal) (freelancer principal) (amount uint))
  (let (
    (client-stats (get-user-stats client))
    (freelancer-stats (get-user-stats freelancer))
    (platform-fee (calculate-platform-fee amount))
    (freelancer-amount (- amount platform-fee))
    (current-reputation (get reputation-score freelancer-stats))
    (new-reputation (+ current-reputation u10))
    (capped-reputation (if (<= new-reputation u1000) new-reputation u1000))
  )
    (map-set user-stats 
      { user: client }
      (merge client-stats { 
        tasks-completed: (+ (get tasks-completed client-stats) u1),
        total-spent: (+ (get total-spent client-stats) amount)
      })
    )
    (map-set user-stats 
      { user: freelancer }
      (merge freelancer-stats { 
        tasks-completed: (+ (get tasks-completed freelancer-stats) u1),
        total-earned: (+ (get total-earned freelancer-stats) freelancer-amount),
        reputation-score: capped-reputation
      })
    )
  )
)

;; Public functions

;; Create a new task
(define-public (create-task (freelancer principal) (amount uint) (description (string-utf8 500)))
  (let (
    (task-id (var-get next-task-id))
    (current-block-height stacks-block-height)
  )
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (not (is-eq tx-sender freelancer)) ERR_CANNOT_APPROVE_OWN_TASK)
    (asserts! (is-none (get-task task-id)) ERR_TASK_ALREADY_EXISTS)
    
    (map-set tasks
      { task-id: task-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        amount: amount,
        description: description,
        status: TASK_STATUS_CREATED,
        created-at: current-block-height,
        client-approved: false,
        freelancer-approved: false,
        client-approval-time: none,
        freelancer-approval-time: none
      }
    )
    
    (var-set next-task-id (+ task-id u1))
    (ok task-id)
  )
)

;; Fund a task (client deposits funds into escrow)
(define-public (fund-task (task-id uint))
  (match (get-task task-id)
    task-data (begin
      (asserts! (is-eq tx-sender (get client task-data)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get status task-data) TASK_STATUS_CREATED) ERR_TASK_ALREADY_COMPLETED)
      
      ;; Transfer funds to contract
      (try! (stx-transfer? (get amount task-data) tx-sender (as-contract tx-sender)))
      
      ;; Update escrow balance
      (map-set escrow-balances
        { task-id: task-id }
        { amount: (get amount task-data) }
      )
      
      ;; Update task status
      (map-set tasks
        { task-id: task-id }
        (merge task-data { status: TASK_STATUS_FUNDED })
      )
      
      (ok true)
    )
    ERR_TASK_NOT_FOUND
  )
)

;; Client approves task completion
(define-public (client-approve-completion (task-id uint))
  (match (get-task task-id)
    task-data (begin
      (asserts! (is-eq tx-sender (get client task-data)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get status task-data) TASK_STATUS_FUNDED) ERR_TASK_NOT_FUNDED)
      (asserts! (not (get client-approved task-data)) ERR_ALREADY_APPROVED)
      
      (map-set tasks
        { task-id: task-id }
        (merge task-data { 
          client-approved: true,
          client-approval-time: (some stacks-block-height)
        })
      )
      
      ;; Check if both parties have approved and auto-complete if so
      (if (get freelancer-approved task-data)
        (ok (try! (complete-task task-id)))
        (ok true)
      )
    )
    ERR_TASK_NOT_FOUND
  )
)

;; Freelancer approves task completion
(define-public (freelancer-approve-completion (task-id uint))
  (match (get-task task-id)
    task-data (begin
      (asserts! (is-eq tx-sender (get freelancer task-data)) ERR_UNAUTHORIZED)
      (asserts! (is-eq (get status task-data) TASK_STATUS_FUNDED) ERR_TASK_NOT_FUNDED)
      (asserts! (not (get freelancer-approved task-data)) ERR_ALREADY_APPROVED)
      
      (map-set tasks
        { task-id: task-id }
        (merge task-data { 
          freelancer-approved: true,
          freelancer-approval-time: (some stacks-block-height)
        })
      )
      
      ;; Check if both parties have approved and auto-complete if so
      (if (get client-approved task-data)
        (ok (try! (complete-task task-id)))
        (ok true)
      )
    )
    ERR_TASK_NOT_FOUND
  )
)

;; Complete task and release funds (called automatically when both approve)
(define-private (complete-task (task-id uint))
  (match (get-task task-id)
    task-data (match (get-escrow-balance task-id)
      escrow-data (let (
        (total-amount (get amount escrow-data))
        (platform-fee (calculate-platform-fee total-amount))
        (freelancer-amount (- total-amount platform-fee))
        (client (get client task-data))
        (freelancer (get freelancer task-data))
      )
        (asserts! (and (get client-approved task-data) (get freelancer-approved task-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status task-data) TASK_STATUS_FUNDED) ERR_TASK_NOT_FUNDED)
        
        ;; Transfer funds to freelancer
        (try! (as-contract (stx-transfer? freelancer-amount tx-sender freelancer)))
        
        ;; Transfer platform fee to contract owner
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
        
        ;; Update task status
        (map-set tasks
          { task-id: task-id }
          (merge task-data { status: TASK_STATUS_COMPLETED })
        )
        
        ;; Clear escrow balance
        (map-delete escrow-balances { task-id: task-id })
        
        ;; Update user statistics
        (update-user-stats-completion client freelancer total-amount)
        
        (ok true)
      )
      ERR_TASK_NOT_FOUND
    )
    ERR_TASK_NOT_FOUND
  )
)

;; Emergency functions (only contract owner)
(define-public (set-platform-fee-percentage (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee u1000) (err u108)) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

;; Dispute resolution (simplified - in production would have more complex logic)
(define-public (resolve-dispute (task-id uint) (award-to-freelancer bool))
  (match (get-task task-id)
    task-data (match (get-escrow-balance task-id)
      escrow-data (let (
        (total-amount (get amount escrow-data))
        (platform-fee (calculate-platform-fee total-amount))
        (net-amount (- total-amount platform-fee))
        (client (get client task-data))
        (freelancer (get freelancer task-data))
      )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status task-data) TASK_STATUS_FUNDED) ERR_TASK_NOT_FUNDED)
        
        ;; Award funds based on decision
        (if award-to-freelancer
          (try! (as-contract (stx-transfer? net-amount tx-sender freelancer)))
          (try! (as-contract (stx-transfer? net-amount tx-sender client)))
        )
        
        ;; Transfer platform fee
        (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))
        
        ;; Update task status
        (map-set tasks
          { task-id: task-id }
          (merge task-data { status: TASK_STATUS_DISPUTED })
        )
        
        ;; Clear escrow balance
        (map-delete escrow-balances { task-id: task-id })
        
        (ok true)
      )
      ERR_TASK_NOT_FOUND
    )
    ERR_TASK_NOT_FOUND
  )
)
