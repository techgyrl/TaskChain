;; Reputation & Rating System Smart Contract
;; Manages ratings, feedback, and reputation scores for freelancers and clients

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TASK-NOT-FOUND (err u101))
(define-constant ERR-TASK-NOT-COMPLETED (err u102))
(define-constant ERR-ALREADY-RATED (err u103))
(define-constant ERR-INVALID-RATING (err u104))
(define-constant ERR-CANNOT-RATE-SELF (err u105))
(define-constant ERR-NOT-TASK-PARTICIPANT (err u106))
(define-constant ERR-INVALID-FEEDBACK-LENGTH (err u107))
(define-constant ERR-USER-NOT-FOUND (err u108))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-RATING u1)
(define-constant MAX-RATING u5)
(define-constant MAX-FEEDBACK-LENGTH u512)
(define-constant REPUTATION-DECAY-FACTOR u95) ;; 95% - slight decay over time
(define-constant INITIAL-REPUTATION-SCORE u100)

;; Task status constants
(define-constant TASK-COMPLETED u3)

;; Data structures
(define-map tasks
  { task-id: uint }
  {
    client: principal,
    freelancer: principal,
    status: uint,
    completed-at: (optional uint),
    payment-amount: uint
  }
)

(define-map ratings
  { 
    task-id: uint,
    rater: principal 
  }
  {
    ratee: principal,
    rating: uint,
    feedback: (string-utf8 512),
    rated-at: uint,
    payment-amount: uint ;; Context for weighted ratings
  }
)

(define-map user-profiles
  { user: principal }
  {
    total-ratings-received: uint,
    total-rating-score: uint,
    total-ratings-given: uint,
    reputation-score: uint,
    last-reputation-update: uint,
    is-freelancer: bool,
    is-client: bool
  }
)

(define-map detailed-reputation
  { user: principal }
  {
    five-star-count: uint,
    four-star-count: uint,
    three-star-count: uint,
    two-star-count: uint,
    one-star-count: uint,
    total-earnings: uint, ;; For freelancers
    total-spent: uint, ;; For clients
    completion-rate: uint ;; Percentage * 100
  }
)

(define-map mutual-ratings-tracker
  { task-id: uint }
  {
    client-rated: bool,
    freelancer-rated: bool,
    both-completed: bool
  }
)

;; Counters
(define-data-var task-counter uint u0)

;; Helper functions
(define-private (is-valid-rating (rating uint))
  (and (>= rating MIN-RATING) (<= rating MAX-RATING))
)

(define-private (is-valid-feedback (feedback (string-utf8 512)))
  (<= (len feedback) MAX-FEEDBACK-LENGTH)
)

;; User profile management
(define-public (register-user (is-freelancer bool) (is-client bool))
  (let
    (
      (existing-profile (map-get? user-profiles { user: tx-sender }))
    )
    (if (is-none existing-profile)
      (begin
        (map-set user-profiles 
          { user: tx-sender }
          {
            total-ratings-received: u0,
            total-rating-score: u0,
            total-ratings-given: u0,
            reputation-score: INITIAL-REPUTATION-SCORE,
            last-reputation-update: stacks-block-height,
            is-freelancer: is-freelancer,
            is-client: is-client
          }
        )
        (map-set detailed-reputation
          { user: tx-sender }
          {
            five-star-count: u0,
            four-star-count: u0,
            three-star-count: u0,
            two-star-count: u0,
            one-star-count: u0,
            total-earnings: u0,
            total-spent: u0,
            completion-rate: u10000 ;; 100% * 100
          }
        )
        (ok "User registered successfully")
      )
      (ok "User already exists")
    )
  )
)

;; Task creation (simplified - would integrate with main task contract)
(define-public (create-task 
  (freelancer principal) 
  (payment-amount uint))
  (let
    (
      (task-id (+ (var-get task-counter) u1))
    )
    (map-set tasks
      { task-id: task-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        status: u1, ;; Created
        completed-at: none,
        payment-amount: payment-amount
      }
    )
    (var-set task-counter task-id)
    (ok task-id)
  )
)

;; Mark task as completed (would be called by main contract)
(define-public (complete-task (task-id uint))
  (let
    (
      (task-data (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get client task-data))
      (is-eq tx-sender (get freelancer task-data))
      (is-eq tx-sender CONTRACT-OWNER)
    ) ERR-NOT-AUTHORIZED)
    
    (map-set tasks
      { task-id: task-id }
      (merge task-data {
        status: TASK-COMPLETED,
        completed-at: (some stacks-block-height)
      })
    )
    
    ;; Initialize rating tracker
    (map-set mutual-ratings-tracker
      { task-id: task-id }
      {
        client-rated: false,
        freelancer-rated: false,
        both-completed: false
      }
    )
    
    (ok true)
  )
)

;; Submit rating and feedback
(define-public (submit-rating 
  (task-id uint)
  (rating uint)
  (feedback (string-utf8 512)))
  (let
    (
      (task-data (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
      (rater tx-sender)
      (is-client (is-eq rater (get client task-data)))
      (is-freelancer (is-eq rater (get freelancer task-data)))
      (ratee (if is-client (get freelancer task-data) (get client task-data)))
      (existing-rating (map-get? ratings { task-id: task-id, rater: rater }))
    )
    ;; Validations
    (asserts! (is-eq (get status task-data) TASK-COMPLETED) ERR-TASK-NOT-COMPLETED)
    (asserts! (or is-client is-freelancer) ERR-NOT-TASK-PARTICIPANT)
    (asserts! (not (is-eq rater ratee)) ERR-CANNOT-RATE-SELF)
    (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
    (asserts! (is-valid-rating rating) ERR-INVALID-RATING)
    (asserts! (is-valid-feedback feedback) ERR-INVALID-FEEDBACK-LENGTH)
    
    ;; Store rating
    (map-set ratings
      { task-id: task-id, rater: rater }
      {
        ratee: ratee,
        rating: rating,
        feedback: feedback,
        rated-at: stacks-block-height,
        payment-amount: (get payment-amount task-data)
      }
    )
    
    ;; Update ratee's profile
    (unwrap! (update-user-reputation ratee rating (get payment-amount task-data)) ERR-USER-NOT-FOUND)
    
    ;; Update rater's given ratings count
    (unwrap! (increment-ratings-given rater) ERR-USER-NOT-FOUND)
    
    ;; Update mutual rating tracker
    (unwrap! (update-mutual-rating-tracker task-id is-client is-freelancer) ERR-TASK-NOT-FOUND)
    
    (ok true)
  )
)

;; Update user reputation based on new rating
(define-private (update-user-reputation 
  (user principal) 
  (new-rating uint) 
  (payment-amount uint))
  (let
    (
      (profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND))
      (detailed (unwrap! (map-get? detailed-reputation { user: user }) ERR-USER-NOT-FOUND))
      (new-total-ratings (+ (get total-ratings-received profile) u1))
      (new-total-score (+ (get total-rating-score profile) new-rating))
      (average-rating (/ (* new-total-score u100) new-total-ratings))
      
      ;; Calculate weighted reputation score (considers payment amount and recency)
      (payment-weight-calc (/ payment-amount u10))
      (payment-weight (if (> payment-weight-calc u50) u50 payment-weight-calc)) ;; Max weight of 50
      (weighted-score (+ (* average-rating u70) (* new-rating u20) payment-weight))
      (new-reputation (if (> weighted-score u500) u500 weighted-score)) ;; Cap at 500
    )
    ;; Update main profile
    (map-set user-profiles
      { user: user }
      (merge profile {
        total-ratings-received: new-total-ratings,
        total-rating-score: new-total-score,
        reputation-score: new-reputation,
        last-reputation-update: stacks-block-height
      })
    )
    
    ;; Update detailed reputation - increment the appropriate star count
    (map-set detailed-reputation
      { user: user }
      (merge detailed {
        five-star-count: (if (is-eq new-rating u5) 
          (+ (get five-star-count detailed) u1)
          (get five-star-count detailed)),
        four-star-count: (if (is-eq new-rating u4) 
          (+ (get four-star-count detailed) u1)
          (get four-star-count detailed)),
        three-star-count: (if (is-eq new-rating u3) 
          (+ (get three-star-count detailed) u1)
          (get three-star-count detailed)),
        two-star-count: (if (is-eq new-rating u2) 
          (+ (get two-star-count detailed) u1)
          (get two-star-count detailed)),
        one-star-count: (if (is-eq new-rating u1) 
          (+ (get one-star-count detailed) u1)
          (get one-star-count detailed))
      })
    )
    
    (ok true)
  )
)

;; Increment ratings given counter
(define-private (increment-ratings-given (user principal))
  (let
    (
      (profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND))
    )
    (map-set user-profiles
      { user: user }
      (merge profile {
        total-ratings-given: (+ (get total-ratings-given profile) u1)
      })
    )
    (ok true)
  )
)

;; Update mutual rating tracker
(define-private (update-mutual-rating-tracker 
  (task-id uint) 
  (rater-is-client bool) 
  (rater-is-freelancer bool))
  (let
    (
      (tracker (unwrap! (map-get? mutual-ratings-tracker { task-id: task-id }) ERR-TASK-NOT-FOUND))
      (new-client-rated (if rater-is-client true (get client-rated tracker)))
      (new-freelancer-rated (if rater-is-freelancer true (get freelancer-rated tracker)))
      (both-completed (and new-client-rated new-freelancer-rated))
    )
    (map-set mutual-ratings-tracker
      { task-id: task-id }
      {
        client-rated: new-client-rated,
        freelancer-rated: new-freelancer-rated,
        both-completed: both-completed
      }
    )
    (ok true)
  )
)

;; Read-only functions

;; Get user reputation profile
(define-read-only (get-user-profile (user principal))
  (let
    (
      (profile (map-get? user-profiles { user: user }))
      (detailed (map-get? detailed-reputation { user: user }))
    )
    (ok {
      basic-profile: profile,
      detailed-reputation: detailed
    })
  )
)

;; Calculate average rating for user
(define-read-only (get-average-rating (user principal))
  (let
    (
      (profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND))
      (total-ratings (get total-ratings-received profile))
    )
    (if (> total-ratings u0)
      (ok (/ (* (get total-rating-score profile) u100) total-ratings))
      (ok u0)
    )
  )
)

;; Get rating for specific task
(define-read-only (get-task-rating (task-id uint) (rater principal))
  (ok (map-get? ratings { task-id: task-id, rater: rater }))
)

;; Get all ratings for a task (both directions)
(define-read-only (get-task-mutual-ratings (task-id uint))
  (let
    (
      (task-data (unwrap! (map-get? tasks { task-id: task-id }) ERR-TASK-NOT-FOUND))
      (client (get client task-data))
      (freelancer (get freelancer task-data))
      (client-rating (map-get? ratings { task-id: task-id, rater: client }))
      (freelancer-rating (map-get? ratings { task-id: task-id, rater: freelancer }))
      (tracker (map-get? mutual-ratings-tracker { task-id: task-id }))
    )
    (ok {
      client-rating: client-rating,
      freelancer-rating: freelancer-rating,
      rating-status: tracker
    })
  )
)

;; Get reputation score with decay factor
(define-read-only (get-current-reputation (user principal))
  (let
    (
      (profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND))
      (time-since-update (- stacks-block-height (get last-reputation-update profile)))
      (decay-blocks u1440) ;; Approximate blocks per day
      (decay-periods (/ time-since-update decay-blocks))
      (decay-multiplier (if (> decay-periods u0) 
        (pow REPUTATION-DECAY-FACTOR decay-periods) 
        u100))
      (current-reputation (/ (* (get reputation-score profile) decay-multiplier) u100))
    )
    (ok (if (< current-reputation u1) u1 current-reputation)) ;; Minimum reputation of 1
  )
)

;; Get user ranking information
(define-read-only (get-user-ranking (user principal))
  (let
    (
      (profile (unwrap! (map-get? user-profiles { user: user }) ERR-USER-NOT-FOUND))
      (detailed (unwrap! (map-get? detailed-reputation { user: user }) ERR-USER-NOT-FOUND))
      (avg-rating (unwrap! (get-average-rating user) ERR-USER-NOT-FOUND))
      (current-rep (unwrap! (get-current-reputation user) ERR-USER-NOT-FOUND))
    )
    (ok {
      average-rating: avg-rating,
      current-reputation: current-rep,
      total-ratings: (get total-ratings-received profile),
      five-star-percentage: (if (> (get total-ratings-received profile) u0)
        (/ (* (get five-star-count detailed) u10000) (get total-ratings-received profile))
        u0),
      completion-rate: (get completion-rate detailed)
    })
  )
)

;; Check if both parties have rated each other for a task
(define-read-only (are-mutual-ratings-complete (task-id uint))
  (let
    (
      (tracker (map-get? mutual-ratings-tracker { task-id: task-id }))
    )
    (if (is-some tracker)
      (ok (get both-completed (unwrap-panic tracker)))
      (ok false)
    )
  )
)

;; Get user's rating history (simplified - would need pagination in production)
(define-read-only (get-user-rating-summary (user principal))
  (let
    (
      (profile (map-get? user-profiles { user: user }))
      (detailed (map-get? detailed-reputation { user: user }))
    )
    (if (and (is-some profile) (is-some detailed))
      (ok {
        total-received: (get total-ratings-received (unwrap-panic profile)),
        total-given: (get total-ratings-given (unwrap-panic profile)),
        breakdown: {
          five-stars: (get five-star-count (unwrap-panic detailed)),
          four-stars: (get four-star-count (unwrap-panic detailed)),
          three-stars: (get three-star-count (unwrap-panic detailed)),
          two-stars: (get two-star-count (unwrap-panic detailed)),
          one-stars: (get one-star-count (unwrap-panic detailed))
        }
      })
      ERR-USER-NOT-FOUND
    )
  )
)