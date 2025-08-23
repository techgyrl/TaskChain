;; TaskChain Governance Contract
;; Enables decentralized governance for platform parameters and upgrades

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-INVALID-PROPOSAL (err u402))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u403))
(define-constant ERR-VOTING-ENDED (err u404))
(define-constant ERR-VOTING-ACTIVE (err u405))
(define-constant ERR-ALREADY-VOTED (err u406))
(define-constant ERR-INSUFFICIENT-TOKENS (err u407))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u408))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u409))

;; Proposal types
(define-constant PROPOSAL-TYPE-PARAMETER u1)
(define-constant PROPOSAL-TYPE-FEE-UPDATE u2)
(define-constant PROPOSAL-TYPE-UPGRADE u3)
(define-constant PROPOSAL-TYPE-EMERGENCY u4)

;; Governance parameters
(define-data-var proposal-counter uint u0)
(define-data-var min-proposal-threshold uint u1000) ;; Minimum TCHN tokens to create proposal
(define-data-var voting-period uint u10080) ;; Voting period in blocks (~1 week)
(define-data-var quorum-threshold uint u20) ;; 20% of total supply needed for quorum
(define-data-var approval-threshold uint u51) ;; 51% approval needed to pass

;; Platform parameters that can be governed
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (basis points)
(define-data-var max-task-duration uint u4320) ;; Maximum task duration in blocks
(define-data-var min-task-payment uint u1000000) ;; Minimum task payment in microSTX
(define-data-var dispute-resolution-fee uint u100000) ;; Fee for dispute resolution

;; Data structures
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    proposal-type: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    parameter-name: (optional (string-ascii 50)),
    parameter-value: (optional uint),
    target-contract: (optional principal),
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    executed: bool,
    created-at: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool, ;; true for yes, false for no
    voting-power: uint,
    block-height: uint
  }
)

(define-map voter-participation
  { voter: principal }
  {
    total-votes: uint,
    last-vote-block: uint
  }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-governance-parameters)
  {
    min-proposal-threshold: (var-get min-proposal-threshold),
    voting-period: (var-get voting-period),
    quorum-threshold: (var-get quorum-threshold),
    approval-threshold: (var-get approval-threshold)
  }
)

(define-read-only (get-platform-parameters)
  {
    platform-fee-percentage: (var-get platform-fee-percentage),
    max-task-duration: (var-get max-task-duration),
    min-task-payment: (var-get min-task-payment),
    dispute-resolution-fee: (var-get dispute-resolution-fee)
  }
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
    (let
      (
        (current-block stacks-block-height)
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (total-supply u10000000) ;; This should be fetched from TCHN token contract
        (quorum-met (>= (* total-votes u100) (* total-supply (var-get quorum-threshold))))
        (approval-met (>= (* (get votes-for proposal) u100) (* total-votes (var-get approval-threshold))))
      )
      {
        active: (and (>= current-block (get start-block proposal)) 
                    (<= current-block (get end-block proposal))),
        passed: (and quorum-met approval-met (> current-block (get end-block proposal))),
        executed: (get executed proposal),
        quorum-met: quorum-met,
        approval-met: approval-met,
        total-votes: total-votes
      }
    )
    { active: false, passed: false, executed: false, quorum-met: false, approval-met: false, total-votes: u0 }
  )
)

;; Private functions

(define-private (get-voting-power (voter principal))
  ;; This should integrate with TCHN token contract to get actual balance
  ;; For now, returning a placeholder value
  u1000
)

;; Public functions

(define-public (create-proposal 
  (proposal-type uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (parameter-name (optional (string-ascii 50)))
  (parameter-value (optional uint))
  (target-contract (optional principal))
)
  (let
    (
      (proposer-balance (get-voting-power tx-sender))
      (proposal-id (+ (var-get proposal-counter) u1))
      (start-block (+ stacks-block-height u144)) ;; Start voting after ~1 day
      (end-block (+ start-block (var-get voting-period)))
    )
    (asserts! (>= proposer-balance (var-get min-proposal-threshold)) ERR-INSUFFICIENT-TOKENS)
    (asserts! (and (>= proposal-type u1) (<= proposal-type u4)) ERR-INVALID-PROPOSAL)
    
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        proposal-type: proposal-type,
        title: title,
        description: description,
        parameter-name: parameter-name,
        parameter-value: parameter-value,
        target-contract: target-contract,
        start-block: start-block,
        end-block: end-block,
        votes-for: u0,
        votes-against: u0,
        executed: false,
        created-at: stacks-block-height
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (voter-power (get-voting-power tx-sender))
      (current-block stacks-block-height)
    )
    (asserts! (>= current-block (get start-block proposal)) ERR-VOTING-ENDED)
    (asserts! (<= current-block (get end-block proposal)) ERR-VOTING-ENDED)
    (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
    (asserts! (> voter-power u0) ERR-INSUFFICIENT-TOKENS)
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: support,
        voting-power: voter-power,
        block-height: current-block
      }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal
        (if support
          { 
            votes-for: (+ (get votes-for proposal) voter-power),
            votes-against: (get votes-against proposal)
          }
          { 
            votes-for: (get votes-for proposal),
            votes-against: (+ (get votes-against proposal) voter-power)
          }
        )
      )
    )
    
    ;; Update voter participation
    (map-set voter-participation
      { voter: tx-sender }
      {
        total-votes: (+ (default-to u0 (get total-votes (map-get? voter-participation { voter: tx-sender }))) u1),
        last-vote-block: current-block
      }
    )
    
    (ok true)
  )
)



;; Placeholder execution functions - these should be implemented based on specific requirements
(define-private (execute-parameter-change (proposal (tuple (proposal-id uint) (proposer principal) (title (string-ascii 100)) (description (string-ascii 500)) (proposal-type uint) (target-value uint) (votes-for uint) (votes-against uint) (start-block uint) (end-block uint) (executed bool))))
  (ok u0)
)

(define-private (execute-fee-update (proposal (tuple (proposal-id uint) (proposer principal) (title (string-ascii 100)) (description (string-ascii 500)) (proposal-type uint) (target-value uint) (votes-for uint) (votes-against uint) (start-block uint) (end-block uint) (executed bool))))
  (ok u0)
)

(define-private (execute-upgrade (proposal (tuple (proposal-id uint) (proposer principal) (title (string-ascii 100)) (description (string-ascii 500)) (proposal-type uint) (target-value uint) (votes-for uint) (votes-against uint) (start-block uint) (end-block uint) (executed bool))))
  (ok u0)
)

(define-private (execute-emergency-action (proposal (tuple (proposal-id uint) (proposer principal) (title (string-ascii 100)) (description (string-ascii 500)) (proposal-type uint) (target-value uint) (votes-for uint) (votes-against uint) (start-block uint) (end-block uint) (executed bool))))
  (ok u0)
)


;; Admin functions (only for initial setup and emergency)

(define-public (update-governance-parameter (parameter (string-ascii 20)) (value uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (if (is-eq parameter "min-proposal-threshold")
      (var-set min-proposal-threshold value)
      (if (is-eq parameter "voting-period")
        (var-set voting-period value)
        (if (is-eq parameter "quorum-threshold")
          (var-set quorum-threshold value)
          (if (is-eq parameter "approval-threshold")
            (var-set approval-threshold value)
            false
          )
        )
      )
    )
    (ok true)
  )
)

;; Emergency pause function
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    ;; This would integrate with main contract to pause operations
    (ok true)
  )
)
