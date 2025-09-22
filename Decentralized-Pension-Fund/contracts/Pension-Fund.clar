;; Decentralized Pension Fund Contract
;; Community-managed retirement savings with transparent governance

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_MEMBER_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_MEMBER (err u104))
(define-constant ERR_NOT_RETIREMENT_AGE (err u105))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u106))
(define-constant ERR_ALREADY_VOTED (err u107))
(define-constant ERR_PROPOSAL_EXPIRED (err u108))
(define-constant ERR_INVALID_PROPOSAL (err u109))
(define-constant RETIREMENT_AGE u65)
(define-constant MIN_CONTRIBUTION u1000000) ;; 1 STX minimum
(define-constant PROPOSAL_DURATION u1440) ;; ~10 days in blocks

;; Data Variables
(define-data-var total-fund uint u0)
(define-data-var member-count uint u0)
(define-data-var proposal-counter uint u0)

;; Data Maps
(define-map members 
  principal 
  {
    balance: uint,
    age: uint,
    join-block: uint,
    last-contribution: uint,
    is-active: bool
  }
)

(define-map proposals
  uint
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    recipient: (optional principal),
    proposal-type: (string-ascii 20), ;; "withdrawal", "investment", "governance"
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    executed: bool
  }
)

(define-map votes
  {proposal-id: uint, voter: principal}
  {vote: bool, voting-power: uint}
)

(define-map governance-settings
  (string-ascii 50)
  uint
)

;; Initialize governance settings
(map-set governance-settings "quorum-threshold" u51) ;; 51% quorum
(map-set governance-settings "approval-threshold" u60) ;; 60% approval needed

;; Read-only functions
(define-read-only (get-member-info (member principal))
  (map-get? members member)
)

(define-read-only (get-total-fund)
  (var-get total-fund)
)

(define-read-only (get-member-count)
  (var-get member-count)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-member-balance (member principal))
  (match (map-get? members member)
    member-data (some (get balance member-data))
    none
  )
)

(define-read-only (calculate-voting-power (member principal))
  (match (map-get? members member)
    member-data 
      (let ((balance (get balance member-data))
            (tenure (- block-height (get join-block member-data))))
        (+ balance (/ tenure u100))) ;; Balance + tenure bonus
    u0
  )
)

(define-read-only (get-governance-setting (setting (string-ascii 50)))
  (default-to u0 (map-get? governance-settings setting))
)

;; Private functions
(define-private (is-member (user principal))
  (match (map-get? members user)
    member-data (get is-active member-data)
    false
  )
)

(define-private (is-retirement-eligible (member principal))
  (match (map-get? members member)
    member-data (>= (get age member-data) RETIREMENT_AGE)
    false
  )
)

(define-private (calculate-withdrawal-amount (member principal))
  (match (map-get? members member)
    member-data
      (let ((balance (get balance member-data))
            (tenure (- block-height (get join-block member-data)))
            (bonus-rate (/ tenure u10000))) ;; Small bonus for longer tenure
        (+ balance (/ (* balance bonus-rate) u100)))
    u0
  )
)

;; Public functions

;; Join the pension fund
(define-public (join-fund (age uint))
  (let ((caller tx-sender))
    (asserts! (not (is-member caller)) ERR_ALREADY_MEMBER)
    (asserts! (> age u18) ERR_INVALID_AMOUNT)
    
    (map-set members caller
      {
        balance: u0,
        age: age,
        join-block: block-height,
        last-contribution: u0,
        is-active: true
      }
    )
    
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)
  )
)

;; Make a contribution to the pension fund
(define-public (contribute (amount uint))
  (let ((caller tx-sender))
    (asserts! (is-member caller) ERR_MEMBER_NOT_FOUND)
    (asserts! (>= amount MIN_CONTRIBUTION) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    
    (match (map-get? members caller)
      member-data
        (let ((new-balance (+ (get balance member-data) amount)))
          (map-set members caller
            (merge member-data 
              {
                balance: new-balance,
                last-contribution: block-height
              }
            )
          )
          (var-set total-fund (+ (var-get total-fund) amount))
          (ok new-balance)
        )
      ERR_MEMBER_NOT_FOUND
    )
  )
)

;; Create a proposal
(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (amount uint)
    (recipient (optional principal))
    (proposal-type (string-ascii 20)))
  (let ((caller tx-sender)
        (proposal-id (+ (var-get proposal-counter) u1)))
    
    (asserts! (is-member caller) ERR_MEMBER_NOT_FOUND)
    (asserts! (> (len title) u0) ERR_INVALID_PROPOSAL)
    
    (map-set proposals proposal-id
      {
        proposer: caller,
        title: title,
        description: description,
        amount: amount,
        recipient: recipient,
        proposal-type: proposal-type,
        votes-for: u0,
        votes-against: u0,
        created-at: block-height,
        executed: false
      }
    )
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (support bool))
  (let ((caller tx-sender)
        (voting-power (calculate-voting-power caller)))
    
    (asserts! (is-member caller) ERR_MEMBER_NOT_FOUND)
    (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: caller})) ERR_ALREADY_VOTED)
    
    (match (map-get? proposals proposal-id)
      proposal-data
        (begin
          (asserts! (< (- block-height (get created-at proposal-data)) PROPOSAL_DURATION) ERR_PROPOSAL_EXPIRED)
          
          (map-set votes {proposal-id: proposal-id, voter: caller}
            {vote: support, voting-power: voting-power}
          )
          
          (if support
            (map-set proposals proposal-id
              (merge proposal-data {votes-for: (+ (get votes-for proposal-data) voting-power)})
            )
            (map-set proposals proposal-id
              (merge proposal-data {votes-against: (+ (get votes-against proposal-data) voting-power)})
            )
          )
          
          (ok true)
        )
      ERR_PROPOSAL_NOT_FOUND
    )
  )
)

;; Execute a proposal (if it passes)
(define-public (execute-proposal (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal-data
      (let ((total-votes (+ (get votes-for proposal-data) (get votes-against proposal-data)))
            (total-voting-power (* (var-get member-count) u1000)) ;; Approximate total voting power
            (quorum-met (>= (* total-votes u100) (* total-voting-power (get-governance-setting "quorum-threshold"))))
            (proposal-passed (>= (* (get votes-for proposal-data) u100) (* total-votes (get-governance-setting "approval-threshold")))))
        
        (asserts! (not (get executed proposal-data)) ERR_INVALID_PROPOSAL)
        (asserts! quorum-met ERR_INVALID_PROPOSAL)
        (asserts! proposal-passed ERR_INVALID_PROPOSAL)
        
        (map-set proposals proposal-id
          (merge proposal-data {executed: true})
        )
        
        ;; Execute based on proposal type
        (if (is-eq (get proposal-type proposal-data) "withdrawal")
          (match (get recipient proposal-data)
            recipient-addr
              (begin
                (try! (as-contract (stx-transfer? (get amount proposal-data) tx-sender recipient-addr)))
                (var-set total-fund (- (var-get total-fund) (get amount proposal-data)))
                (ok true)
              )
            ERR_INVALID_PROPOSAL
          )
          (ok true) ;; Other proposal types would be implemented here
        )
      )
    ERR_PROPOSAL_NOT_FOUND
  )
)

;; Retire and withdraw funds
(define-public (retire)
  (let ((caller tx-sender))
    (asserts! (is-member caller) ERR_MEMBER_NOT_FOUND)
    (asserts! (is-retirement-eligible caller) ERR_NOT_RETIREMENT_AGE)
    
    (match (map-get? members caller)
      member-data
        (let ((withdrawal-amount (calculate-withdrawal-amount caller)))
          (asserts! (<= withdrawal-amount (var-get total-fund)) ERR_INSUFFICIENT_BALANCE)
          
          ;; Transfer funds to retiree
          (try! (as-contract (stx-transfer? withdrawal-amount tx-sender caller)))
          
          ;; Update member status and fund balance
          (map-set members caller (merge member-data {is-active: false, balance: u0}))
          (var-set total-fund (- (var-get total-fund) withdrawal-amount))
          (var-set member-count (- (var-get member-count) u1))
          
          (ok withdrawal-amount)
        )
      ERR_MEMBER_NOT_FOUND
    )
  )
)

;; Emergency withdrawal (with penalty)
(define-public (emergency-withdrawal)
  (let ((caller tx-sender))
    (asserts! (is-member caller) ERR_MEMBER_NOT_FOUND)
    
    (match (map-get? members caller)
      member-data
        (let ((balance (get balance member-data))
              (penalty (/ balance u10)) ;; 10% penalty
              (withdrawal-amount (- balance penalty)))
          
          (asserts! (<= balance (var-get total-fund)) ERR_INSUFFICIENT_BALANCE)
          
          ;; Transfer reduced amount to member
          (try! (as-contract (stx-transfer? withdrawal-amount tx-sender caller)))
          
          ;; Update member status and fund balance
          (map-set members caller (merge member-data {is-active: false, balance: u0}))
          (var-set total-fund (- (var-get total-fund) balance))
          (var-set member-count (- (var-get member-count) u1))
          
          (ok withdrawal-amount)
        )
      ERR_MEMBER_NOT_FOUND
    )
  )
)