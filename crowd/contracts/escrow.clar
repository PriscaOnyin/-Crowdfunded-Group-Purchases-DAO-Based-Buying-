;; escrow-contract.clar
;; A smart contract that locks funds until a purchase goal is reached

;; Error codes
(define-constant ERR-DEADLINE-PASSED (err u100))
(define-constant ERR-GOAL-NOT-MET (err u101))
(define-constant ERR-ALREADY-CLAIMED (err u102))
(define-constant ERR-NOT-BENEFICIARY (err u103))
(define-constant ERR-DEADLINE-NOT-REACHED (err u104))
(define-constant ERR-GOAL-ALREADY-MET (err u105))
(define-constant ERR-ZERO-AMOUNT (err u106))

;; Data variables
(define-data-var beneficiary principal tx-sender)
(define-data-var goal uint u0)
(define-data-var deadline uint u0)
(define-data-var total-raised uint u0)
(define-data-var is-executed bool false)
(define-data-var is-cancelled bool false)

;; Maps to track contributions and claims
(define-map contributions principal uint)
(define-map refund-claimed principal bool)

;; Initialize the escrow contract
(define-public (initialize (purchase-goal uint) (deadline-height uint) (purchase-beneficiary principal))
  (begin
    (asserts! (is-eq tx-sender (var-get beneficiary)) (err u1))
    (asserts! (> purchase-goal u0) (err u2))
    (asserts! (> deadline-height stacks-block-height) (err u3))
    
    (var-set goal purchase-goal)
    (var-set deadline deadline-height)
    (var-set beneficiary purchase-beneficiary)
    
    (ok true)))

;; Contribute funds to the escrow
(define-public (contribute)
  (let ((amount (stx-get-balance tx-sender)))
    (begin
      ;; Check that the deadline hasn't passed
      (asserts! (< stacks-block-height (var-get deadline)) ERR-DEADLINE-PASSED)
      ;; Check that the goal hasn't been met yet
      (asserts! (not (var-get is-executed)) ERR-GOAL-ALREADY-MET)
      ;; Check that the contract isn't cancelled
      (asserts! (not (var-get is-cancelled)) ERR-GOAL-NOT-MET)
      ;; Check that the amount is greater than zero
      (asserts! (> amount u0) ERR-ZERO-AMOUNT)
      
      ;; Transfer STX from sender to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update the contribution map and total raised
      (map-set contributions tx-sender 
        (+ (default-to u0 (map-get? contributions tx-sender)) amount))
      (var-set total-raised (+ (var-get total-raised) amount))
      
      ;; Check if goal is met after this contribution
      (if (>= (var-get total-raised) (var-get goal))
        (execute-purchase)
        (ok true)))))

;; Execute the purchase if goal is met
(define-private (execute-purchase)
  (begin
    (asserts! (>= (var-get total-raised) (var-get goal)) ERR-GOAL-NOT-MET)
    (asserts! (not (var-get is-executed)) ERR-GOAL-ALREADY-MET)
    
    ;; Transfer the funds to the beneficiary
    (try! (as-contract (stx-transfer? (var-get total-raised) tx-sender (var-get beneficiary))))
    
    ;; Mark as executed
    (var-set is-executed true)
    (ok true)))

;; Manually trigger execution (can be called by anyone if goal is met)
(define-public (trigger-execution)
  (begin
    (asserts! (>= (var-get total-raised) (var-get goal)) ERR-GOAL-NOT-MET)
    (asserts! (not (var-get is-executed)) ERR-GOAL-ALREADY-MET)
    (execute-purchase)))

;; Claim refund if deadline passed and goal not met
(define-public (claim-refund)
  (let 
    ((contribution (default-to u0 (map-get? contributions tx-sender))))
    (begin
      ;; Check that the deadline has passed
      (asserts! (>= stacks-block-height (var-get deadline)) ERR-DEADLINE-NOT-REACHED)
      ;; Check that the goal was not met
      (asserts! (< (var-get total-raised) (var-get goal)) ERR-GOAL-NOT-MET)
      ;; Check that the user has not already claimed their refund
      (asserts! (not (default-to false (map-get? refund-claimed tx-sender))) ERR-ALREADY-CLAIMED)
      ;; Check that the user has contributed
      (asserts! (> contribution u0) ERR-ZERO-AMOUNT)
      
      ;; Transfer the refund
      (try! (as-contract (stx-transfer? contribution tx-sender tx-sender)))
      
      ;; Mark as claimed
      (map-set refund-claimed tx-sender true)
      (ok true))))

;; Cancel the escrow and enable refunds (only beneficiary can do this)
(define-public (cancel-escrow)
  (begin
    (asserts! (is-eq tx-sender (var-get beneficiary)) ERR-NOT-BENEFICIARY)
    (asserts! (not (var-get is-executed)) ERR-GOAL-ALREADY-MET)
    (asserts! (not (var-get is-cancelled)) (err u107))
    
    (var-set is-cancelled true)
    (ok true)))

;; Read-only functions to check status
(define-read-only (get-contribution (contributor principal))
  (default-to u0 (map-get? contributions contributor)))

(define-read-only (get-total-raised)
  (var-get total-raised))

(define-read-only (get-goal)
  (var-get goal))

(define-read-only (get-deadline)
  (var-get deadline))

(define-read-only (is-goal-met)
  (>= (var-get total-raised) (var-get goal)))

(define-read-only (is-deadline-passed)
  (>= stacks-block-height (var-get deadline)))

(define-read-only (get-beneficiary)
  (var-get beneficiary))

(define-read-only (get-contract-status)
  {
    total-raised: (var-get total-raised),
    goal: (var-get goal),
    deadline: (var-get deadline),
    is-executed: (var-get is-executed),
    is-cancelled: (var-get is-cancelled),
    beneficiary: (var-get beneficiary),
    current-block: stacks-block-height
  })