;; Define constants for contribution limits
(define-constant MIN-CONTRIBUTION u1000000) ;; 1 STX
(define-constant MAX-CONTRIBUTION u1000000000) ;; 1000 STX

;; Define a data map to store user contributions
(define-map user-contributions principal uint)

;; Define a non-fungible token for representing stakes
(define-non-fungible-token stake uint)

;; Counter for stake IDs
(define-data-var stake-id-counter uint u0)

;; Total contributions
(define-data-var total-contributions uint u0)

(define-public (join-and-contribute (amount uint))
    (let 
        (
            (caller tx-sender)
            (current-contribution (default-to u0 (map-get? user-contributions caller)))
        )
        ;; Check if the contribution is within limits
        (asserts! (> amount (- MIN-CONTRIBUTION u1)) (err u1)) ;; Error 1: Contribution too low
        (asserts! (< amount (+ MAX-CONTRIBUTION u1)) (err u2)) ;; Error 2: Contribution too high
        
        ;; Transfer the funds to the contract
        (match (stx-transfer? amount caller (as-contract tx-sender))
            success (let ((new-stake-id (+ (var-get stake-id-counter) u1)))
                ;; Update user's contribution
                (map-set user-contributions caller (+ current-contribution amount))
                
                ;; Update total contributions
                (var-set total-contributions (+ (var-get total-contributions) amount))
                
                ;; Increment stake ID counter
                (var-set stake-id-counter new-stake-id)
                
                ;; Mint a new stake token for the user
                (match (nft-mint? stake new-stake-id caller)
                    mint-success (ok true) ;; Success: contribution added and NFT minted
                    mint-error (begin
                        ;; Revert the contribution if NFT minting fails
                        (map-set user-contributions caller current-contribution)
                        (var-set total-contributions (- (var-get total-contributions) amount))
                        (var-set stake-id-counter (- new-stake-id u1))
                        (match (as-contract (stx-transfer? amount tx-sender caller))
                            revert-success (err u4) ;; Error 4: NFT minting failed, funds returned
                            revert-error (err u5) ;; Error 5: NFT minting failed, funds stuck
                        )
                    )
                )
            )
            error (err u3) ;; Error 3: STX transfer failed
        )
    )
)

;; Function to get user's contribution
(define-read-only (get-user-contribution (user principal))
    (default-to u0 (map-get? user-contributions user))
)

;; Function to get total contributions
(define-read-only (get-total-contributions)
    (var-get total-contributions)
)

;; Function to get current stake ID
(define-read-only (get-current-stake-id)
    (var-get stake-id-counter)
)