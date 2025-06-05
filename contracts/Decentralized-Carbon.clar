
;; title: Decentralized-Carbon
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-already-verified (err u105))
(define-constant err-not-verified (err u106))
(define-constant err-expired (err u107))
(define-constant err-invalid-price (err u108))

(define-data-var next-credit-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-map carbon-credits
  { credit-id: uint }
  {
    project-id: uint,
    issuer: principal,
    owner: principal,
    amount: uint,
    price-per-ton: uint,
    verified: bool,
    verification-data: (string-ascii 256),
    created-at: uint,
    expires-at: uint,
    retired: bool
  }
)

(define-map carbon-projects
  { project-id: uint }
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    location: (string-ascii 64),
    project-type: (string-ascii 32),
    issuer: principal,
    total-credits-issued: uint,
    verified: bool,
    iot-device-id: (string-ascii 64),
    created-at: uint
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

(define-map marketplace-listings
  { credit-id: uint }
  {
    seller: principal,
    price-per-ton: uint,
    amount-available: uint,
    listed-at: uint,
    active: bool
  }
)

(define-map iot-verifications
  { device-id: (string-ascii 64), timestamp: uint }
  {
    project-id: uint,
    carbon-offset: uint,
    temperature: int,
    humidity: uint,
    verified-by: principal,
    verification-hash: (buff 32)
  }
)

(define-map authorized-verifiers
  { verifier: principal }
  { authorized: bool }
)

(define-public (register-project (name (string-ascii 64)) (description (string-ascii 256)) (location (string-ascii 64)) (project-type (string-ascii 32)) (iot-device-id (string-ascii 64)))
  (let
    (
      (project-id (var-get next-project-id))
    )
    (map-set carbon-projects
      { project-id: project-id }
      {
        name: name,
        description: description,
        location: location,
        project-type: project-type,
        issuer: tx-sender,
        total-credits-issued: u0,
        verified: false,
        iot-device-id: iot-device-id,
        created-at: stacks-block-height
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (authorize-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-verifiers { verifier: verifier } { authorized: true })
    (ok true)
  )
)

(define-public (submit-iot-verification (device-id (string-ascii 64)) (project-id uint) (carbon-offset uint) (temperature int) (humidity uint) (verification-hash (buff 32)))
  (let
    (
      (verifier-auth (default-to { authorized: false } (map-get? authorized-verifiers { verifier: tx-sender })))
    )
    (asserts! (get authorized verifier-auth) err-unauthorized)
    (map-set iot-verifications
      { device-id: device-id, timestamp: stacks-block-height }
      {
        project-id: project-id,
        carbon-offset: carbon-offset,
        temperature: temperature,
        humidity: humidity,
        verified-by: tx-sender,
        verification-hash: verification-hash
      }
    )
    (ok true)
  )
)

(define-public (verify-project (project-id uint))
  (let
    (
      (project (unwrap! (map-get? carbon-projects { project-id: project-id }) err-not-found))
      (verifier-auth (default-to { authorized: false } (map-get? authorized-verifiers { verifier: tx-sender })))
    )
    (asserts! (get authorized verifier-auth) err-unauthorized)
    (asserts! (not (get verified project)) err-already-verified)
    (map-set carbon-projects
      { project-id: project-id }
      (merge project { verified: true })
    )
    (ok true)
  )
)

(define-public (issue-carbon-credits (project-id uint) (amount uint) (price-per-ton uint) (verification-data (string-ascii 256)) (expires-at uint))
  (let
    (
      (project (unwrap! (map-get? carbon-projects { project-id: project-id }) err-not-found))
      (credit-id (var-get next-credit-id))
    )
    (asserts! (is-eq tx-sender (get issuer project)) err-unauthorized)
    (asserts! (get verified project) err-not-verified)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> price-per-ton u0) err-invalid-price)
    (asserts! (> expires-at stacks-block-height) err-invalid-amount)
    
    (map-set carbon-credits
      { credit-id: credit-id }
      {
        project-id: project-id,
        issuer: tx-sender,
        owner: tx-sender,
        amount: amount,
        price-per-ton: price-per-ton,
        verified: true,
        verification-data: verification-data,
        created-at: stacks-block-height,
        expires-at: expires-at,
        retired: false
      }
    )
    
    (map-set carbon-projects
      { project-id: project-id }
      (merge project { total-credits-issued: (+ (get total-credits-issued project) amount) })
    )
    
    (let
      (
        (current-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
      )
      (map-set user-balances
        { user: tx-sender }
        { balance: (+ (get balance current-balance) amount) }
      )
    )
    
    (var-set next-credit-id (+ credit-id u1))
    (ok credit-id)
  )
)

(define-public (list-credits-for-sale (credit-id uint) (price-per-ton uint) (amount uint))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (user-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
    )
    (asserts! (is-eq tx-sender (get owner credit)) err-unauthorized)
    (asserts! (not (get retired credit)) err-invalid-amount)
    (asserts! (< stacks-block-height (get expires-at credit)) err-expired)
    (asserts! (>= (get balance user-balance) amount) err-insufficient-balance)
    (asserts! (> price-per-ton u0) err-invalid-price)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set marketplace-listings
      { credit-id: credit-id }
      {
        seller: tx-sender,
        price-per-ton: price-per-ton,
        amount-available: amount,
        listed-at: stacks-block-height,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (buy-carbon-credits (credit-id uint) (amount uint))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (listing (unwrap! (map-get? marketplace-listings { credit-id: credit-id }) err-not-found))
      (seller-balance (default-to { balance: u0 } (map-get? user-balances { user: (get seller listing) })))
      (buyer-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
      (total-cost (* amount (get price-per-ton listing)))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (seller-payment (- total-cost platform-fee))
    )
    (asserts! (get active listing) err-not-found)
    (asserts! (not (get retired credit)) err-invalid-amount)
    (asserts! (< stacks-block-height (get expires-at credit)) err-expired)
    (asserts! (>= (get amount-available listing) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq tx-sender (get seller listing))) err-unauthorized)
    
    (try! (stx-transfer? total-cost tx-sender (get seller listing)))
    
    (map-set user-balances
      { user: (get seller listing) }
      { balance: (- (get balance seller-balance) amount) }
    )
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (+ (get balance buyer-balance) amount) }
    )
    
    (if (is-eq (get amount-available listing) amount)
      (map-set marketplace-listings
        { credit-id: credit-id }
        (merge listing { active: false, amount-available: u0 })
      )
      (map-set marketplace-listings
        { credit-id: credit-id }
        (merge listing { amount-available: (- (get amount-available listing) amount) })
      )
    )
    
    (ok true)
  )
)

(define-public (retire-carbon-credits (credit-id uint) (amount uint))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (user-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
    )
    (asserts! (is-eq tx-sender (get owner credit)) err-unauthorized)
    (asserts! (not (get retired credit)) err-invalid-amount)
    (asserts! (>= (get balance user-balance) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (- (get balance user-balance) amount) }
    )
    
    (if (is-eq (get amount credit) amount)



      (begin
        (map-set carbon-credits
          { credit-id: credit-id }
          (merge credit { retired: true })
        )
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (transfer-credits (credit-id uint) (amount uint) (recipient principal))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (sender-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
      (recipient-balance (default-to { balance: u0 } (map-get? user-balances { user: recipient })))
    )
    (asserts! (is-eq tx-sender (get owner credit)) err-unauthorized)
    (asserts! (not (get retired credit)) err-invalid-amount)
    (asserts! (>= (get balance sender-balance) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (- (get balance sender-balance) amount) }
    )
    
    (map-set user-balances
      { user: recipient }
      { balance: (+ (get balance recipient-balance) amount) }
    )
    
    (ok true)
  )
)

(define-read-only (get-carbon-credit (credit-id uint))
  (map-get? carbon-credits { credit-id: credit-id })
)

(define-read-only (get-carbon-project (project-id uint))
  (map-get? carbon-projects { project-id: project-id })
)

(define-read-only (get-user-balance (user principal))
  (default-to { balance: u0 } (map-get? user-balances { user: user }))
)

(define-read-only (get-marketplace-listing (credit-id uint))
  (map-get? marketplace-listings { credit-id: credit-id })
)

(define-read-only (get-iot-verification (device-id (string-ascii 64)) (timestamp uint))
  (map-get? iot-verifications { device-id: device-id, timestamp: timestamp })
)

(define-read-only (is-authorized-verifier (verifier principal))
  (default-to { authorized: false } (map-get? authorized-verifiers { verifier: verifier }))
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-next-credit-id)
  (var-get next-credit-id)
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)
