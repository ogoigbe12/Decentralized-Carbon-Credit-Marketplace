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

(define-constant err-not-staked (err u109))
(define-constant err-staking-period-not-ended (err u110))
(define-constant err-already-staked (err u111))
(define-constant stake-reward-rate u500)

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



(define-map staked-credits
  { user: principal, stake-id: uint }
  {
    credit-id: uint,
    amount: uint,
    staked-at: uint,
    stake-duration: uint,
    reward-rate: uint,
    active: bool
  }
)

(define-data-var next-stake-id uint u1)

(define-public (stake-carbon-credits (credit-id uint) (amount uint) (duration-blocks uint))
  (let
    (
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (user-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
      (stake-id (var-get next-stake-id))
    )
    (asserts! (is-eq tx-sender (get owner credit)) err-unauthorized)
    (asserts! (not (get retired credit)) err-invalid-amount)
    (asserts! (>= (get balance user-balance) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= duration-blocks u1000) err-invalid-amount)
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (- (get balance user-balance) amount) }
    )
    
    (map-set staked-credits
      { user: tx-sender, stake-id: stake-id }
      {
        credit-id: credit-id,
        amount: amount,
        staked-at: stacks-block-height,
        stake-duration: duration-blocks,
        reward-rate: stake-reward-rate,
        active: true
      }
    )
    
    (var-set next-stake-id (+ stake-id u1))
    (ok stake-id)
  )
)

(define-public (unstake-carbon-credits (stake-id uint))
  (let
    (
      (stake (unwrap! (map-get? staked-credits { user: tx-sender, stake-id: stake-id }) err-not-staked))
      (user-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
      (staking-end-block (+ (get staked-at stake) (get stake-duration stake)))
      (reward-amount (/ (* (get amount stake) (get reward-rate stake)) u10000))
    )
    (asserts! (get active stake) err-not-staked)
    (asserts! (>= stacks-block-height staking-end-block) err-staking-period-not-ended)
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (+ (get balance user-balance) (get amount stake) reward-amount) }
    )
    
    (map-set staked-credits
      { user: tx-sender, stake-id: stake-id }
      (merge stake { active: false })
    )
    
    (ok { original-amount: (get amount stake), reward-amount: reward-amount })
  )
)

(define-read-only (get-stake-info (user principal) (stake-id uint))
  (map-get? staked-credits { user: user, stake-id: stake-id })
)

(define-read-only (calculate-stake-reward (user principal) (stake-id uint))
  (match (map-get? staked-credits { user: user, stake-id: stake-id })
    stake (/ (* (get amount stake) (get reward-rate stake)) u10000)
    u0
  )
)

(define-map escrow-agreements
  { escrow-id: uint }
  {
    buyer: principal,
    seller: principal,
    credit-id: uint,
    amount: uint,
    stx-amount: uint,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 16),
    requires-verification: bool,
    arbitrator: (optional principal)
  }
)

(define-map escrow-funds
  { escrow-id: uint }
  { stx-held: uint, credits-held: uint }
)

(define-data-var next-escrow-id uint u1)
(define-constant escrow-duration-blocks u1440)

(define-public (create-escrow (seller principal) (credit-id uint) (amount uint) (stx-amount uint) (requires-verification bool) (arbitrator (optional principal)))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (seller-balance (default-to { balance: u0 } (map-get? user-balances { user: seller })))
    )
    (asserts! (>= (get balance seller-balance) amount) err-insufficient-balance)
    (asserts! (> stx-amount u0) err-invalid-amount)
    (asserts! (> amount u0) err-invalid-amount)
    
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    
    (map-set user-balances
      { user: seller }
      { balance: (- (get balance seller-balance) amount) }
    )
    
    (map-set escrow-agreements
      { escrow-id: escrow-id }
      {
        buyer: tx-sender,
        seller: seller,
        credit-id: credit-id,
        amount: amount,
        stx-amount: stx-amount,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height escrow-duration-blocks),
        status: "active",
        requires-verification: requires-verification,
        arbitrator: arbitrator
      }
    )
    
    (map-set escrow-funds
      { escrow-id: escrow-id }
      { stx-held: stx-amount, credits-held: amount }
    )
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-public (release-escrow (escrow-id uint))
  (let
    (
      (agreement (unwrap! (map-get? escrow-agreements { escrow-id: escrow-id }) err-not-found))
      (funds (unwrap! (map-get? escrow-funds { escrow-id: escrow-id }) err-not-found))
      (buyer-balance (default-to { balance: u0 } (map-get? user-balances { user: (get buyer agreement) })))
    )
    (asserts! (is-eq (get status agreement) "active") err-invalid-amount)
    (asserts! (or (is-eq tx-sender (get buyer agreement)) (is-eq tx-sender (get seller agreement))) err-unauthorized)
    
    (try! (as-contract (stx-transfer? (get stx-held funds) tx-sender (get seller agreement))))
    
    (map-set user-balances
      { user: (get buyer agreement) }
      { balance: (+ (get balance buyer-balance) (get credits-held funds)) }
    )
    
    (map-set escrow-agreements
      { escrow-id: escrow-id }
      (merge agreement { status: "completed" })
    )
    
    (ok true)
  )
)

(define-map credit-audit-trail
  { credit-id: uint, event-id: uint }
  {
    event-type: (string-ascii 16),
    from-user: (optional principal),
    to-user: (optional principal),
    amount: uint,
    price-per-ton: (optional uint),
    stx-amount: (optional uint),
    block-height: uint,
    transaction-memo: (string-ascii 128),
    verified-by: (optional principal)
  }
)

(define-map credit-event-counters
  { credit-id: uint }
  { total-events: uint }
)

(define-private (log-credit-event (credit-id uint) (event-type (string-ascii 16)) (from-user (optional principal)) (to-user (optional principal)) (amount uint) (price-per-ton (optional uint)) (stx-amount (optional uint)) (memo (string-ascii 128)) (verifier (optional principal)))
  (let
    (
      (event-counter (default-to { total-events: u0 } (map-get? credit-event-counters { credit-id: credit-id })))
      (event-id (get total-events event-counter))
    )
    (map-set credit-audit-trail
      { credit-id: credit-id, event-id: event-id }
      {
        event-type: event-type,
        from-user: from-user,
        to-user: to-user,
        amount: amount,
        price-per-ton: price-per-ton,
        stx-amount: stx-amount,
        block-height: stacks-block-height,
        transaction-memo: memo,
        verified-by: verifier
      }
    )
    (map-set credit-event-counters
      { credit-id: credit-id }
      { total-events: (+ event-id u1) }
    )
    (ok true)
  )
)

(define-public (log-credit-issuance (credit-id uint) (issuer principal) (amount uint) (memo (string-ascii 128)))
  (log-credit-event credit-id "issuance" none (some issuer) amount none none memo (some tx-sender))
)

(define-public (log-credit-transfer (credit-id uint) (from-user principal) (to-user principal) (amount uint) (memo (string-ascii 128)))
  (log-credit-event credit-id "transfer" (some from-user) (some to-user) amount none none memo none)
)

(define-public (log-credit-sale (credit-id uint) (seller principal) (buyer principal) (amount uint) (price-per-ton uint) (stx-amount uint) (memo (string-ascii 128)))
  (log-credit-event credit-id "sale" (some seller) (some buyer) amount (some price-per-ton) (some stx-amount) memo none)
)

(define-public (log-credit-retirement (credit-id uint) (owner principal) (amount uint) (memo (string-ascii 128)))
  (log-credit-event credit-id "retirement" (some owner) none amount none none memo none)
)

(define-read-only (get-credit-audit-event (credit-id uint) (event-id uint))
  (map-get? credit-audit-trail { credit-id: credit-id, event-id: event-id })
)

(define-read-only (get-credit-event-count (credit-id uint))
  (default-to { total-events: u0 } (map-get? credit-event-counters { credit-id: credit-id }))
)

(define-read-only (get-credit-full-history (credit-id uint))
  (let
    (
      (event-count (get total-events (get-credit-event-count credit-id)))
    )
    (map get-credit-audit-event (list credit-id credit-id credit-id credit-id credit-id) (list u0 u1 u2 u3 u4))
  )
)


(define-map batch-trades
  { batch-id: uint }
  {
    buyer: principal,
    total-stx-spent: uint,
    total-credits-acquired: uint,
    items-count: uint,
    executed-at: uint,
    status: (string-ascii 16)
  }
)

(define-map batch-trade-items
  { batch-id: uint, item-index: uint }
  {
    credit-id: uint,
    seller: principal,
    amount: uint,
    price-per-ton: uint,
    stx-paid: uint
  }
)

(define-data-var next-batch-id uint u1)

(define-public (batch-purchase-credits (trades (list 10 { credit-id: uint, amount: uint })))
  (let
    (
      (batch-id (var-get next-batch-id))
      (result (fold process-batch-trade-item trades { index: u0, total-stx: u0, total-credits: u0, success: true }))
    )
    (asserts! (get success result) err-invalid-amount)
    (asserts! (> (get total-credits result) u0) err-invalid-amount)
    
    (map-set batch-trades
      { batch-id: batch-id }
      {
        buyer: tx-sender,
        total-stx-spent: (get total-stx result),
        total-credits-acquired: (get total-credits result),
        items-count: (get index result),
        executed-at: stacks-block-height,
        status: "completed"
      }
    )
    
    (var-set next-batch-id (+ batch-id u1))
    (ok { batch-id: batch-id, items-processed: (get index result), total-credits: (get total-credits result) })
  )
)

(define-private (process-batch-trade-item 
  (trade-item { credit-id: uint, amount: uint })
  (accumulator { index: uint, total-stx: uint, total-credits: uint, success: bool })
)
  (if (not (get success accumulator))
    accumulator
    (match (execute-single-batch-trade (get credit-id trade-item) (get amount trade-item) (get index accumulator))
      trade-result (merge accumulator { 
        index: (+ (get index accumulator) u1),
        total-stx: (+ (get total-stx accumulator) (get stx-cost trade-result)),
        total-credits: (+ (get total-credits accumulator) (get amount trade-item)),
        success: true 
      })
      error-val (merge accumulator { success: false })
    )
  )
)

(define-private (execute-single-batch-trade (credit-id uint) (amount uint) (item-index uint))
  (let
    (
      (listing (unwrap! (map-get? marketplace-listings { credit-id: credit-id }) err-not-found))
      (credit (unwrap! (map-get? carbon-credits { credit-id: credit-id }) err-not-found))
      (seller-balance (default-to { balance: u0 } (map-get? user-balances { user: (get seller listing) })))
      (buyer-balance (default-to { balance: u0 } (map-get? user-balances { user: tx-sender })))
      (total-cost (* amount (get price-per-ton listing)))
    )
    (asserts! (get active listing) err-not-found)
    (asserts! (not (get retired credit)) err-invalid-amount)
    (asserts! (>= (get amount-available listing) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    
    (unwrap! (stx-transfer? total-cost tx-sender (get seller listing)) err-insufficient-balance)
    
    (map-set user-balances { user: (get seller listing) } { balance: (- (get balance seller-balance) amount) })
    (map-set user-balances { user: tx-sender } { balance: (+ (get balance buyer-balance) amount) })
    
    (map-set marketplace-listings
      { credit-id: credit-id }
      (merge listing { 
        amount-available: (- (get amount-available listing) amount),
        active: (> (- (get amount-available listing) amount) u0)
      })
    )
    
    (ok { stx-cost: total-cost, seller: (get seller listing) })
  )
)

(define-read-only (get-batch-trade (batch-id uint))
  (map-get? batch-trades { batch-id: batch-id })
)

(define-read-only (get-batch-trade-item (batch-id uint) (item-index uint))
  (map-get? batch-trade-items { batch-id: batch-id, item-index: item-index })
)