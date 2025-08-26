
  ;; Pharma Supply Tracker Contract
;; A blockchain-based drug supply chain verification system
;; Prevents counterfeit medications and enables efficient recalls

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-invalid-batch (err u102))
(define-constant err-batch-exists (err u103))
(define-constant err-batch-not-found (err u104))
(define-constant err-invalid-data (err u105))
(define-constant err-batch-recalled (err u106))

;; Drug batch structure
(define-map drug-batches
  { batch-id: (string-ascii 32) }
  {
    drug-name: (string-ascii 64),
    manufacturer: principal,
    manufacture-date: uint,
    expiry-date: uint,
    serial-number: (string-ascii 32),
    ndc-code: (string-ascii 16),
    is-verified: bool,
    is-recalled: bool,
    verification-timestamp: uint,
    recall-reason: (optional (string-ascii 128))
  })

;; Authorized manufacturers
(define-map authorized-manufacturers principal bool)

;; Total verified batches counter
(define-data-var total-verified-batches uint u0)

;; Total recalled batches counter
(define-data-var total-recalled-batches uint u0)

;; Initialize authorized manufacturers (owner only)
(define-public (add-authorized-manufacturer (manufacturer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-manufacturers manufacturer true)
    (ok true)))

;; Function 1: Register and verify a new drug batch
(define-public (register-drug-batch
    (batch-id (string-ascii 32))
    (drug-name (string-ascii 64))
    (manufacturer principal)
    (manufacture-date uint)
    (expiry-date uint)
    (serial-number (string-ascii 32))
    (ndc-code (string-ascii 16)))

    (begin
      (let
        ((existing-batch (map-get? drug-batches { batch-id: batch-id }))
         (is-authorized (default-to false (map-get? authorized-manufacturers manufacturer))))

        ;; Validate inputs
        (asserts! (is-none existing-batch) err-batch-exists)
        (asserts! (> (len batch-id) u0) err-invalid-data)
        (asserts! (> (len drug-name) u0) err-invalid-data)
        (asserts! (> (len serial-number) u0) err-invalid-data)
        (asserts! (> (len ndc-code) u0) err-invalid-data)
        (asserts! (> expiry-date manufacture-date) err-invalid-data)
        ;; CORRECTED LINE: Use the 'block-height' variable directly
        (asserts! (> expiry-date stacks-block-height) err-invalid-data)

        ;; CORRECTED BLOCK: Check if manufacturer is authorized
        (asserts! is-authorized err-unauthorized)

        ;; Register the batch
        (map-set drug-batches
          { batch-id: batch-id }
          {
            drug-name: drug-name,
            manufacturer: manufacturer,
            manufacture-date: manufacture-date,
            expiry-date: expiry-date,
            serial-number: serial-number,
            ndc-code: ndc-code,
            is-verified: true,
            is-recalled: false,
            verification-timestamp: stacks-block-height,
            recall-reason: none
          })

        ;; Increment verified batches counter
        (var-set total-verified-batches (+ (var-get total-verified-batches) u1))

        ;; Print verification event
        (print {
          event: "batch-registered",
          batch-id: batch-id,
          drug-name: drug-name,
          manufacturer: manufacturer,
          verified-at: stacks-block-height,
        })

        (ok true))))

(define-read-only (get-batch-info (batch-id (string-ascii 32)))
  (match (map-get? drug-batches { batch-id: batch-id })
    batch-data (ok batch-data)
    (err err-batch-not-found)))

;; Read-only function: Verify batch authenticity and safety
(define-read-only (verify-batch-safety (batch-id (string-ascii 32)))
  (let (
    (batch-data (map-get? drug-batches { batch-id: batch-id }))
    (current-time stacks-block-height)
  )
    (match batch-data
      batch (ok {
        is-authentic: (get is-verified batch),
        is-safe: (and
          (get is-verified batch)
          (not (get is-recalled batch))
          (> (get expiry-date batch) current-time)),
        is-recalled: (get is-recalled batch),
        is-expired: (<= (get expiry-date batch) current-time),
        batch-status: (if (get is-recalled batch)
          "RECALLED"
          (if (<= (get expiry-date batch) current-time)
            "EXPIRED"
            "SAFE"))
      })
      (err err-batch-not-found))))

;; Read-only function: Check if manufacturer is authorized
(define-read-only (is-authorized-manufacturer (manufacturer principal))
  (ok (default-to false (map-get? authorized-manufacturers manufacturer))))

;; Read-only function: Get contract statistics
(define-read-only (get-contract-stats)
  (ok {
    total-verified-batches: (var-get total-verified-batches),
    total-recalled-batches: (var-get total-recalled-batches),
    active-batches: (- (var-get total-verified-batches) (var-get total-recalled-batches))
  }))

;; Read-only function: Get contract owner
(define-read-only (get-contract-owner)
    (ok contract-owner))

