;; PrivacyCompliance Tracker Contract
;; GDPR and privacy regulation compliance with automated data handling verification

;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-invalid-data (err u102))
(define-constant err-entity-not-found (err u103))
(define-constant err-already-compliant (err u104))

;; Data structures for compliance tracking
(define-map compliance-records
  principal  ;; entity address
  {
    entity-name: (string-ascii 50),
    compliance-status: (string-ascii 20),  ;; "compliant", "non-compliant", "pending"
    last-audit-date: uint,
    data-processing-purposes: (string-ascii 200),
    consent-obtained: bool,
    data-retention-period: uint,  ;; in days
    gdpr-representative: (optional principal),
    compliance-score: uint  ;; 0-100
  }
)

;; Audit trail for compliance activities
(define-map audit-trail
  {entity: principal, audit-id: uint}
  {
    auditor: principal,
    audit-date: uint,
    findings: (string-ascii 300),
    recommendations: (string-ascii 300),
    compliance-before: uint,
    compliance-after: uint
  }
)

;; Tracking variables
(define-data-var total-registered-entities uint u0)
(define-data-var total-audits uint u0)

;; Function 1: Register Entity for Compliance Tracking
;; This function allows entities to register themselves for GDPR compliance monitoring
(define-public (register-entity-compliance 
  (entity-name (string-ascii 50))
  (data-processing-purposes (string-ascii 200))
  (consent-obtained bool)
  (data-retention-period uint)
  (gdpr-representative (optional principal)))
  
  (let (
    (entity tx-sender)
    (current-block-height stacks-block-height)
  )
    ;; Validate input data
    (asserts! (> (len entity-name) u0) err-invalid-data)
    (asserts! (> (len data-processing-purposes) u0) err-invalid-data)
    (asserts! (> data-retention-period u0) err-invalid-data)
    
    ;; Check if entity is not already registered
    (asserts! (is-none (map-get? compliance-records entity)) err-already-compliant)
    
    ;; Calculate initial compliance score based on provided data
    (let (
      (base-score u30)
      (consent-bonus (if consent-obtained u25 u0))
      (retention-bonus (if (<= data-retention-period u730) u20 u10)) ;; Bonus for retention <= 2 years
      (representative-bonus (if (is-some gdpr-representative) u15 u0))
      (purpose-bonus (if (> (len data-processing-purposes) u50) u10 u5))
      (initial-score (+ base-score consent-bonus retention-bonus representative-bonus purpose-bonus))
    )
      
      ;; Register the entity
      (map-set compliance-records entity {
        entity-name: entity-name,
        compliance-status: "pending",
        last-audit-date: current-block-height,
        data-processing-purposes: data-processing-purposes,
        consent-obtained: consent-obtained,
        data-retention-period: data-retention-period,
        gdpr-representative: gdpr-representative,
        compliance-score: initial-score
      })
      
      ;; Update total registered entities
      (var-set total-registered-entities (+ (var-get total-registered-entities) u1))
      
      (ok {
        entity: entity,
        initial-score: initial-score,
        status: "registered",
        message: "Entity successfully registered for GDPR compliance tracking"
      })
    )
  )
)

;; Function 2: Conduct Compliance Audit
;; This function allows authorized auditors to conduct compliance audits and update entity status
(define-public (conduct-compliance-audit
  (target-entity principal)
  (findings (string-ascii 300))
  (recommendations (string-ascii 300))
  (new-compliance-score uint))
  
  (let (
    (auditor tx-sender)
    (current-audit-id (+ (var-get total-audits) u1))
    (current-block-height stacks-block-height)
    (entity-record (map-get? compliance-records target-entity))
  )
    ;; Validate inputs
    (asserts! (> (len findings) u0) err-invalid-data)
    (asserts! (<= new-compliance-score u100) err-invalid-data)
    
    ;; Check if entity exists
    (asserts! (is-some entity-record) err-entity-not-found)
    
    ;; Only contract owner or designated auditors can conduct audits
    (asserts! (is-eq auditor contract-owner) err-unauthorized)
    
    (match entity-record current-record
      (let (
        (previous-score (get compliance-score current-record))
        (new-status 
          (if (>= new-compliance-score u80) 
            "compliant" 
            (if (>= new-compliance-score u50) 
              "pending" 
              "non-compliant")))
      )
        
        ;; Update compliance record
        (map-set compliance-records target-entity
          (merge current-record {
            compliance-status: new-status,
            last-audit-date: current-block-height,
            compliance-score: new-compliance-score
          })
        )
        
        ;; Record audit trail
        (map-set audit-trail 
          {entity: target-entity, audit-id: current-audit-id}
          {
            auditor: auditor,
            audit-date: current-block-height,
            findings: findings,
            recommendations: recommendations,
            compliance-before: previous-score,
            compliance-after: new-compliance-score
          }
        )
        
        ;; Update audit counter
        (var-set total-audits current-audit-id)
        
        (ok {
          entity: target-entity,
          audit-id: current-audit-id,
          previous-score: previous-score,
          new-score: new-compliance-score,
          status: new-status,
          message: "Compliance audit completed successfully"
        })
      )
      err-entity-not-found
    )
  )
)

;; Read-only functions for querying compliance data

;; Get entity compliance record
(define-read-only (get-compliance-record (entity principal))
  (ok (map-get? compliance-records entity))
)

;; Get audit details
(define-read-only (get-audit-record (entity principal) (audit-id uint))
  (ok (map-get? audit-trail {entity: entity, audit-id: audit-id}))
)

;; Get total statistics
(define-read-only (get-compliance-statistics)
  (ok {
    total-entities: (var-get total-registered-entities),
    total-audits: (var-get total-audits),
    contract-owner: contract-owner
  })
)