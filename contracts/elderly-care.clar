;; Elderly Care Coordination Smart Contract
;; A comprehensive platform for senior services with caregiver scheduling,
;; health monitoring, family communication, and emergency response

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-EXISTS (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INVALID-PRIORITY (err u104))

;; Data Variables
(define-data-var next-senior-id uint u1)
(define-data-var next-caregiver-id uint u1)
(define-data-var next-appointment-id uint u1)
(define-data-var next-health-record-id uint u1)
(define-data-var next-emergency-id uint u1)

;; Data Maps

;; Senior profiles with basic information and emergency contacts
(define-map seniors
  uint
  {
    name: (string-ascii 50),
    age: uint,
    address: (string-ascii 100),
    emergency-contact: principal,
    family-members: (list 5 principal),
    active: bool,
    created-at: uint
  }
)

;; Caregiver profiles with qualifications and availability
(define-map caregivers
  uint
  {
    name: (string-ascii 50),
    principal: principal,
    qualifications: (string-ascii 200),
    hourly-rate: uint,
    available: bool,
    rating: uint,
    total-reviews: uint,
    created-at: uint
  }
)

;; Appointment scheduling system
(define-map appointments
  uint
  {
    senior-id: uint,
    caregiver-id: uint,
    service-type: (string-ascii 50),
    scheduled-date: uint,
    duration-hours: uint,
    status: (string-ascii 20),
    notes: (string-ascii 300),
    created-by: principal,
    created-at: uint
  }
)

;; Health monitoring records
(define-map health-records
  uint
  {
    senior-id: uint,
    record-type: (string-ascii 30),
    vital-signs: (string-ascii 200),
    medications: (string-ascii 300),
    notes: (string-ascii 500),
    recorded-by: principal,
    recorded-at: uint
  }
)

;; Emergency response system
(define-map emergencies
  uint
  {
    senior-id: uint,
    emergency-type: (string-ascii 50),
    description: (string-ascii 300),
    priority-level: uint,
    status: (string-ascii 20),
    responder: (optional principal),
    resolved-at: (optional uint),
    created-at: uint
  }
)

;; Family communication messages
(define-map family-messages
  { senior-id: uint, message-id: uint }
  {
    from: principal,
    message: (string-ascii 500),
    message-type: (string-ascii 20),
    timestamp: uint,
    read: bool
  }
)

;; Senior ownership mapping
(define-map senior-owners
  uint
  principal
)

;; Public Functions

;; Register a new senior in the system
(define-public (register-senior
    (name (string-ascii 50))
    (age uint)
    (address (string-ascii 100))
    (emergency-contact principal)
    (family-members (list 5 principal))
  )
  (let
    (
      (senior-id (var-get next-senior-id))
    )
    (map-set seniors senior-id
      {
        name: name,
        age: age,
        address: address,
        emergency-contact: emergency-contact,
        family-members: family-members,
        active: true,
        created-at: u1
      }
    )
    (map-set senior-owners senior-id tx-sender)
    (var-set next-senior-id (+ senior-id u1))
    (ok senior-id)
  )
)

;; Register a new caregiver
(define-public (register-caregiver
    (name (string-ascii 50))
    (qualifications (string-ascii 200))
    (hourly-rate uint)
  )
  (let
    (
      (caregiver-id (var-get next-caregiver-id))
    )
    (map-set caregivers caregiver-id
      {
        name: name,
        principal: tx-sender,
        qualifications: qualifications,
        hourly-rate: hourly-rate,
        available: true,
        rating: u5,
        total-reviews: u0,
        created-at: u1
      }
    )
    (var-set next-caregiver-id (+ caregiver-id u1))
    (ok caregiver-id)
  )
)

;; Schedule an appointment
(define-public (schedule-appointment
    (senior-id uint)
    (caregiver-id uint)
    (service-type (string-ascii 50))
    (scheduled-date uint)
    (duration-hours uint)
    (notes (string-ascii 300))
  )
  (let
    (
      (appointment-id (var-get next-appointment-id))
      (senior-owner (unwrap! (map-get? senior-owners senior-id) ERR-NOT-FOUND))
    )
    ;; Check if sender is authorized (senior owner or family member)
    (asserts! (is-authorized-for-senior senior-id) ERR-NOT-AUTHORIZED)
    ;; Verify senior and caregiver exist
    (asserts! (is-some (map-get? seniors senior-id)) ERR-NOT-FOUND)
    (asserts! (is-some (map-get? caregivers caregiver-id)) ERR-NOT-FOUND)
    
    (map-set appointments appointment-id
      {
        senior-id: senior-id,
        caregiver-id: caregiver-id,
        service-type: service-type,
        scheduled-date: scheduled-date,
        duration-hours: duration-hours,
        status: "scheduled",
        notes: notes,
        created-by: tx-sender,
        created-at: u1
      }
    )
    (var-set next-appointment-id (+ appointment-id u1))
    (ok appointment-id)
  )
)

;; Record health information
(define-public (record-health-data
    (senior-id uint)
    (record-type (string-ascii 30))
    (vital-signs (string-ascii 200))
    (medications (string-ascii 300))
    (notes (string-ascii 500))
  )
  (let
    (
      (health-record-id (var-get next-health-record-id))
    )
    ;; Check if sender is authorized
    (asserts! (is-authorized-for-senior senior-id) ERR-NOT-AUTHORIZED)
    ;; Verify senior exists
    (asserts! (is-some (map-get? seniors senior-id)) ERR-NOT-FOUND)
    
    (map-set health-records health-record-id
      {
        senior-id: senior-id,
        record-type: record-type,
        vital-signs: vital-signs,
        medications: medications,
        notes: notes,
        recorded-by: tx-sender,
        recorded-at: u1
      }
    )
    (var-set next-health-record-id (+ health-record-id u1))
    (ok health-record-id)
  )
)

;; Create emergency alert
(define-public (create-emergency
    (senior-id uint)
    (emergency-type (string-ascii 50))
    (description (string-ascii 300))
    (priority-level uint)
  )
  (let
    (
      (emergency-id (var-get next-emergency-id))
    )
    ;; Validate priority level (1-5)
    (asserts! (and (>= priority-level u1) (<= priority-level u5)) ERR-INVALID-PRIORITY)
    ;; Check authorization
    (asserts! (is-authorized-for-senior senior-id) ERR-NOT-AUTHORIZED)
    ;; Verify senior exists
    (asserts! (is-some (map-get? seniors senior-id)) ERR-NOT-FOUND)
    
    (map-set emergencies emergency-id
      {
        senior-id: senior-id,
        emergency-type: emergency-type,
        description: description,
        priority-level: priority-level,
        status: "active",
        responder: none,
        resolved-at: none,
        created-at: u1
      }
    )
    (var-set next-emergency-id (+ emergency-id u1))
    (ok emergency-id)
  )
)

;; Respond to emergency
(define-public (respond-to-emergency (emergency-id uint))
  (let
    (
      (emergency-data (unwrap! (map-get? emergencies emergency-id) ERR-NOT-FOUND))
    )
    ;; Check if emergency is still active
    (asserts! (is-eq (get status emergency-data) "active") ERR-INVALID-STATUS)
    
    (map-set emergencies emergency-id
      (merge emergency-data { responder: (some tx-sender), status: "responding" })
    )
    (ok true)
  )
)

;; Update appointment status
(define-public (update-appointment-status
    (appointment-id uint)
    (new-status (string-ascii 20))
  )
  (let
    (
      (appointment-data (unwrap! (map-get? appointments appointment-id) ERR-NOT-FOUND))
    )
    ;; Check if sender is the caregiver or authorized for the senior
    (asserts! (or 
      (is-caregiver-for-appointment appointment-id)
      (is-authorized-for-senior (get senior-id appointment-data))
    ) ERR-NOT-AUTHORIZED)
    
    (map-set appointments appointment-id
      (merge appointment-data { status: new-status })
    )
    (ok true)
  )
)

;; Update caregiver availability
(define-public (update-caregiver-availability (caregiver-id uint) (available bool))
  (let
    (
      (caregiver-data (unwrap! (map-get? caregivers caregiver-id) ERR-NOT-FOUND))
    )
    ;; Check if sender is the caregiver
    (asserts! (is-eq tx-sender (get principal caregiver-data)) ERR-NOT-AUTHORIZED)
    
    (map-set caregivers caregiver-id
      (merge caregiver-data { available: available })
    )
    (ok true)
  )
)

;; Read-only functions

;; Get senior information
(define-read-only (get-senior (senior-id uint))
  (map-get? seniors senior-id)
)

;; Get caregiver information
(define-read-only (get-caregiver (caregiver-id uint))
  (map-get? caregivers caregiver-id)
)

;; Get appointment details
(define-read-only (get-appointment (appointment-id uint))
  (map-get? appointments appointment-id)
)

;; Get health record
(define-read-only (get-health-record (health-record-id uint))
  (map-get? health-records health-record-id)
)

;; Get emergency details
(define-read-only (get-emergency (emergency-id uint))
  (map-get? emergencies emergency-id)
)

;; Check if user is authorized for a senior
(define-read-only (is-authorized-for-senior (senior-id uint))
  (let
    (
      (senior-owner (map-get? senior-owners senior-id))
      (senior-data (map-get? seniors senior-id))
    )
    (or
      ;; Is the senior owner
      (is-eq tx-sender (default-to CONTRACT-OWNER senior-owner))
      ;; Is a family member
      (match senior-data
        senior-info (is-some (index-of (get family-members senior-info) tx-sender))
        false
      )
      ;; Is the emergency contact
      (match senior-data
        senior-info (is-eq tx-sender (get emergency-contact senior-info))
        false
      )
    )
  )
)

;; Check if user is caregiver for specific appointment
(define-read-only (is-caregiver-for-appointment (appointment-id uint))
  (match (map-get? appointments appointment-id)
    appointment-data
      (match (map-get? caregivers (get caregiver-id appointment-data))
        caregiver-data (is-eq tx-sender (get principal caregiver-data))
        false
      )
    false
  )
)

;; Get available caregivers
(define-read-only (is-caregiver-available (caregiver-id uint))
  (match (map-get? caregivers caregiver-id)
    caregiver-data (get available caregiver-data)
    false
  )
)


;; title: elderly-care
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

