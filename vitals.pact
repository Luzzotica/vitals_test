;;
;; 
;;

(namespace "free")

(module vitals MODULE_ADMIN

  ; no-op module admin for example purposes.
  ; in a real contract this could enforce a keyset, or
  ; tally votes, etc.
  (defcap MODULE_ADMIN () 
    "enforces that only the module administrator can run the function"
    (enforce-guard (at 'guard (coin.details "luzzotica1"))))

  ; account admin capability, also a noop for example purposes.
  (defcap ACCOUNT_ADMIN () true)

  ; user debit capability
  (defcap USER_DEBIT (user-id)
    "enforces row guard to allow debiting operations"
    (with-read accounts-table user-id { "guard":= guard }
      (enforce-guard guard)))

  ; define table schema
  (defschema accounts
    balance:decimal
    points:decimal
    guard:guard)

  ; define table
  (deftable accounts-table:{accounts})

  (defun create-account (id initial-balance keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (with-capability (ACCOUNT_ADMIN)
      (enforce (>= initial-balance 0.0) "Initial balances must be >= 0.")
      (insert accounts-table id
        { "balance": initial-balance,
          "points": 0.0,
          "guard": keyset })))

  (defun get-balance (id)
    "Read account balance."
    (at "balance" (read accounts-table id)))

  (defun pay (from to amount)
    "Make a payment debiting FROM and crediting TO for AMOUNT."
    (with-capability (USER_DEBIT from)
      (with-read accounts-table from { "balance":= from-bal }
        (with-read accounts-table to { "balance":= to-bal }
          (enforce (> amount 0.0) "Negative Transaction Amount")
          (enforce (>= from-bal amount) "Insufficient Funds")
          (update accounts-table from
                  { "balance": (- from-bal amount) })
          (update accounts-table to
                  { "balance": (+ to-bal amount) })
          (format "{} paid {} {}" [from to amount])))))
  
  (defun add-points (id amount)
    "Adds the amount of points to the user for this period."
    (with-capability (MODULE_ADMIN)
      (enforce (>= amount 0.0) "Added points must be positive.")
      (with-read accounts-table id { "points":= curr-points }
        (update accounts-table id { "points": (+ curr-points amount)})))
    
  (defun pay-out (from amount)
    "Pay out vital tokens to those who have points accrued over the past period, zero out points"
    (with-capability (MODULE_ADMIN)
      (enforce (> amount 0.0) "Negative Transaction Amount")
      ;  Get the point values as a list and sum them up using fold
      (let (point-values (select accounts-table ['points ] (where 'points (> 0.0))))
        (let (total-points (fold (+) 0 point-values))
        ;  Get each participant, and map the list to pay each one their weighted amount based on points
          )))
      ;  (with-read accounts-table from { "balance":= from-bal }
        
      ;    (enforce (>= from-bal amount) "Insufficient Funds")
      ;    (update accounts-table from
      ;            { "balance": (- from-bal amount) })
      ;    (update accounts-table to
      ;            { "balance": (+ to-bal amount) })
      ;    (format "{} paid {} {}" [from to amount]))))

)

;define table
(create-table accounts-table)

;;;; create accounts
; (env-data { "sarah-keyset": ["sarah"], "james-keyset": ["james"] })
; (use payments)
; (create-account "Sarah" 100.25 (read-keyset "sarah-keyset"))
; (create-account "James" 250.0 (read-keyset "james-keyset"))


;;;; simulate SARAH keyset.
; (env-keys ["sarah"])

;;;; show failure trying to debit James with Sarah's key
; (pay "James" "Sarah" 25.0)

;;;; success Sarah paying James with Sarah's key
; (pay "Sarah" "James" 25.0)
; (format "Sarah's balance is {}" [(get-balance "Sarah")])
; (format "James's balance is {}" [(get-balance "James")])
