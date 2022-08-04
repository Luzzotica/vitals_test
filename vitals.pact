;  Some functions that can be used for testing.
;  (define-keyset "vitals-admin" (read-keyset "vitals-admin"))
;  (define-keyset "vitals-user-1" (read-keyset "vitals-user-1"))
;  (define-keyset "vitals-user-2" (read-keyset "vitals-user-2"))
;  (create-table free.vitals.accounts-table)
;  (free.vitals.create-account "1" 1000000.0 (read-keyset 'vitals-admin ))
;  (free.vitals.create-account "2" 0.0 (read-keyset 'vitals-user-1 ))
;  (free.vitals.create-account "3" 0.0 (read-keyset 'vitals-user-2 ))
;  (free.vitals.add-points "2" 100.0)
;  (free.vitals.add-points "3" 100.0)
;  (select free.vitals.accounts-table ['points ] (where 'points (< 0.0)))
;  (select free.vitals.accounts-table (where 'points (= 0.0)))

(namespace "free")

(module vitals MODULE_ADMIN

  ; no-op module admin for example purposes.
  ; in a real contract this could enforce a keyset, or
  ; tally votes, etc.
  (defcap MODULE_ADMIN () 
    "enforces that only the module administrator can run the function"
    (enforce-guard "vitals-admin")); (at 'guard (coin.details "luzzotica1"))))

  ; account admin capability, also a noop for example purposes.
  (defcap ACCOUNT_ADMIN () true)

  ; user debit capability
  (defcap USER_DEBIT (account:string)
    "enforces row guard to allow debiting operations"
    (with-read accounts-table account { "guard":= guard }
      (enforce-guard guard)))

  ; define table schema
  (defschema accounts
    balance:decimal
    points:decimal
    guard:guard)

  ; define table
  (deftable accounts-table:{accounts})

  (defun create-account (account:string initial-balance keyset)
    "Create a new account for ID with INITIAL-BALANCE funds, must be administrator."
    (with-capability (ACCOUNT_ADMIN)
      (enforce (>= initial-balance 0.0) "Initial balances must be >= 0.")
      (insert accounts-table account
        { "balance": initial-balance,
          "points": 0.0,
          "guard": keyset })))

  (defun get-balance:string (account:string)
    "Read account balance."
    (at "balance" (read accounts-table account)))

  (defun pay:string (from:string to:string amount:decimal)
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
  
  (defun pay-and-reset-points:string (from:string to:string amount:decimal)
    "Make a payment debiting FROM and crediting TO for AMOUNT. Also resets the TO points to zero."
    (with-capability (MODULE_ADMIN)
      (update accounts-table to { "points": 0.0 })
      (pay from to amount)))
  
  (defun add-points (account:string amount:decimal)
    "Adds the amount of points to the user for this period."
    (with-capability (MODULE_ADMIN)
      (enforce (>= amount 0.0) "Added points must be positive.")
      (with-read accounts-table account { "points":= curr-points }
        (update accounts-table account { "points": (+ curr-points amount)}))))
    
  (defun pay-out:string (from:string amount:decimal)
    "Pay out vital tokens to those who have points accrued over the past period, zero out points"
    (with-capability (MODULE_ADMIN)
      (with-capability (USER_DEBIT from)
        (enforce (> amount 0.0) "Negative Transaction Amount")
        (with-read accounts-table from { "balance":= from-bal }
          (enforce (> from-bal amount) "Insufficient Funds")
          ;  Define lambdas and variables to handle folding to sum, and mapping to pay
          (let* (
            (qry (lambda (k obj) (< 0.0 (at 'points obj)))) 
            (handler (lambda (k obj) {'account: k, 'points: (at 'points obj)})) 
            (summer (lambda (i obj) (+ i (at 'points obj)))) 
            (rows (fold-db free.vitals.accounts-table (qry) (handler))) 
            (point-total (fold (summer) 0.0 rows)) 
            (pay-handler (lambda (amount point-total obj) (free.vitals.pay-and-reset-points "1" (at 'account obj) (* (/ (at 'points obj) point-total) amount))))
          ) 
            ;  (format "rows: {}, point-total: {}" [rows, point-total]) 
            ;  Pay out each account that had points
            (map (pay-handler amount point-total) rows))))))

  ;  (defun init ()
  ;    "Initializes all tables, accounts, and permissions"
  ;    (with-capability (MODULE_ADMIN)
  ;      (create-table accounts-table)))
)
