(ns bankaccount.core)

(def number-of-accounts 100)
(def number-of-money-transfers 1000)
(def number-of-accounts-per-money-transfer 5)

(def max-money-amount 10000)
(def max-transfert-amount 100)

;;============================================================
;; Accounts
(defn create-account
  []
  (ref (rand-int max-money-amount)
       :validator #(not (neg? %))))

(def accounts (into [] (take number-of-accounts (repeatedly create-account))))


(defn current-account-balances
  []
  (dosync
   (into [] (map deref accounts))))


;;============================================================
;; Transaction
(def pending-transactions (atom []))

(defn simulate-delay
  []
  (Thread/sleep 1))

(defn transfert-money
  [amount from-account to-account]
  (try
    (dosync
     (alter from-account - amount)
     (simulate-delay)
     (alter to-account + amount))
    (catch IllegalStateException _
      (swap! pending-transactions conj [amount from-account to-account]))))

(defn random-transactions-of-account
  [account]
  (apply concat
         (for [_ (range number-of-accounts-per-money-transfer)]
           (let [amount (rand-int max-transfert-amount)
                 other-account (rand-nth accounts)]
             [(future (transfert-money amount account other-account))
              (future (transfert-money amount other-account account))]))))

(defn all-random-transactions
  []
  (apply concat
         (pmap random-transactions-of-account accounts)))

(defn wait-finish-transactions
  [transactions]
  (doall (pmap deref transactions)))

(defn do-pending-transactions
  []
  (while (not (empty? @pending-transactions))
    (let [[current-transaction & the-rest] @pending-transactions]
      (apply transfert-money current-transaction)
      (reset! pending-transactions the-rest))))

(defn compare-account-balances
  [old new]
  (let [old-new-pairs (map vector old new)
        different-pairs (filter #(apply not= %) old-new-pairs)]
    (if (empty? different-pairs)
      (println "Everything went fine")
      (do
        (println "There was some errors")
        (doall (map #(println (format "-%d became %d" (first %) (second %)))
                    different-pairs))))))

(defn -main
  []
  (let [initial-account-balances (current-account-balances)
        transactions (all-random-transactions)]
    (wait-finish-transactions transactions)
    (do-pending-transactions)
    (compare-account-balances initial-account-balances (current-account-balances))))
