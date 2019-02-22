(ns status-im.tribute-to-talk.db
  (:require [status-im.contact.db :as contact]
            [status-im.utils.ethereum.core :as ethereum]))

(defn get-settings
  [db]
  (let [chain-keyword    (-> (get-in db [:account/account :networks (:network db)])
                             ethereum/network->chain-keyword)]
    (get-in db [:account/account :settings :tribute-to-talk chain-keyword])))

(defn enabled?
  [db]
  (:snt-amount (get-settings db)))

(defn- valid-tribute-tx?
  [db snt-amount tribute-tx-id from-public-key]
  (let [{:keys [value confirmations from]} (get-in db [:wallet :transactions tribute-tx-id])]
    (and (pos? (js/parseInt (or confirmations "0")))
         (<= snt-amount (/ value 10E18))
         (= (ethereum/address= (contact/public-key->address from-public-key)
                               from)))))

(defn filter-message
  "clojure semantics of filter, if true the message is allowed
   if it is a user message and tribute to talk is enabled, the user must be
   in the whitelist or there must be a valid tribute transaction id passed
   along the message"
  [db message-type tribute-tx-id from]
  (let [{:keys [snt-amount]} (get-settings db)
        contact (get-in db [:contacts/contacts from])]
    (or (not= :user-message message-type)
        (not snt-amount)
        ((:contacts/whitelist db) from)
        (valid-tribute-tx? db snt-amount tribute-tx-id from))))
