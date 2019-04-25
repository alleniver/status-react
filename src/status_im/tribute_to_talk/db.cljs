(ns status-im.tribute-to-talk.db
  (:require [status-im.contact.db :as contact.db]
            [status-im.utils.fx :as fx]
            [status-im.utils.ethereum.core :as ethereum]
            [status-im.data-store.contacts :as contacts-store]))

(fx/defn add-to-whitelist
  "Add contact to whitelist"
  [{:keys [db]} public-key]
  {:db (update db :contacts/whitelist (fnil conj #{}) public-key)})

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
         (<= snt-amount (/ value 10E17))
         (= (ethereum/address= (contact.db/public-key->address from-public-key)
                               from)))))

(defn whitelisted-by? [{:keys [system-tags]}]
  (or (contains? system-tags :contact/request-received)
      (contains? system-tags :tribute-to-talk/paid)
      (contains? system-tags :tribute-to-talk/received)))

(defn whitelisted? [{:keys [system-tags]}]
  (or (contains? system-tags :contact/added)
      (contains? system-tags :tribute-to-talk/paid)
      (contains? system-tags :tribute-to-talk/received)))

(defn get-contact-whitelist
  [contacts]
  (reduce (fn [acc {:keys [public-key] :as contact}]
            (if (whitelisted? contact)
              (conj acc public-key) acc))
          (hash-set) contacts))

(defn- mark-tribute
  [{:keys [db now] :as cofx} public-key tag]
  (let [contact (-> (contact.db/public-key->contact
                     (:contacts/contacts db)
                     public-key)
                    (assoc :last-updated now)
                    (update :system-tags conj tag))]
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:contacts/contacts public-key] contact))
               :data-store/tx [(contacts-store/save-contact-tx contact)]}
              (add-to-whitelist public-key))))

(fx/defn mark-tribute-paid
  [cofx public-key]
  (mark-tribute cofx public-key :tribute-to-talk/paid))

(fx/defn mark-tribute-received
  [cofx public-key]
  (mark-tribute cofx public-key :tribute-to-talk/received))

(fx/defn enable-whitelist
  [{:keys [db] :as cofx}]
  (if (enabled? db)
    {:db (assoc db :contacts/whitelist
                (get-contact-whitelist (vals (:contacts/contacts db))))}
    {:db (dissoc db :contacts/whitelist)}))

(defn filter-message
  "clojure semantics of filter, if true the message is allowed
  if it is a user message and tribute to talk is enabled, the user must be
  in the whitelist or there must be a valid tribute transaction id passed
  along the message"
  [{:keys [db] :as cofx} received-message-fx message-type tribute-tx-id from]
  (if (not= :user-message message-type)
    received-message-fx
    (let [{:keys [snt-amount]} (get-settings db)
          contact (get-in db [:contacts/contacts from])]
      (when (or (not snt-amount)
                ((:contacts/whitelist db) from)
                (valid-tribute-tx? db snt-amount tribute-tx-id from))
        (fx/merge cofx
                  received-message-fx
                  (mark-tribute-received from))))))
