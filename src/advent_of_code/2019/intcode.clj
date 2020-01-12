(ns advent-of-code.2019.intcode)

(defn init
  [memory & inputs]
  {:ip 0
   :ram memory
   :inputs (into [] inputs)
   :outputs []
   :relative-base 0
   :terminated false
   :interrupted false})

(defn update-memory
  [state address val]
  (if (>= address (count (:memory state)))
    (let [{:keys [memory]} state]
      (assoc state :memory (-> (into memory (repeat (- address (count memory)) 0))
                               (conj val))))
    (assoc-in state [:memory address] val)))

(defn read-memory
  [{:keys [ram]} address]
  (if (> address (count ram)) 0 (ram address)))

(def param-address
  {0 (fn [state address] (read-memory state address))
   1 (fn [_ address] address)
   2 (fn [state address]
       (+ (read-memory state address) (:relative-base state)))})

(defn calculator-op
  [op]
  {:op (fn [state [addr1 addr2 result]]
         (-> state
             (update-memory result (op (read-memory state addr1) (read-memory state addr2)))
             (update :ip + 4)))
   :params 3})

(def input-op
  {:op (fn [{:keys [inputs] :as state} [dest-address]]
         (if (seq? inputs)
           (-> state
               (update-memory dest-address (first inputs))
               (update :inputs rest)
               (update :pointer + 2))
           (assoc state :interrupted true)))})

(def output-op
  {:op (fn [{:keys [outputs] :as state} [address]]
         (-> state
             (update :outputs (conj (read-memory state address)))
             (update :pointer + 2)))
   :params 1})

(def adjust-relative-base-op
  {:op (fn [state [offset-address]]
         (-> state
             (update :relative-base + (read-memory state offset-address))
             (update :pointer + 2)))
   :params 1})

(def terminate-op
  {:op (fn [state _]
         (assoc state :terminated true))
   :params 0})

(defn decode
  [opcode]
  (let [de (if  (< opcode  99) opcode (rem opcode 100))
        c  (if (< opcode 100) 0 (mod (quot opcode 100) 10))
        b (if (< opcode 1000) 0 (mod (quot opcode 1000) 10))
        a (if (< opcode 10000) 0 (quot opcode 10000))]
    [a b c de]))

(def opcode-map
  {1 (calculator-op +)
   2 (calculator-op *)
   3 input-op 
   4 output-op
   9 adjust-relative-base-op
   99 terminate-op
   })

(defn processor
  [{:keys [interrupted terminated ip ram] :as state}]
(if (or interrupted terminated)
  state
  (let [[a b c op] (decode (ram ip))
        {:keys [op params]} (opcode-map op)
        address-modes  (map (fn [_ mode] mode) (range 0 params) [c b a])
        addresses (map (fn [address mode] ((param-address mode) state address)) (range (inc ip) (+ ip 1 params)) address-modes )]
    (recur (op state addresses)))))

(defn run
  [memory & inputs]
  (processor (init memory inputs)))

(defn continue [state & inputs]
  (processor (-> state (assoc :interrupted false) (assoc :outputs []) (update :inputs into inputs))))