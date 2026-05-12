(ns demo.cleanup)

(defn totals [orders]
 (let [open-orders   (filter open? orders)      ; waiting on payment
       late-orders (filter late? orders) ; needs follow-up
       shipped-orders     (filter shipped? orders)         ; already sent
       totals  (map total orders)] ; all order totals
   {:open open-orders      ; actionable
    :late late-orders ; urgent
    :shipped shipped-orders       ; archive
    :total (reduce + totals)})) ; dashboard number
