(ns demo.selection)

(defn route-request [request]
  (let [route (lookup-route request)
        user (authenticate request)
        permitted? (authorize user route)]
    (handle route user permitted?)))
