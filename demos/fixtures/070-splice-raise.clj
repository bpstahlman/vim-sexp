(ns demo.structure)

(defn prepare-route [request]
  (let [event (parse-request request)
        route (lookup-route event)]
    (handle-event
      (authenticate event)
      (authorize route)
      (dispatch route event))))

