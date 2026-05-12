(ns demo.swap)

(defn pipeline [request]
  (-> request
      validate
      normalize
      authorize
      dispatch))
