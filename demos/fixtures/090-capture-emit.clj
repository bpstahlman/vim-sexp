(ns demo.capture)

(defn summarize [xs]
  (process
    (load xs)
    (validate xs)
    (normalize xs)
    (persist xs)))
