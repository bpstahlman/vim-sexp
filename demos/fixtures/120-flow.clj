(pipeline
  (load source)
  (prepare
    (normalize
      (parse source)
      (enrich context))
    (validate rules))
  (render output))
