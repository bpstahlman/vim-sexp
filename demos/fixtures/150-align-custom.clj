(ns demo.align-custom)

(defn settings []
 (let [host "localhost" ; connection
       port 8080      ; connection
       authentication-token-provider provider ; auth
       timeout-ms 500 ; timing
       retry-count 3       ; timing
       very-long-diagnostic-sink-name sink ; diagnostics
       sample-rate 0.1 ; diagnostics
       enabled? true]     ; state
   {:host host :port port}))
