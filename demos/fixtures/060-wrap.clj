(ns demo.wrap
  (:require [clojure.string :as str]))

(defn normalize-user [user]
  {:id (:id user)
   :name str/trim (:name user)
   :email (str/lower-case (:email user))})

