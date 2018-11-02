(ns indra.mobius.recipes-test
  (:require [indra.complex :as c]
            [indra.mobius :as m]
            [indra.mobius.recipes :refer :all]
            [clojure.test :refer [deftest is testing]]))

(deftest cayley-map-test
  (testing "special point mappings"
    (let [mappings [[c/zero      (c/- c/one)]
                    [c/inf       c/one]
                    [(c/- c/one) c/i]
                    [c/one       (c/- c/i)]
                    [c/i         c/zero]
                    [(c/- c/i)   c/inf]]]
      (is (every? (fn [[before after]]
                    (c/= (m/transform before cayley-map)
                         after))
                  mappings))))
  (testing "K^3 is the identity"
    (m/= cayley-map
         (m/compose cayley-map cayley-map cayley-map))))

(deftest parabolic-commutator-group-test
  (let [tr-a (c/rect 2 3)
        tr-b (c/rect 5 7)
        {:keys [a b]} (parabolic-commutator-group tr-a tr-b)
        a* (m/inverse a)
        b* (m/inverse b)
        commutator (m/compose a b a* b*)]
    (testing "tr(a) and tr(b) are preserved"
      (is (c/=* tr-a (m/trace a)))
      (is (c/=* tr-b (m/trace b))))
    (testing "commutator is parabolic"
      (is (= :parabolic
             (m/classify commutator 1e-10))))))
