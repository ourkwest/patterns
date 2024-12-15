(ns fabric-patterns.circles)


(def get-c%r (fn [diameter] {:radius        (float (/ diameter 2))
                             :circumference (* diameter Math/PI)}))
