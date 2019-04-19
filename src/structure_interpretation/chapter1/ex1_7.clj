(ns chapter1.sqrt)

;good-enough? for very big numbers
(sqrt 100000000)

;good-enough? for very small
(/ (sqrt 0.5) (Math/sqrt 0.5))
(/ (sqrt 0.0005) (Math/sqrt 0.0005))
(/ (sqrt 0.0000005) (Math/sqrt 0.0000005))
(time (sqrt      0.0000005))
(time (Math/sqrt 0.0000005))

(sqrt 9)
(sqrt 3)
(sqrt 2)
(Math/sqrt 2)
(sqrt      0.000000001)
(Math/sqrt 0.000000001)
