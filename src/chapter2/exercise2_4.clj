(ns chapter2.exercise2-4)

(defn cons [x y]
  (fn [m]
    (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(def c1 (cons 1 2))

(car c1)
(cdr c1)

;The cons needs to return a dispatcher procedure 
;that gets a message. 
;The message is a procedure in itself 
;and matches the arguments it is being passed inside the dispatcher procedure
;The message defines what it should return, while to the dispatcher it's only being passed
;objects of the message type with the desired or expected behavior.
;Looks very familiar with the command pattern, just that instead of calling the function
;on a command object, we're passing in the message object and define the return in the message procedure

(defn carr [x y]
  (letfn [(cons [x y] 
                ;letfn is used here just to explicitly name the procedure's behavior.
                (letfn [(dispatch [message] (message x y))]
                  dispatch))
          (car  [dispatch] (dispatch (fn [p q] p)))
          (cdr  [dispatch] (dispatch (fn [p q] q)))]
    (car (cons x y))))

(carr 1 2)