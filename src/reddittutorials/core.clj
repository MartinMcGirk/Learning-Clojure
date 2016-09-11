(ns reddittutorials.core
  (:gen-class))

;; Easy challenge #1 (https://www.reddit.com/r/dailyprogrammer/comments/pih8x/easy_challenge_1/)

(defn get-answer-for-question
  "Expects a question in the form of a string, and gets the user answer"
  [question]
  (println question)
  (clojure.string/trim (read-line)))

(defn get-answers-for-questions
  [questions]
  (map get-answer-for-question questions))

(defn easy-challenge-1
  []
  (let [answers (get-answers-for-questions ["What is your name?"
                                            "What is your age?"
                                            "What is your username?"])]
    (println (str "Your name is " (first answers)
                  ", you are " (second answers)
                  " years old, and your username is " (last answers)))))

;; Easy challenge #2 (https://www.reddit.com/r/dailyprogrammer/comments/pjbj8/easy_challenge_2/)

;;takes in seq of vectors with start work and left work and lunch values
;;and works out how may "billable" hours I spent or have left to spend at work

(defn minutes->decimal
  [minute]
  (float (/ minute 60)))

(defn convert-24-hour-time-to-decimal
  [time]
  (let [hour (subs time 0 2)
        minute (subs time 2 4)]
    (+ (Integer.  hour) (minutes->decimal (Integer. minute)))))

(defn hours-worked-during-day
  ([[start-time end-time lunch]]
   (- (convert-24-hour-time-to-decimal end-time)
      (convert-24-hour-time-to-decimal start-time)
      lunch)))

(defn hours-worked-during-week
  "Expects a seq of vectors of start and end times"
  [hours]
  (reduce (fn [total record] (+ total (hours-worked-during-day record)))
          0
          hours))

(defn hours-left-to-work-this-week
  [total worked]
  (- total worked))

(defn easy-challenge-2
  []
  (hours-left-to-work-this-week 37.5
                                (hours-worked-during-week [["0900" "1730" 1]
                                                           ["0800" "1700" 0.5]
                                                           ["0700" "1800" 1]])))
;; Easy challenge #3 (https://www.reddit.com/r/dailyprogrammer/comments/pkw2m/2112012_challenge_3_easy/)

;;Generates a map of ASCII chars using their char numbers
(def letters (map #(hash-map :letter  (str (char %))
                                      :number (Integer. %))
                           (range 0 126)))

(defn wrap-number
  "Wraps numbers around the range of numbers available in letters"
  [number]
  (let [max (count letters)
        min 1]
    (if (> number max)
      (wrap-number (- number max))
      (if (< number min)
        (wrap-number (+ max number))
        number))))

(defn get-cypherset
  "Returns a set of characters with the number associated with each letter offset"
  [baseset offset]
  (map #(assoc-in %
                  [:number]
                  (wrap-number (+ (:number %) offset)))
       baseset))

(defn get-new-letter
  "Takes two sets of characters and switches between them based on the associated number"
  [letter oldset cypherset]
  (:letter
   (first
    (filter #(= (:number %) 
                (:number
                 (first
                  (filter (fn [rec] (= (:letter rec)
                                       letter))
                          oldset))))
            cypherset))))

(defn encrypt
  [text offset]
  (let [cypherset (get-cypherset letters offset)]
    (clojure.string/join (map #(get-new-letter (str %) letters cypherset) text))))

(defn decrypt
  [text offset]
  (let [cypherset (get-cypherset letters offset)]
    (clojure.string/join (map #(get-new-letter (str %) cypherset letters) text))))

(defn easy-challenge-3
  []
  (let [key 15]
    (decrypt
     (encrypt "This is my answer to easy challenge #3" key)
     key)))
