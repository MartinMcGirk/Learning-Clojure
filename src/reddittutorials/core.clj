(ns reddittutorials.core
  (:gen-class))

;; Easy challenge #1 (https://www.reddit.com/r/dailyprogrammer/comments/pih8x/easy_challenge_1/)

;; Ask the user some questions, and replay the answers to them in a string

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

;; Build a calculator of some sort that could be of use to you in your daily
;; life.

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

;; Create a program to encrypt or decrypt text using a caesar cypher

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

;; Easy challenge #4 (https://www.reddit.com/r/dailyprogrammer/comments/pm6oj/2122012_challenge_4_easy/)

;; Create a random password generator that will create x passwords of y length

(def get-rand-character
  #(char (rand-nth (range 33 127))))

(defn get-password
  [length]
  (clojure.string/join
   (take length (repeatedly #(get-rand-character)))))

(defn get-multiple-passwords
  [number length]
  (take number (repeatedly #(get-password length))))

(defn easy-challenge-4
  []
  (get-multiple-passwords 10 15))

;; Easy challenge #5 (https://www.reddit.com/r/dailyprogrammer/comments/pnhyn/2122012_challenge_5_easy/)

;; Create a program that is password protected

(defn get-credentials
  []
  (vector (get-answer-for-question "Username?")
          (get-answer-for-question "Password?")))

(defn run-program
  []
  (let [[username password] (get-credentials)]
    (if (and (= username "a")
             (= password "b"))
      (println "Access Granted!")
      (println "Access Denied!")))) 
                        
(defn easy-challenge-5
  []
  (run-program))

;; Easy challenge #7 (https://www.reddit.com/r/dailyprogrammer/comments/pr2xr/2152012_challenge_7_easy/)

;; Write a morse code decrypter

(def input-morse ".... . .-.. .-.. --- / -.. .- .. .-.. -.-- / .--. .-. --- --. .-. .- -- -- . .-. / --. --- --- -.. / .-.. ..- -.-. -.- / --- -. / - .... . / -.-. .... .- .-.. .-.. . -. --. . ... / - --- -.. .- -.--")

(def morse-alphabet {"/" " ", ".-" "A", "-..." "B", "-.-." "C"
                     "-.." "D", "." "E", "..-." "F", "--." "G"
                     "...." "H", ".." "I", ".---" "J", "-.-" "K"
                     ".-.." "L", "--" "M", "-." "N", "---" "O"
                     ".--." "P", "--.-" "Q", ".-." "R", "..." "S"
                     "-" "T", "..-" "U", "...-" "V", ".--" "W"
                     "-..-" "X", "-.--" "Y", "--.." "Z"})

(defn split-using-delimiter
  [text delimiter]
  (clojure.string/split text delimiter))

(defn morse->string
  [morse]
  (clojure.string/join (map #(morse-alphabet %) (split-using-delimiter input-morse #" "))))

(defn easy-challenge-7
  []
  (morse->string input-morse))

;; Easy challenge #8 (https://www.reddit.com/r/dailyprogrammer/comments/pserp/2162012_challenge_8_easy/)

;; Sing "99 bottles of beer on the wall" song

(defn sing-song
  [num]
  (if (zero? num)
    (println "END")
    (let [nxt (dec num)]
      (println (str num " bottles of beer on the wall, " num " bottles of beer... Take one down, pass it around, "
                    nxt " bottles of beer on the wall"))
      (recur nxt))))

(defn easy-challenge-8
  []
  (sing-song 99))