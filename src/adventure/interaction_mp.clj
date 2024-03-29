; (ns interaction.core)
; (require '[clojure.string :as str])

; ;; # Interaction
; ;; A simple parser that can remember facts.
; ;;

; ;; # Actions
; ;;
; ;; Let an *action* be a function with a specific
; ;; interface: It will take in a *state* followed by
; ;; a sequence of arguments.  It will return a vector
; ;; consisting of a new state and a response string.
; ;;
; ;; Here is a simple example, a post-increment function.
; ;; Assume the state is a hash-map with a key :vars
; ;; and a value containing a hash of the variables and
; ;; their values.


; (defn post-increment
;   "Action: post-increment
;    Returns the value of a variable from the state and increments it."
;   [state var]
;   (if-let [val (get-in state [:vars var])]
;     [(update-in state [:vars var] inc) val]
;     [(assoc-in  state [:vars var] 1)   0  ]
;   ))


; ;; <pre><code>
; ;; interaction.core=> (post-increment {:vars {:x 10}} :x)
; ;; [{:vars {:x 11}} 10]
; ;; </code></pre>

; ;; ## Your work
; ;;
; ;; Fill in the code for these functions.
; ;;

; (defn lookup-var
;   "Given a state and a variable name, return the value of the variable
;   if it has been defined, otherwise return 0."
;   [state var]
;   [state
;   (if-let [val (get-in state [:vars var])]
;     (get-in state [:vars var])
;     0)])
; ;; <pre><code>
; ;; interaction.core=> (lookup-var {:vars {:x 10}} :x)
; ;; [{:vars {:x 10}} 10]
; ;; </code></pre>

; (defn set-var
;   "Action: set-var. Set var = e1.  Return the new value as a result."
;   [state var e1]
;   (if-let [val (get-in state [:vars e1])]
;     (def elem (get-in state [:vars e1]))
;     (def elem e1))
;   [(assoc-in  state [:vars var] elem) elem])
; ;; <pre><code>
; ;; interaction.core=> (set-var {:vars {:x 10}} :y :x)
; ;; [{:vars {:x 10 :y 10}} 10]
; ;; </code></pre>

; (defn plus
;   [state e1 e2]
;   (if-let [val (get-in state [:vars e1])]
;     (def elem1 (get-in state [:vars e1]))
;     (def elem1 e1))
;   (if-let [val (get-in state [:vars e2])]
;     (def elem2 (get-in state [:vars e2]))
;     (def elem2 e2))
;   (+ elem1 elem2))

; (defn set-plus
;   "Action: set-plus.  Set var = e1 + e2, return the sum as a result."
;   [state var e1 e2]
;   (def sum (plus state e1 e2))
;   [(assoc-in  state [:vars var] sum) sum])
; ;; <pre><code>
; ;; interaction.core=> (set-plus {:vars {:x 10}} :y :x 20)
; ;; [{:vars {:x 10 :y 30}} 30]
; ;; </code></pre>



; (defn there-is-a
;   "Action: there-is-a.  Remember that an object obj exists.
;   Returns \"There is a obj\" as a result."
;   [state object]
;   [(assoc-in  state [:objects object] []) (str "There is a " (name object) ".")])

; ;; <pre><code>
; ;; interaction.core=> (there-is-a {:vars {:x 10}} :shoe)
; ;; [{:vars {:x 10 :y 10}
; ;;   :objects {:shoe []}} "There is a shoe."]
; ;; </code></pre>

; (defn the-obj-is
;   "Action: there-obj-a.  Remember an adjective that applies to an object.
;   Returns \"The obj is adj\" as a result."
;   [state object adj]
;   (let [cur (get-in state [:objects object])]
;   (let [vec (vec (for [i (range (+ (count cur) 1))]
;     (if (= i (count cur)) adj
;     (nth cur i))))]
;   [(assoc-in state [:objects object] vec)
;   (str "The " (name object) " is " (name adj) ".")])))
; ;; <pre><code>
; ;; interaction.core=> (the-obj-is {:vars {:x 10} :objects {:shoe []}} :shoe :blue)
; ;; [{:vars {:x 10} :objects {:shoe [:blue]}} "The shoe is blue."]
; ;; </code></pre>



; (defn describe-obj
;   "Describe the given object \"The obj is adj\" if it exists in state . If not, return \"There is no obj\""
;   [state object]
;   (vec (flatten [state
;   (if-let [val (get-in state [:objects object])]
;     (for [i (range (count (get-in state [:objects object])))]
;        (str "The " (name object) " is " (name (nth (get-in state [:objects object]) i)) "."))
;     (str "There is no " (name object) "."))])))
; ;; <pre><code>
; ;; interaction.core=> (describe-obj  {:vars {:x 10} :objects {:shoe [:blue :big]}} :shoe)
; ;; [{:vars {:x 10}, :objects {:shoe [:blue :big]}} "The shoe is blue." "The shoe is big."]
; ;; </code></pre>


; (defn forget-obj
;   "Delete the given object and return \"What obj?\""
;   [state object]
;   [(assoc-in state [:objects] (dissoc (state :objects) object))
;   (str "What " (name object) "?")])
; ;; <pre><code>
; ;; interaction.core=> (forget-obj {:objects {:show [:exciting]}} :show)
; ;; [{:objects {}} "What show?"]
; ;; </code></pre>


; ;; # Action Environment
; ;;
; ;; The runtime environment is a vector of the form
; ;;
; ;; ``` [ phrase action phrase action ...]```
; ;;
; ;; The "phrase" is a canonicalized vector of keywords.
; ;; (Remember that a keyword is a symbol starting with a colon, like :name.)
; ;; The `@` character will represent a variable.

; (def initial-env [  [:postinc "@"] post-increment ])  ;; add your other functions here

; ;; # Parsing
; ;;
; ;; This is mostly code from the lecture.

; (defn canonicalize
;   "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
;   [input]
;   (vec (map keyword (str/split (str/replace (str/lower-case input) #"[?.!]" "") #" +"))))


; ;; <pre><code>
; ;; interaction.core> (canonicalize "The shoe is blue.")
; ;; [:the :shoe :is :blue]
; ;; </code></pre>

; (defn react
;   "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
;   If there is no match, return the original state and result \"I don't know what you mean.\""
;   [state input-vector]
;   (if (= (first input-vector) :postinc) (post-increment state (second input-vector))
;   (if (= (first input-vector) :lookup) (lookup-var state (second input-vector))
;   (if (= (first input-vector) :set) (if (> (count input-vector) 4)
;     (set-plus state (nth input-vector 1) (nth input-vector 3) (nth input-vector 5))
;     (set-var state (nth input-vector 1) (nth input-vector 3)))
;   (if (= (first input-vector) :there) (there-is-a state (nth input-vector 3))
;   (if (= (first input-vector) :the) (the-obj-is state (nth input-vector 1) (nth input-vector 3))
;   (if (= (first input-vector) :describe) (describe-obj state (nth input-vector 1))
;   (if (= (first input-vector) :forget) (forget-obj state (nth input-vector 1))))))))))

; ;; <pre><code>
; ;; interaction.core> (react {:vars {:x 10} :runtime initial-env} [:postinc :x])
; ;; [ {:vars {:x 11} :runtime { ... omitted for space ... }}  10]
; ;; </code></pre>

; (defn repl
;   "Start a REPL using the given environment.  The :runtime key should map to the action environment.
;   Prints out the result and loops the state for another round.  Quits when you say bye.
;   You may need (flush) to print out '>' without a newline "
;   [env]
;   )

; ;; <pre><code>
; ;; interaction.core=> (repl initial-env)
; ;; Welcome!  Let's talk.
; ;; > there is a spoon.
; ;; There is a spoon.
; ;; > the spoon is blue.
; ;; The spoon is blue.
; ;; > the spoon is big.
; ;; The spoon is big.
; ;; > describe the spoon.
; ;; The spoon is blue.
; ;; The spoon is big.
; ;; > forget the spoon.
; ;; What spoon?
; ;; > describe the spoon.
; ;; There is no spoon.
; ;; > bye
; ;; nil
; ;; interaction.core=>
; ;; </code></pre>

; (defn main
;   "Start the REPL with the initial environment."
;   []
;   (repl initial-env)
;   )
