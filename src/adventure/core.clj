
(ns adventure.core
  (:gen-class))

(require '[clojure.string :as str])

(def init-map
  {:lobby {:desc "You are in Siebel Lobby, you hear shrieking and yelling from far away. You suspect that it is coming from the upper floors"
           :title "Siebel Lobby"
           :dir {:north :elevator :east :advising }
           :contents #{}}
   :elevator {:desc "The first room"
            :title "Siebel Elevator"
            :dir {:down :basement :up :upper :north :einstein :south :lobby}
            :contents #{}}
   :basement {:desc "You are in the Siebel Basement"
            :title "Siebel Basement"
            :dir {:up :elevator :east :ews}
            :contents #{}}
   :ews {:desc "You see there are students cowering under the desks. You quickly run to one of the computers that is already logged in."
            :title "Engineering Workstations"
            :dir {:west :basement }
            :contents #{:usb}}
   :upper {:desc "You see the new Bosstown Dynamics robots rising up against a group of researchers."
            :title "Third Floor Lobby"
            :dir {:west :robot :east :room6 }
            :contents #{}}
    :advising {:desc "The fifth room"
             :title "You are in the CS Advising room, and spot a dog like robot who gut-kicks you in the stomach."
             :dir {:west :lobby}
             :contents #{}}
    :room6 {:desc "You are trapped in the conference room with another robot that has escaped."
             :title "Room 3124"
             :dir {:west :upper}
             :contents #{}}
    :robot {:desc "You are in the room with the Bosstown Dynamics robots, it is your job to insert the infected USB into the central server to shut them down."
             :title "Robotics Labratory"
             :dir {:east :upper}
             :contents #{}}
    :einstein {:desc "You are at einstein bagels you should replenish yourself."
             :title "Einstein Bagels"
             :dir {:south :lobby}
             :contents #{:bagel}}
  })

(def init-items
    {:usb {:desc "USB to load the virus onto"
               :name "USB Key (use \"usb\" as name)" }
    :bagel {:desc "Your bagel with cream cheese"
               :name "Bagel With Cream Cheese (use \"bagel\" as name)" }
    :infected {:desc "The USB with the virus"
               :name "Infected USB (use \"infected\" as name)" }

  })

(def init-adventurer
    {:location :lobby
     :inventory #{}
     :health 250
     :intelligence 10
     :tick 0
     :seen #{:lobby}})

(def init-state {:map init-map :items init-items :adventurer init-adventurer})


(defn current [state]
  (println "map: ")
    (def rooms [:lobby :elevator :basement :upper :advising :room6 :robot :room8])
    (doseq [i (range (count rooms))]
      (println "  " (nth rooms i) "  " (get-in state [:map (nth rooms i)])))
  (println "adventurer: ")
    (def adv-attributes [:location :inventory :health :intelligence :tick :seen])
    (doseq [i (range (count adv-attributes))]
      (println "  " (nth adv-attributes i) "  " (get-in state [:adventurer (nth adv-attributes i)]))))


(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (let[new-state  (assoc-in state [:adventurer :location] dest)]
        (if-let [val (get-in state [:adventurer :seen dest])]
          (println (get-in state [:map dest :title]))
          (println (get-in state [:map dest :desc])))
        (if (<= (get-in state [:adventurer :health]) 0) 
        (do (println "You have died. Goodbye. ") (System/exit 0))
        (update-in (update-in (update-in new-state [:adventurer :health] - 10) [:adventurer :seen] #(conj % dest)) [:adventurer :tick] #(inc %))
        )
      ))))

(defn eat [state]
  (let [inventory (get-in state [:inventory])]
    (if (contains? inventory :bagel)
      (do (println "Sadly you don't have anything to eat... Maybe go to einsteins?") state)
      (do (println "You eat your food and now you are full of energy!") (update-in state [:adventurer :health] + 200) ))))


(defn examine [state item]
  (if-let [val (get-in state [:map (get-in state [:adventurer :location]) :contents item])]
    (println (get-in state [:items item :desc]))
    (if-let [val (get-in state [:adventurer :inventory item])]
      (println (get-in state [:items item :desc]))
      (println "item not in room or inventory")))
  state)

(defn look [state]
  (do
    (print (get-in state [:map (get-in state [:adventurer :location]) :desc]))
    (let [items (vec (get-in state [:map (get-in state [:adventurer :location]) :contents]))]
    (print ". Items: ")
      (if (= (count items) 0)
        (print "none. ")
      (doseq [i (range (count items))]
        (print (get-in init-state [:items (items i) :name]))
        (if (= i (- (count items) 1))
          (print ". ")
          (print ", ")))))
    (let [dirs (vec (get-in state [:map (get-in state [:adventurer :location]) :dir]))]
    (print "Paths: ")
      (if (= (count dirs) 0)
        (println "none. ")
      (doseq [i (range (/ (count dirs) 1))]
        (if (= (+ i 1) (count dirs))
          (println (name ((dirs i) 0)) (str "- "(name ((dirs i) 1)) ". "))
          (print (name ((dirs i) 0)) (str "- "(name ((dirs i) 1)) ", "))
          ))))
  state))

(defn inventory [state]
  (let [items (vec (get-in state [:adventurer :inventory]))]
    (if (= (count items) 0)
      (println "Your inventory is empty. ")
      (do
      (print "inventory: ")
      (doseq [i (range (count items))]
        (print (get-in init-state [:items (items i) :name]))
        (if (= i (- (count items) 1))
          (println ".")
          (print ", ")
        ))))
  state))

(defn take [state item]
  (inventory
  (if-let [val (get-in state [:map (get-in state [:adventurer :location]) :contents item])]
    (let [new-state (update-in state [:adventurer :inventory] #(conj % item))]
      (let [updated-state (update-in new-state [:map (get-in state [:adventurer :location]) :contents] #(disj % item))]
        ;(current updated-state)
        updated-state))
  state)))

(defn drop [state item]
  (inventory
  (if-let [val (get-in state [:adventurer :inventory item])]
    (let [new-state (update-in state [:adventurer :inventory] #(disj % item))]
      (let [updated-state (update-in new-state [:map (get-in state [:adventurer :location]) :contents] #(conj % item))]
          updated-state))
  state)))

(defn successful-transform [state]
  (def new-state (update-in (update-in (update-in state [:adventurer :intelligence] + 20) [:adventurer :inventory] #(disj % :usb)) [:adventurer :inventory] #(conj % :infected)))
  (println "The Virus has been uploaded to transform the USB into the infected USB!")
  new-state
)

(defn transform [state item]
  (if (= item :usb)
    (do
      (if-let [val (get-in state [:adventurer :inventory :usb])]
        (if (= :advising (get-in state [:adventurer :location]))
          (def new-state (successful-transform state))
          (do (print "You can't upload the virus here") (def new-state state)))
        (do (print "You dont have the USB") (def new-state state)))
        new-state
      )
    (do
      (print "you can't infect can't that!")
      state
      ))
)

(defn help [state]
  (println "type 'go north' or 'n' to go north")
  (println "type 'look' to look around")
  (println "type 'take obj' to pick up an object")
  (println "type 'drop obj' to drop an object")
  (println "type 'i' to look at inventory")
  (println "type 'transform obj' to try to transform the object")
  (println "goal of the game: find a USB key in the EWS, take it to advising, transform it to an Infected USB, then place the Infected USB in the Robotics Lab")

  state)


(defn match [pattern input]
  (loop [pattern pattern
    input input
    vars '()]
  (cond (and (empty? pattern) (empty? input)) (reverse vars)
    (or (empty? pattern) (empty? input)) nil
      (= (first pattern) "@")
        (recur (rest pattern)
        (rest input)
    (cons (first input) vars))
      (= (first pattern) (first input))
        (recur (rest pattern)
        (rest input)
      vars)
      :error nil )))

(defn base [state]
	   [["@"]
        (fn [x]
          (if (or (= x "n") (= x "north")) (go state :north)
          (if (or (= x "e") (= x "east" )) (go state :east)
          (if (or (= x "s") (= x "south")) (go state :south)
          (if (or (= x "w") (= x "west" )) (go state :west)
          (if (or (= x "u") (= x "up" )) (go state :up)
          (if (or (= x "d") (= x "down" )) (go state :down)
          (if (= x "look") (look state)
          (if (= x "eat") (eat state)
          (if (= x "help") (help state)
          (if (or (= x "i") (= x "inventory")) (inventory state)
          (if (= x "print") (do (current state) state)
          (let [ret "invalid input"] (println ret) state)))))))))))))
     ["go" "@"]
        (fn [x] (go state (keyword x)))
     ["examine" "@"]
        (fn [x] (examine state (keyword x)))
     ["take" "@"]
        (fn [x] (take state (keyword x)))
     ["drop" "@"]
        (fn [x] (drop state (keyword x)))
     ["transform" "@"]
        (fn [x] (transform state (keyword x)))])


(defn react [state baseinput]
  (let [canon-input (vec (str/split (str/replace (str/lower-case baseinput) #"[?.!]" "") #" +"))]
	(loop [idx 0]
  	(if (>= idx (count (base state)))
      (let [ret "invalid input"]
        (println ret)
        state)
    	(if-let [vars (match ((base state) idx) canon-input)]
      	(apply ((base state) (inc idx)) vars)
      	(recur (+ idx 2)))))))

(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
  (if-let [val (get-in local-state [:map :robot :contents :infected])]
    (println (str "\nYou win! You finished in " (get-in local-state [:adventurer :tick]) " moves."))

    (do (print (str "\nYou are at " (get-in local-state [:map (get-in local-state [:adventurer :location]) :title]) ". "))
    (let [
          _  (println "What do you want to do?")
          command (read-line)]
      (if (= command "quit") (println "bye!")
        (let [new-state (react local-state command)]
        (recur new-state))))))))