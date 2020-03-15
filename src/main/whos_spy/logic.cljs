(ns whos-spy.logic)

;; Logic

(def db [["咖啡" "奶茶"]
         ["鬍子" "眉毛"]
         ["小說" "漫畫"]])

(defn draw-words [db]
  (->> db
       (rand-nth)
       (shuffle)
       (zipmap [:spy :common])))

(defn mark-characters [players {:keys [spy common moron]}]
  (let [characters (-> (concat (repeat spy :spy)
                               (repeat common :common)
                               (repeat moron :moron))
                       (shuffle))]
    (mapv (fn [p c]
           (assoc p :character c))
         players characters)))

(defn init-settings [players]
  (let [spy (js/Math.round (* (count players) 0.2))]
    {:spy spy :common (- (count players) spy)}))

(defn init-game [{:keys [words players]}]
  (let [words (if (some nil? (vals words))
                (draw-words db)
                words)
        settings (init-settings players)]
    {:players (mark-characters players settings)
     :words words
     :settings settings}))

(defn count-character [players & character-types]
  (let [character-fn #(and ((set character-types) (:character %))
                           (not (:voted %)))]
   (-> (filter character-fn players)
       (count))))

(defn spy-win? [{:keys [players]}]
  (and (> (count-character players :spy) 0)
       (<= (count-character players :common :moron) 1)))

(defn common-win? [{:keys [players]}]
  (and (<= (count-character players :spy) 0)))

(defn moron-win? [{:keys [players]}]
  (and (or (= (count-character players :spy) 0)
           (= (count-character players :common) 0))
       (> (count-character players :moron) 0)))

(defn game-over? [game]
  (or (spy-win? game)
      (common-win? game)
      (moron-win? game)))
