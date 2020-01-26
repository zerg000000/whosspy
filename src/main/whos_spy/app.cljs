(ns whos-spy.app
  (:require [uix.core.alpha :as uix]
            [uix.dom.alpha :as dom]
            [goog.object :as o]))

;; Logic

(def db [{:spy "咖啡"
          :common "奶茶"
          :moron nil}])

(defn mark-characters [players {:keys [spy common moron]}]
  (let [characters (-> (concat (repeat spy :spy)
                               (repeat common :common)
                               (repeat moron :moron))
                       (shuffle))]
    (mapv (fn [p c]
           (assoc p :character c))
         players characters)))

(defn init-game [players settings]
  (let [words (rand-nth db)]
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

;; UI

(defn button [{:keys [on-click]} text]
  [:button.btn {:on-click on-click}
   text])

(defn name-input [{:keys [on-new-name]}]
  (let [ref (uix/ref)]
    [:input {:on-key-down (fn [e]
                            (when (= 13 (o/get e "keyCode"))
                              (on-new-name (o/get @ref "value"))
                              (o/set @ref "value" nil)))
             :ref ref}]))

(defn list-view [{:keys [data render-item key-fn]}]
  [:ul
   (for [item data]
     ^{:key (key-fn item)}
     [render-item item])])

(defn item-renderer [item]
  [:li (:name item)])

(defn setup-scene [{:keys [on-setup-finish]}]
  (let [state* (uix/state [])]
    [:<>
     [:div
      [:span (str "Players: " (count @state*))]
      [button {:on-click #(on-setup-finish @state*)} "Start"]]
     [name-input {:on-new-name (fn on-new-name [n]
                                 (swap! state*
                                        (fn append-new-name [names]
                                          (conj names {:id (count names)
                                                       :name n}))))}]
     [list-view {:data @state*
                 :render-item item-renderer
                 :key-fn :id}]]))

(defn confirm-scene [{:keys [game on-confirm]}]
  (let [idx (uix/state 0)
        {:keys [words players]} game
        cur-player (get-in players [@idx])]
    [:<>
     [:h3 "Please pass your phone according to the name"]
     [:h2 (:name cur-player)]
     [:h3 (get words (:characters cur-player))]
     (if cur-player
       [:button {:on-click #(swap! idx inc)} "Next Player"]
       [:button {:on-click on-confirm} "Start"])]))

(defn vote-for-spy [game-state player]
  (update game-state
          :players
          (fn [players]
            (map (fn [p]
                   (if (= (:id p) (:id player))
                     (assoc p :voted true)
                     p))
                 players))))

(defn gaming-scene [{:keys [init-state on-game-over]}]
  (let [gaming-state (uix/state init-state)]
   [:<>
    [list-view {:data (:players @gaming-state)
                :render-item (fn [item]
                               (if (:voted item)
                                 [:li (str (:name item) "(" (name (:character item)) ")")]
                                 [:li {:on-click #(swap! gaming-state vote-for-spy item)}
                                  (:name item)]))
                :key-fn :id}]
    (when (game-over? @gaming-state)
      (cond
        (moron-win? @gaming-state)
        "Moron Win!"
        (spy-win? @gaming-state)
        "Spy Win!"
        (common-win? @gaming-state)
        "Common Win!"))
    [:button {:on-click #(on-game-over)} "結束"]]))

(defn app []
  (let [scene (uix/state :setup)
        game (uix/state {})
        finish-setup-cb (fn [g]
                          (reset! game (init-game g {:spy 1 :common 3}))
                          (reset! scene :confirm))
        confirm-character-cb (fn [] (reset! scene :gaming))]
    (case @scene
      :confirm
      [confirm-scene {:game @game
                      :on-confirm confirm-character-cb}]
      :gaming
      [gaming-scene {:init-state @game
                     :on-game-over (fn [& _] (reset! scene nil))}]
      :gameover
      [:div "Game Over"]
      [setup-scene {:on-setup-finish finish-setup-cb}])))

(defn ^:dev/after-load init []
  (dom/render [app] js/root))
