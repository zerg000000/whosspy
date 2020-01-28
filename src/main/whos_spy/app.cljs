(ns whos-spy.app
  (:require [uix.core.alpha :as uix]
            [uix.dom.alpha :as dom]
            [goog.object :as o]))

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

(defn init-game [players]
  (let [words (draw-words db)
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

;; UI

(defn button [{:keys [on-click disabled]} text]
  [:button {:on-click on-click
            :disabled disabled
            :style {:font-size "18px"
                    :padding "8px"
                    :margin "8px"
                    :background-color "transparent"
                    :border "1px solid black"}}
   text])

(defn name-input [{:keys [on-new-name placeholder]}]
  (let [ref (uix/ref)]
    [:input {:on-key-down (fn [e]
                            (when (= 13 (o/get e "keyCode"))
                              (on-new-name (o/get @ref "value"))
                              (o/set @ref "value" nil)))
             :placeholder placeholder
             :style {:flex "1 0 auto"
                     :position :sticky
                     :top "60px"
                     :font-size "18px"
                     :margin-top "8px"
                     :padding "8px"
                     :border-top "1px solid black"
                     :border-bottom "1px solid black"
                     :height "32px"}
             :ref ref}]))

(defn list-view
  "Flex based list view"
  [{:keys [data render-item key-fn on-item-click]}]
  [:ul {:style {:list-style "none"
                :display :flex
                :flex-direction :row
                :flex-wrap :wrap
                :width "100%"
                :padding-left "0"}}
   (for [item data]
     ^{:key (key-fn item)}
     [render-item  (assoc item :on-item-click on-item-click)])])

(defn name-label [{:keys [on-item-click] :as item}]
  [:li
   {:style {:list-style "none"
            :list-style-type "none"
            :border "1px solid black"
            :border-radius "5px"
            :padding "4px 8px"
            :margin "4px"
            :display :flex
            :align-items :center
            :justify-content :center}}
   [:div {:style {:margin-right "8px"}} (:name item)]
   [:div {:on-click #(on-item-click item)
          :style {:font-size "21px"}} " \u2297"]])

(defn sticky-header [{:keys [right middle left]}]
  [:section {:style {:position :sticky
                        :background-color :white
                        :top 0}}
      [:div {:style {:display :flex
                     :flex-direction :row
                     :align-items :center}}
       (when left
         left)
       (when middle
         middle)
       (when right
         right)]])

(defn setup-scene [{:keys [on-setup-finish init-state]}]
  (let [state* (uix/state (:players init-state []))
        words (uix/state nil)
        custom-words-cb #()
        on-finish (uix/callback #(on-setup-finish @state*) [state*])
        enough-player? (> (count @state*) 3)]
    [:div {:style {:display :flex
                   :flex-direction :column}}
     [sticky-header {:middle  (let [{:keys [spy common]} (init-settings @state*)]
                                [:div {:style {:flex "1 0 auto"}}
                                  (if enough-player?
                                    (str "玩家: " (count @state*)  ", 間諜：" spy ", 平民：" common)
                                    "最少要有四人才可以開始遊戲")])
                     :right [button {:on-click on-finish
                                     :disabled (not enough-player?)} "開始"]}]
     [:section {:style {:display :flex
                        :flex-direction :row
                        :align-items :center}}
      [:div {:style {:flex "1 0 auto"}} "暗號"]
      [button {:on-click custom-words-cb} "隨機"]]
     [name-input {:placeholder "輸入玩家名稱"
                  :on-new-name (fn on-new-name [n]
                                 (swap! state*
                                        (fn append-new-name [names]
                                          (conj names {:id (js/Date.)
                                                       :name n}))))}]
     [list-view {:data @state*
                 :render-item name-label
                 :on-item-click (fn [item]
                                  (swap! state*
                                         (fn [players]
                                            (remove #(= (:id %) (:id item))  players))))
                 :key-fn :id}]]))

(defn player-code-card [{:keys [on-item-click] :as player} words]
  (let [clicked (uix/state false)]
   [:li {:style (cond->
                    {:border "1px solid black"
                     :width "45%"
                     :margin " calc( 2.5% - 1px)"
                     :display :flex
                     :flex-direction :column
                     :align-items :center
                     :justify-content :center}
                    @clicked
                    (assoc :background-color :grey))}
    [:h4
     {:style {:max-width "150px"
              :text-overflow :ellipsis
              :overflow-x :hidden}}
     (:name player)]
    (if @clicked
      [button {} "已確認"]
      [button {:on-click (fn [e]
                           (js/alert (get words (:character player)))
                           (reset! clicked true))}
       "確認暗號"])]))

(defn confirm-scene [{:keys [game on-confirm]}]
  (let [{:keys [words players]} game]
    [:div
     {:style {:display :flex
              :flex-direction :column}}
     [sticky-header {:middle [:div {:style {:flex "1 0 auto"}}
                              "確認暗號"]
                     :right [button {:on-click on-confirm} "開始"]}]
     [:p "當所有人都確認過自己的勢力暗號就可以開始"]
     [list-view {:data players
                 :key-fn :id
                 :render-item (fn [item]
                                [player-code-card item words])}]]))

(defn vote-for-spy [game-state player]
  (update game-state
          :players
          (fn [players]
            (map (fn [p]
                   (if (= (:id p) (:id player))
                     (assoc p :voted true)
                     p))
                 players))))

(defn player-vote-card [{:keys [on-item-click player]}]
  (let [clicked (uix/state false)]
   [:li {:style (cond->
                    {:border "1px solid black"
                     :width "45%"
                     :margin " calc( 2.5% - 1px)"
                     :display :flex
                     :flex-direction :column
                     :align-items :center
                     :justify-content :center}
                    (:voted player)
                    (assoc :background-color :grey))}
    [:h4
     {:style {:max-width "150px"
              :text-overflow :ellipsis
              :overflow-x :hidden}}
     (:name player)]
    (if (:voted player)
      [button {} (case (:character player)
                   :spy "臥底"
                   :common "平民"
                   :moron "白板")]
      [button {:on-click (fn [e]
                           (reset! clicked true)
                           (on-item-click player))}
       "處決"])]))

(defn gaming-scene [{:keys [init-state on-game-over]}]
  (let [gaming-state (uix/state init-state)]
    [:div {:style {:display :flex
                   :flex-direction :column}}
     [sticky-header {:middle [:div {:style {:flex "1 0 auto"}} "投票"]
                     :right  [button {:on-click on-game-over} "結束"]}]
     [:p "每回合，所有人輪流發言證明自己不是臥底，然後投票處決一人。若所有臥底被處死，平民勝利。若臥底生存到最後一回合，則臥底勝。"]
     (when (game-over? @gaming-state)
       [:h4
        (cond
          (moron-win? @gaming-state)
          "白板勝利!"
          (spy-win? @gaming-state)
          "臥底勝利!"
          (common-win? @gaming-state)
          "平民勝利!")])
     [list-view {:data (:players @gaming-state)
                 :render-item
                 (fn [item]
                   [player-vote-card {:on-item-click
                                      #(swap! gaming-state vote-for-spy item)
                                      :player item}])
                 :key-fn :id}]]))

(defn app []
  (let [scene (uix/state :setup)
        game (uix/state {})
        finish-setup-cb (fn [g]
                          (reset! game (init-game g))
                          (reset! scene :confirm))
        confirm-character-cb (fn [] (reset! scene :gaming))
        return-setup-cb (uix/callback (fn [& _] (reset! scene nil)) [scene])]
    (case @scene
      :confirm
      [confirm-scene {:game @game
                      :on-confirm confirm-character-cb}]
      :gaming
      [gaming-scene {:init-state @game
                     :on-game-over return-setup-cb}]
      [setup-scene {:init-state @game
                    :on-setup-finish finish-setup-cb}])))

(defn ^:dev/after-load init []
  (dom/render [app] js/root))
