(ns whos-spy.app
  (:require [uix.core.alpha :as uix]
            [uix.dom.alpha :as dom]
            [goog.object :as o]
            [whos-spy.logic :as logic]))

(defn mark-for [game-state player marker]
  (update game-state
          :players
          (fn [players]
            (map (fn [p]
                   (if (= (:id p) (:id player))
                     (assoc p marker true)
                     p))
                 players))))

(defn t [msg]
   (case msg
     :spy "臥底"
     :common "平民"
     :moron "白板"
     :gaming-desc "每回合，所有人輪流發言證明自己不是臥底，然後投票處決一人。若所有臥底被處死，平民勝利。若臥底生存到最後一回合，則臥底勝。"))

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
    [:input.flex-auto
     {:on-key-down (fn [e]
                     (when (= 13 (o/get e "keyCode"))
                       (on-new-name (o/get @ref "value"))
                       (o/set @ref "value" nil)))
      :placeholder placeholder
      :style {:position :sticky
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
  [:ul.list-none.flex.flew-row.flex-wrap
   {:style {:width "100%"}}
   (for [item data]
     ^{:key (key-fn item)}
     [render-item  (assoc item :on-item-click on-item-click)])])

(defn name-label [{:keys [on-item-click] :as item}]
  [:li.flex.items-center.content-center.list-none
   {:style {:border "1px solid black"
            :border-radius "5px"
            :padding "4px 8px"
            :margin "4px"}}
   [:div {:style {:margin-right "8px"}} (:name item)]
   [:div {:on-click #(on-item-click item)
          :style {:font-size "21px"}} " \u2297"]])

(defn sticky-header [{:keys [right middle left]}]
  [:section {:style {:position :sticky
                     :background-color :white
                     :top 0}}
      [:div.flex.flex-row.items-center
       (when left
         left)
       (when middle
         middle)
       (when right
         right)]])

(defn setup-scene [{:keys [on-setup-finish init-state]}]
  (let [players (uix/state (:players init-state []))
        words (uix/state nil)
        custom-words-cb #()
        on-finish (uix/callback #(on-setup-finish @players) [players])
        enough-player? (> (count @players) 3)]
    [:div.flex.flex-col
     [sticky-header {:middle  (let [{:keys [spy common]} (logic/init-settings @players)]
                                [:div.flex-auto
                                  (if enough-player?
                                    (str "玩家: " (count @players)  ", 間諜：" spy ", 平民：" common)
                                    (str "還差" (- 4 (count @players))  "人才可以開始遊戲"))])
                     :right [button {:on-click on-finish
                                     :disabled (not enough-player?)} "開始"]}]
     [:section.flex.flex-row.items-center
      [:div.flex-auto "暗號"]
      [button {:on-click custom-words-cb} "隨機"]]
     [name-input {:placeholder "輸入玩家名稱"
                  :on-new-name (fn on-new-name [n]
                                 (swap! players
                                        (fn append-new-name [names]
                                          (conj names {:id (.getTime (js/Date.))
                                                       :name n}))))}]
     [list-view {:data @players
                 :render-item name-label
                 :on-item-click (fn [item]
                                  (swap! players
                                         (fn [players]
                                            (remove #(= (:id %) (:id item))  players))))
                 :key-fn :id}]]))

(defn player-code-card [{:keys [on-item-click]} player words]
  [:li.flex.flex-col.items-center.content-center
   {:style (cond->
               {:border "1px solid black"
                :width "45%"
                :margin " calc( 2.5% - 1px)"}
             (:confirmed player)
             (assoc :background-color :grey))}
   [:h4
    {:style {:max-width "150px"
             :text-overflow :ellipsis
             :overflow-x :hidden}}
    (:name player)]
   (if (:confirmed player)
     [button {} "已確認"]
     [button {:on-click on-item-click}
      "確認暗號"])])

(defn confirm-scene [{:keys [game on-confirm]}]
  (let [game-state (uix/state game)
        {:keys [players words]} @game-state]
    [:div.flex.flex-col
     [sticky-header {:middle [:div.flex-auto
                              "確認暗號"]
                     :right [button {:on-click on-confirm
                                     :disabled (not (every? :confirmed players))}
                             "開始"]}]
     [:p "當所有人都確認過自己的勢力暗號就可以開始"]
     [list-view {:data players
                 :key-fn :id
                 :render-item (fn [player]
                                [player-code-card
                                 {:on-item-click
                                  #(do
                                     (js/alert (get words (:character player)))
                                     (swap! game-state mark-for player :confirmed))}
                                 player words])}]]))

(defn player-vote-card [{:keys [on-item-click player]}]
  (let [clicked (uix/state false)]
   [:li.flex.flex-col.items-center.content-center
    {:style (cond->
                {:border "1px solid black"
                 :width "45%"
                 :margin " calc( 2.5% - 1px)"}
                (:voted player)
                (assoc :background-color :grey))}
    [:h4
     {:style {:max-width "150px"
              :text-overflow :ellipsis
              :overflow-x :hidden}}
     (:name player)]
    (if (:voted player)
      [button {} (t (:character player))]
      [button {:on-click (fn [e]
                           (reset! clicked true)
                           (on-item-click player))}
       "處決"])]))

(defn gaming-scene [{:keys [init-state on-game-over]}]
  (let [gaming-state (uix/state init-state)
        dead (uix/state [])]
    [:div.flex.flex-col
     [sticky-header {:middle [:div.flex-auto
                              (cond
                                (= (count @dead) 0)
                                "投票"
                                (logic/moron-win? @gaming-state)
                                "白板勝利!"
                                (logic/spy-win? @gaming-state)
                                "臥底勝利!"
                                (logic/common-win? @gaming-state)
                                "平民勝利!"
                                :else
                                (let [turn (count @dead)]
                                   (str "第" turn "回合，" (-> @dead last :character t) "被處決")))]
                     :right  [button {:on-click on-game-over} "結束"]}]
     [:p (t :gaming-desc)]
     [list-view {:data (:players @gaming-state)
                 :render-item
                 (fn [item]
                   [player-vote-card {:on-item-click
                                      #(do (swap! gaming-state mark-for item :voted)
                                           (swap! dead conj item))
                                      :player item}])
                 :key-fn :id}]]))

(defn app []
  (let [scene (uix/state :setup)
        game (uix/state {})
        finish-setup-cb (fn [g]
                          (reset! game (logic/init-game g))
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
