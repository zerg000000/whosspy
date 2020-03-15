(ns whos-spy.app
  (:require [uix.core.alpha :as uix]
            [uix.dom.alpha :as dom]
            [goog.object :as o]
            ["react-transition-group/CSSTransition" :as css-transition]
            [whos-spy.logic :as logic]
            [whos-spy.translation :as t]
            [clojure.string :as string]))

(defn mark-for [game-state player marker]
  (update game-state
          :players
          (fn [players]
            (map (fn [p]
                   (if (= (:id p) (:id player))
                     (assoc p marker true)
                     p))
                 players))))

;; UI

(defn button [{:keys [on-click disabled]} text]
  [:button.font-bold
   {:on-click on-click
    :disabled disabled
    :style (cond-> {:font-size "18px"
                    :padding "8px"
                    :background-color "transparent"
                    :border "1px solid black"}
                   disabled
                   (assoc :background-color :grey))}
   text])

(defn name-input [{:keys [on-new-name placeholder]}]
  (let [ref (uix/ref)]
    [:input.flex-auto.sticky
     {:on-key-down (fn [e]
                     (when (= 13 (o/get e "keyCode"))
                       (on-new-name (o/get @ref "value"))
                       (o/set @ref "value" nil)))
      :placeholder placeholder
      :style {:top "53px"
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
  [:ul.list-none.flex.flew-row.flex-wrap.w-full.overflow-x-hidden
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
  [:section.sticky.p-1
   {:style {:background-color :white
            :top 0
            :z-index 1}}
   [:div.flex.flex-row.items-center
    (when left
      left)
    (when middle
      middle)
    (when right
      right)]])

(defn custom-word-inputs [{:keys [custom? words on-spy-input on-common-input]}]
  [:> css-transition {:classNames "alert"
                      :in custom?
                      :mountOnEnter true
                      :timeout 100}
   [:section.flex.flex-row.items-center.content-center
    [:div.flex-auto
     [:label {:for "spyword" :style {:width "3rem"}} "間諜: "]
     [:input#spyword.outline-none.secret
      {:style       {:width         "6rem"
                     :padding-left  "8px"
                     :border-bottom "1px solid black"}
       :on-blur     on-spy-input
       :placeholder (when (string/blank?  (:spy words))
                      "空白")}]]
    [:div.flex-auto
     [:label {:for "commonword" :style {:width "3rem"}} "平民: "]
     [:input#commonword.outline-none.secret
      {:style {:width "6rem"
               :border-bottom "1px solid black"}
       :on-blur     on-common-input
       :placeholder (when (string/blank?  (:common words))
                      "空白")}]]]])

(defn switch [{:keys [value on-toggle]}]
  [:label.switch
   [:input {:type :checkbox :default-value value
            :on-click on-toggle}]
   [:span.slider]])

(defn setup-scene [{:keys [on-setup-finish init-state]}]
  (let [players (uix/state (:players init-state []))
        words (uix/state {:common nil :spy nil})
        custom? (uix/state false)
        on-finish (uix/callback #(on-setup-finish {:players @players :words @words}) [players words])
        enough-player? (> (count @players) 3)
        words-okay? (or (not @custom?) (and (:common @words) (:spy @words)))]
    [:div.flex.flex-col
     [sticky-header {:middle  (let [{:keys [spy common]} (logic/init-settings @players)]
                                [:div.flex-auto
                                  (if enough-player?
                                    (str "玩家: " (count @players)  ", 間諜：" spy ", 平民：" common)
                                    (str "還差" (- 4 (count @players))  "人才可以開始遊戲"))])
                     :right [button {:on-click on-finish
                                     :disabled (not (and enough-player?
                                                         words-okay?))} "開始"]}]
     [:section.flex.flex-row.items-center.p-1
      [:div.flex-auto "自定暗號"]
      [switch {:value @custom?
               :on-toggle #(swap! custom? not)}]]
     [custom-word-inputs {:custom? @custom?
                          :words @words
                          :on-spy-input (fn [in]
                                          (swap! words assoc :spy (-> in .-target .-value)))
                          :on-common-input (fn [in]
                                             (swap! words assoc :common (-> in .-target .-value)))}]
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
     [:p (t/t :confirm-desc)]
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
      [button {} (t/t (:character player))]
      [button {:on-click (fn [e]
                           (reset! clicked true)
                           (on-item-click player))}
       (t/t :vote)])]))

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
                                   (str "第" turn "回合，" (-> @dead last :character t/t) "被處決")))]
                     :right  [button {:on-click on-game-over} "結束"]}]
     [:p (t/t :gaming-desc)]
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
