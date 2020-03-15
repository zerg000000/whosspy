(ns whos-spy.translation)

(defn t [msg]
  (case msg
    :vote "處決"
    :spy "臥底"
    :common "平民"
    :moron "白板"
    :gaming-desc "每回合，所有人輪流發言證明自己不是臥底，然後投票處決一人。若所有臥底被處死，平民勝利。若臥底生存到最後一回合，則臥底勝。"
    :confirm-desc "當所有人都確認過自己的勢力暗號就可以開始"))
