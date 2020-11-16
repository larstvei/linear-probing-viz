(ns linear-probing.core
  (:require [quil.core :as q :include-macros true]
            [clojure.core.matrix :as mat]
            [quil.middleware :as m]))

(defn mapvals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn get-d []
  (* 0.8 (min (q/width) (q/height))))

(defn i->pos [d N i]
  (let [theta (- (* q/TWO-PI (/ i N)) (/ q/PI 2))
        r (/ d 2)]
    (mat/scale r [(q/cos theta) (q/sin theta)])))

(defn seek [d N node]
  (let [{:keys [pos]} node
        target (i->pos d N (:target node))
        dir (mat/sub target pos)
        vel (mat/scale 0.05 dir)]
    (update node :pos (partial mapv +) vel)))

(defn set-target [d N A node]
  (let [{:keys [pos k target]} node
        pos2 (i->pos d N target)
        dist (q/map-range (mat/distance pos pos2) d 0 1 0)]
    (if (and (< dist 0.25)
             (A target)
             (not= (A target) k))
      (update node :target #(mod (inc %) N))
      node)))

(defn insert-nearby [A d N nodes]
  (-> (fn [A [k {:keys [pos target] :as node}]]
        (let [pos2 (i->pos d N target)
              dist (q/map-range (mat/distance pos pos2) d 0 1 0)]
          (if (and (< dist 0.3) (nil? (A target)))
            (assoc A target k)
            A)))
      (reduce A nodes)))

(defn fill-hole [{:keys [N A nodes] :as state} i]
  (loop [s 1 seen #{}]
    (let [j (mod (+ i s) N)
          k (A j)
          seen (conj seen j)]
      (cond (nil? k) state
            (seen (mod k N)) (recur (inc s) seen)
            :else (-> (assoc-in state [:nodes k :target] i)
                      (assoc-in [:A i] k)
                      (assoc-in [:A j] nil)
                      (fill-hole j))))))

(defn delete-node [{:keys [N A] :as state} k]
  (loop [i (mod k N)]
    (cond (nil? (A i)) state
          (not= (A i) k) (recur (mod (inc i) N))
          :else (-> (assoc-in state [:A i] nil)
                    (update :nodes dissoc k)
                    (fill-hole i)))))

(defn rehash [{:keys [N] :as state}]
  (-> (assoc state :A (vec (repeat N nil)))
      (update :nodes (partial mapvals #(assoc % :target (mod (:k %) N))))))

(defn make-node [k N]
  {:pos [0 0] :k k :target (mod k N)})

(defn insert-node [{:keys [N] :as state} k]
  (update state :nodes assoc k (make-node k N)))

(defn insert-random-node [state]
  (insert-node state (rand-int 1000)))

(defn delete-random-node [{:keys [A] :as state}]
  (let [ks (filterv identity A)]
    (if (empty? ks)
      state
      (delete-node state (rand-nth ks)))))

(defn pos->angle [pos]
  (let [theta (apply q/atan2 (reverse pos))]
    (if (< (- (/ q/PI 2)) theta (/ q/PI 2))
      theta
      (+ theta q/PI))))

(defn resize [state k]
  (rehash (assoc state :N k)))

(defn setup []
  (q/text-font "Roboto Mono")
  (q/text-size 20)
  (q/fill 255)
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/text-align :center :center)
  (let [N 10]
    {:d (get-d) :N N
     :nodes {}
     :A (vec (repeat N nil))}))

(defn update-state [{:keys [d N A nodes] :as state}]
  (-> (assoc state :d (get-d))
      (update :nodes (partial mapvals (partial seek d N)))
      (update :nodes (partial mapvals (partial set-target d N A)))
      (update :A insert-nearby d N nodes)))

(defn draw-state [{:keys [d N nodes]}]
  (q/background 0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (dotimes [i N]
    (q/with-fill [(* 256 (/ i N)) 255 255 100]
      (let [[x y] (i->pos (* 1.15 d) N i)]
        (q/text (str i) x y))))

  (doseq [[k {:keys [pos]}] nodes]
    (q/with-fill [(* 256 (/ (mod k N) N)) 255 255]
      (q/with-translation pos
        (q/with-rotation [(pos->angle pos)]
          (q/text (str k) 0 0))))))

(defn do-op [op op-default]
  (let [k (.-value (.getElementById js/document "k"))]
    (q/with-sketch (q/get-sketch-by-id "linear-probing")
      (if (empty? k)
        (swap! (q/state-atom) op-default)
        (swap! (q/state-atom) op (int k))))))

(defn key-handler [state event]
  (case (q/key-as-keyword)
    :r (do-op resize rehash)
    :i (do-op insert-node insert-random-node)
    :d (do-op delete-node delete-random-node)
    :h (rehash state)
    :up (-> (update state :N inc) rehash)
    :down (-> (update state :N dec) (update :N max 1) rehash)
    state))

(defn get-size []
  (let [div (.getElementById js/document "linear-probing")
        w (.-offsetWidth div)
        h (.-offsetHeight div)]
    [w h]))

(defn windowresize-handler []
  (q/with-sketch (q/get-sketch-by-id "linear-probing")
    (apply q/resize-sketch (get-size))))

(defn ^:export run-sketch []
  (let [[w h] (get-size)
        insert-btn (.getElementById js/document "insert")
        delete-btn (.getElementById js/document "delete")
        resize-btn (.getElementById js/document "resize")]
    (.addEventListener js/window "resize" windowresize-handler)
    (.addEventListener insert-btn "click" #(do-op insert-node insert-random-node))
    (.addEventListener delete-btn "click" #(do-op delete-node delete-random-node))
    (.addEventListener resize-btn "click" #(do-op resize rehash))
    (q/defsketch linear-probing
      :host "linear-probing"
      :size [w h]
      :setup setup
      :update update-state
      :key-pressed key-handler
      :draw draw-state
      :middleware [m/fun-mode])))

;; uncomment this line to reset the sketch:
#_(run-sketch)
