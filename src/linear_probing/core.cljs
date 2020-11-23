(ns linear-probing.core
  (:require [quil.core :as q :include-macros true]
            [clojure.core.matrix :as mat]
            [quil.middleware :as m]))

(def views (cycle [:text :dots]))

(defn mapvals [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn get-d []
  (* 0.8 (min (q/width) (q/height))))

(defn i->pos [d N i]
  (let [theta (- (* q/TWO-PI (/ i N)) (/ q/PI 2))
        r (/ d 2)]
    (mat/scale r [(q/cos theta) (q/sin theta)])))

(defn seek [speed d N node]
  (let [{:keys [pos]} node
        target (i->pos d N (:target node))
        dir (mat/sub target pos)
        vel (mat/scale (/ speed 1000) dir)]
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
  (if-not ((set A) k)
    (update state :nodes dissoc k)
    (loop [i (mod k N)]
      (cond (nil? (A i)) state
            (not= (A i) k) (recur (mod (inc i) N))
            :else (-> (assoc-in state [:A i] nil)
                      (update :nodes dissoc k)
                      (fill-hole i))))))

(defn rehash [{:keys [N] :as state}]
  (-> (assoc state :A (vec (repeat N nil)))
      (update :nodes (partial mapvals #(assoc % :target (mod (:k %) N))))))

(defn make-node [k N]
  {:pos [0 0] :k k :target (mod k N)})

(defn insert-node [{:keys [N] :as state} k]
  (update state :nodes assoc k (make-node k N)))

(defn insert-random-node [state]
  (insert-node state (rand-int 1000)))

(defn bulk-insert
  ([state] (insert-random-node state))
  ([state k]
   (let [rands (repeatedly k #(rand-int 1000))]
     (reduce insert-node state rands))))

(defn delete-random-node [{:keys [A] :as state}]
  (let [ks (filterv identity A)]
    (if (empty? ks)
      state
      (delete-node state (rand-nth ks)))))

(defn clear [state & _discard-args]
  (-> (assoc state :nodes {})
      (update :A (partial mapv empty))))

(defn pos->angle [pos]
  (let [theta (apply q/atan2 (reverse pos))]
    (if (< (- (/ q/PI 2)) theta (/ q/PI 2))
      theta
      (+ theta q/PI))))

(defn resize [state k]
  (rehash (assoc state :N k)))

(defn set-text-size []
  (let [size (min (q/width) (q/height))
        font-size (/ size 25.0)
        zoom (/ (- size 1.0) size)]
    (q/text-size (* zoom 20 (q/display-density)))))

(defn setup []
  (q/text-font "Roboto Mono")
  (set-text-size)
  (q/fill 255)
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/text-align :center :center)
  (let [N 10]
    {:d (get-d) :N N
     :nodes {}
     :A (vec (repeat N nil))
     :view :text
     :speed 50}))

(defn update-state [{:keys [d N A speed nodes] :as state}]
  (-> (assoc state :d (get-d))
      (update :nodes (partial mapvals (partial seek speed d N)))
      (update :nodes (partial mapvals (partial set-target d N A)))
      (update :A insert-nearby d N nodes)))

(defn draw-state [{:keys [d N nodes view]}]
  (q/background 0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (dotimes [i N]
    (let [[x y] (i->pos (* 1.15 d) N i)
          [x1 y1] (i->pos (* 1.15 d) N i)
          [x2 y2] (i->pos (* 1.05 d) N i)]
      (q/stroke (* 256 (/ i N)) 255 255 100)
      (q/fill (* 256 (/ i N)) 255 255 100)
      (case view
        :text (q/text (str i) x y)
        :dots (q/line x1 y1 x2 y2))))

  (q/no-stroke)
  (doseq [[k {:keys [pos]}] nodes]
    (q/with-fill [(* 256 (/ (mod k N) N)) 255 255]
      (q/with-translation pos
        (q/with-rotation [(pos->angle pos)]
          (case view
            :text (q/text (str k) 0 0)
            :dots (q/ellipse 0 0 5 5)))))))

(defn do-op [op op-default]
  (let [field (.getElementById js/document "k")
        k (.-value field)]
    (set! (.-value field) "")
    (q/with-sketch (q/get-sketch-by-id "linear-probing")
      (if (empty? k)
        (swap! (q/state-atom) op-default)
        (swap! (q/state-atom) op (int k))))))

(defn set-speed []
  (let [field (.getElementById js/document "speed")
        speed (.-value field)]
    (q/with-sketch (q/get-sketch-by-id "linear-probing")
      (swap! (q/state-atom) assoc :speed speed))))

(defn next-view [{:keys [view] :as state}]
  (let [new-view (second (drop-while (partial not= view) views))]
    (assoc state :view new-view)))

(defn key-handler [state event]
  (case (q/key-as-keyword)
    :r (do-op resize rehash)
    :i (do-op insert-node insert-random-node)
    :b (do-op bulk-insert bulk-insert)
    :d (do-op delete-node delete-random-node)
    :h (rehash state)
    :v (next-view state)
    :c (clear state)
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
    (let [[w h] (get-size)]
      (q/resize-sketch w h)
      (set-text-size))))

(defn add-listerners []
  (let [insert-btn (.getElementById js/document "insert")
        bulk-insert-btn (.getElementById js/document "bulk-insert")
        delete-btn (.getElementById js/document "delete")
        resize-btn (.getElementById js/document "resize")
        view-btn (.getElementById js/document "view")
        clear-btn (.getElementById js/document "clear")
        speed-slider (.getElementById js/document "speed")]
    (.addEventListener js/window "resize" windowresize-handler true)
    (.addEventListener insert-btn "click" #(do-op insert-node insert-random-node))
    (.addEventListener bulk-insert-btn "click" #(do-op bulk-insert bulk-insert))
    (.addEventListener delete-btn "click" #(do-op delete-node delete-random-node))
    (.addEventListener resize-btn "click" #(do-op resize rehash))
    (.addEventListener view-btn "click" #(do-op next-view next-view))
    (.addEventListener clear-btn "click" #(do-op clear clear))
    (.addEventListener speed-slider "input" #(set-speed))))

(defn ^:export run-sketch []
  (add-listerners)
  (q/defsketch linear-probing
    :host "linear-probing"
    :size (get-size)
    :setup setup
    :update update-state
    :key-pressed key-handler
    :draw draw-state
    :middleware [m/fun-mode]))

;; uncomment this line to reset the sketch:
#_(run-sketch)
