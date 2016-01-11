(ns euler-clj.dijkstra)

(defn game-map
  "Generate a map of size nxn with tiles of random traversability cost."
  [n]
  {:width n
   :height n
   :tiles (into [] (repeatedly (* n n) (fn [] {:type (rand-int 10)})))})

(defn coords-of-index
  [i {width :width height :height}]
  [(quot i width) (mod i width)])

(defn index-of-coordinates
  [coords game-map]
  (let [i (first coords)
        j (last coords)]
  (+ (* i (:width game-map)) j)))

(defn cost
  "Returns the traversability cost of a tile on a given game-map."
  [tile game-map]
  (->
   (:tiles game-map)
   (get (index-of-coordinates tile game-map))
   (:type)))

(defn neighbors?
  "Returns true if you can go to tile2 from tile1"
  [tile1 tile2 game-map]
  (let [i2 (first tile2)
        j2 (last tile2)
        di (- (first tile2) (first tile1))
        dj (- (last tile2) (last tile1))]
    (and
     (= 1 (+ (Math/abs di) (Math/abs dj)))
     (not (= 0 (cost tile2 game-map)))
     (<= 0 i2)
     (<= 0 j2)
     (> (:height game-map) i2)
     (> (:width game-map) j2))))

(defn neighbors
  "Returns a list of the neighbors of a tile"
  [[i j] game-map]
  (filter
   (fn [x] (neighbors? [i j] x game-map))
   [[(+ i 1) j] [(- i 1) j] [i (+ 1 j)] [i (- j 1)]]))

(defn update-tile
  [coords new-tile game-map]
  (let [tiles (:tiles game-map)]
    (assoc tiles (index-of-coordinates coords) new-tile)))

(defn map-with-infinite-distances
  [game-map]
  (assoc game-map
         :tiles
         (->> (:tiles game-map)
              (map (fn [x] (assoc x :cost Double/POSITIVE_INFINITY :predecessor nil)))
              (into []))))

(defn initialized-map
  [source game-map]
  (-> game-map
      (map-with-infinite-distances)
      (assoc-in [:tiles (index-of-coordinates source game-map) :cost] 0)))

(defn map-with-neighbor-updated
  [origin destination game-map]
  (let [origin-tile (get (:tiles game-map) (index-of-coordinates origin game-map))
        destination-tile (get (:tiles game-map) (index-of-coordinates destination game-map))
        new-cost (+ (:cost origin-tile) (cost destination game-map))]
    (if (< new-cost (:cost destination-tile))
      (assoc-in game-map
                [:tiles (index-of-coordinates destination game-map)]
                {:cost new-cost :predecessor origin})
      game-map)))

(defn map-with-node-visited
  [coords game-map]
  (reduce (fn [acc neighbor] (map-with-neighbor-updated coords neighbor acc))
          game-map
          (neighbors coords game-map)))

(defn cost-coord-pairs-of-map
  [game-map]
  (->> (:tiles game-map)
       (map-indexed (fn [i x] {:cost (:cost x) :coords (coords-of-index i game-map)}))
       (into [])))

(defn dijkstra-map
  [source game-map]
  (let [augmented-map (initialized-map source game-map)]))

(defn dijksta-predecessors
  [source game-map]
  (let [augmented-tiles-coll
        (assoc
         (->>
          (:tiles game-map)
          (map (fn [x] (assoc x :cost Double/POSITIVE_INFINITY :predecessor nil)))
          (into []))
         (index-of-coordinates source game-map)
         (assoc (nth (:tiles game-map) (index-of-coordinates source game-map)) :cost 0 :predecessor nil))
         ]
    augmented-tiles-coll))
