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
     (= 1 (+ (Math/abs di) (Math/abs dj))) ; tile is neighboring
     (not (= 0 (cost tile2 game-map))) ; tile is not marked as impassable
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
              (map (fn [x] (assoc x :cost Double/POSITIVE_INFINITY :predecessor nil :visited false)))
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
      (-> game-map
          (assoc-in [:tiles (index-of-coordinates destination game-map) :cost] new-cost)
          (assoc-in [:tiles (index-of-coordinates destination game-map) :predecessor] origin))
      game-map)))

(defn map-with-node-visited
  [coords game-map]
  (->> (neighbors coords game-map)
       (reduce (fn [acc neighbor] (map-with-neighbor-updated coords neighbor acc)) game-map)
       (#(assoc-in % [:tiles (index-of-coordinates coords %) :visited] true))))

(defn cost-coord-pairs-of-map
  "Returns a list of pairs of coordinates and cost to traverse the said coords. If a node
  is already visited it is considered impassable."
  [game-map]
  (->> (:tiles game-map)
       (map-indexed
        (fn [i x]
          {:cost (if (:visited x) Double/POSITIVE_INFINITY (:cost x))
           :coords (coords-of-index i game-map)}))
       (into [])))

(defn arg-min
  "Returns the argument such that f is minimal."
  [f args]
  (reduce (fn [acc arg] (if (< (f arg) (f acc)) arg acc))
          nil
          args))

(defn next-node-to-visit
  [game-map]
  (->> (cost-coord-pairs-of-map game-map)
       (arg-min (fn [x] (if (nil? x) Double/POSITIVE_INFINITY (:cost x))))
       (:coords)))

(defn dijkstra-map
  [source game-map]
  (let [augmented-map (initialized-map source game-map)]
    (loop [current-map augmented-map]
      (let [next-node (next-node-to-visit current-map)]
        (if (nil? next-node)
          current-map
          (->> (next-node-to-visit current-map)
               (#(map-with-node-visited % current-map))
               (recur)))))))

(defn path-to-node
  [destination dijkstraized-map]
  (let [tiles (:tiles dijkstraized-map)]
    (loop [path [destination]]
      (let [predecessor (:predecessor
                         (get tiles
                              (index-of-coordinates (first path) dijkstraized-map)))]
        (if (nil? predecessor)
          path
          (recur (list* predecessor path)))))))
