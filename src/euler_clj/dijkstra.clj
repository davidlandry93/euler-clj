(ns euler-clj.dijkstra)

(defn game-map
  "Generate a map of size nxn with tiles of random traversability cost."
  [n]
  {:width n
   :height n
   :tiles (into [] (repeatedly (* n n) (fn [] {:type (rand-int 10)})))})

(defn index-of-coordinates
  [coords game-map]
  (let [i (first coords)
        j (last coords)]
  (+ (* i (:width game-map)) j)))

(defn cost
  "Returns the traversability cost of a tile on a given game-map."
  [tile game-map]
  (let [i (first tile)
        j (last tile)]
    (:type (->
     (:tiles game-map)
     (get (index-of-coordinates tile game-map))))))

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
  [tile game-map]
  (let
      [i (first tile)
       j (last tile)]
    (filter
     (partial neighbors? tile)
     [[(+ i 1) j] [(- i 1) j] [i (+ 1 j)] [i (- 1 j)]])))

(defn update-tile
  [coords new-tile game-map]
  (let [tiles (:tiles game-map)]
    (assoc tiles (index-of-coordinates coords) new-tile)))

(defn dijkstra-predecessors
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
