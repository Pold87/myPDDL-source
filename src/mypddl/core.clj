(ns mypddl.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]
            [me.raynes.fs :as fs]
            [pddl-clojure-interface.core :as pci]
            [quil.core :as quil]
            [fipp.edn :refer (pprint) :rename {pprint fipp}]))

;;; Math helper functions
(defn sqr
  "Square of a number"
  [x]
  (* x x))

(defn round-places 
  "Round number to decimal places"
  [number decimals]
  (let [factor (math/expt 10 decimals)]
    (double (/ (math/round (* factor number)) factor))))

(defn squared-euclidean-distance
  "Compute squared Euclidean distance between two sequences."
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Compute the Euclidean distance between two coordinates."
  [a b]
  (math/sqrt (squared-euclidean-distance a b)))

;;; End math helper functions.


(defn get-specified-predicate-in-pddl-file
  "Extracts all matching predicates in the init part in a PDDL problem"
  [pddl-file predicate-name]
  (filter #(and (seq? %)
                (= predicate-name (first %)))
          (pci/get-PDDL-init pddl-file)))


(defn calc-distance
  "Calculate distance between PDDL objects"
  [locations]
  (for [[ _ loc1 & xyz-1] locations
        [ _ loc2 & xyz-2] locations]
    ;; Euclidean distance rounded to 4 decimal places.
    (list 'distance loc1 loc2 (round-places (euclidean-distance xyz-1 xyz-2) 4))))

(defn create-PDDL-file
  "Create a PDDL file from a template and add domain name"
  [file-in file-out regex new-domain-name]
  (spit file-out
        (clojure.string/replace
         (slurp file-in)
         regex
         new-domain-name)))

(defn hash-map->dot
  "Converts a hash-map to
  dot language for creating
  UML diagrams"
  [h-map]  
  (map (fn [map-entry]
         (str (key map-entry)
              "[label = \"{" (key map-entry) "|"
              (clojure.string/join #"\\l"  (val map-entry))
              "}\"]\n"))
       h-map))


(defn pddl-pred->hash-map
  "Take a PDDL predicate, e.g.
  '(at ?x - location ?y - object) and returns a
  hash-map, that assigns the involved types
  to this predicate, e.g.
  {location [(at ?x - location ?y - object)],
   object [(at ?x - location ?y - object)]}"
  [pddl-pred]
  (reduce (fn [h-map pddl-type]
            (assoc h-map
              pddl-type
              (list pddl-pred)))
          {}
          (pci/get-types-in-predicate pddl-pred)))


(defn all-pddl-preds->hash-map
  "Takes a list of PDDL predicates and
  returns a hash-map of types and the
  assigned predicate"
  [pddl-preds]
  ;; remove :predicates keyword
  (let [pddl-preds (if (= :predicates (first pddl-preds))
                     (rest pddl-preds)
                     pddl-preds)]
    (apply merge-with concat
           (map pddl-pred->hash-map pddl-preds))))

(defn split-up
  "Split a PDDL type list (:types obj1.1 obj1.2 - objT1 obj2 - objT2 ...)
  into strings of subtypes and associated types,
  [[subytype1 subtype 2 ... - type][subtype1 subtype2 ...][type]"
  [coll]
  ;; Remove ':types' if it is present.
  (let [coll (if (= :types (first coll))
               (rest coll)
               coll)]
    ;; Capturing group 1 is type1.1 type1.2.
    ;; Capturing group 1 is type1.
    (re-seq #"((?:(?:\b[a-zA-Z](?:\w|-|_)+)\s+)+)-\s+(\b[a-zA-Z](?:\w|-|_)+)"
            (clojure.string/join " " coll))))

(defn types->hash-map-helper
  "Convert splitted type list (['<expr>' '<subtype1.1> <subtype1.2> ...' '<type1>']
  to a hash-map {'<type1>': ['<subtype1.1>' '<subtype1.2>' ...], '<type2>': ...}"
  [coll]
  (reduce (fn [h-map [_ objs obj-type]]
            (let [key-obj-type (keyword obj-type)
                  existing-vals (key-obj-type h-map)]
              (assoc h-map
                key-obj-type
                (concat existing-vals
                        (clojure.string/split objs #"\s")))))
          {}
          coll))

(defn types->hash-map
  "Splits types and converts them into a hash-map"
  [pddl-types]
  (types->hash-map-helper (split-up pddl-types)))

(defn types-map-entry->dot-language
  "Convert a hash-map entry
to the dot language"
  [entry]
  (str "\"" (name (key entry)) "\""
       " -> "
       "{"  (clojure.string/join " "
                                 (map #(str "\"" % "\"")
                                      (val entry)))
       "}"))


(defn types-hash-map->dot-language
  "Convert a PDDL types hash-map
to the dot language notation"
  [pddl-types-map]
  (clojure.string/join "\n"
                       (map types-map-entry->dot-language
                            pddl-types-map)))

(defn PDDL->dot-with-style
  "Add dot template"
  [preds types dot-template]
  (let [template (slurp dot-template)
        predicates (clojure.string/join
                    (hash-map->dot
                     (all-pddl-preds->hash-map preds)))
        types (types-hash-map->dot-language
                (types->hash-map types))]
    (clojure.string/replace  template
                             #"GENERATED"
                             (str predicates types))))


(defn PDDL->dot
  "Takes a complete PDDL file
and generates a UML type diagram"
  [pddl-file dot-template]
  (PDDL->dot-with-style (pci/get-PDDL-predicates pddl-file)
                        (pci/get-PDDL-types pddl-file)
                        dot-template))

;;; Copied from https://www.refheap.com/9034
(defn exit-on-close [sketch]
  "Guarantees that Clojure script will be
exited after the JFrame is closed"
  (let [frame (-> sketch .getParent .getParent .getParent .getParent)]
    (.setDefaultCloseOperation frame javax.swing.JFrame/EXIT_ON_CLOSE)))
  

(defn -main
  "Runs the input/output scripts"
  [& args]
  (let [tool (first args)]
    (cond

     ;; myPDDL-new: Create a new PDDL project
     (= "new" tool)
     
     (let [project-name (nth args 1)
           domain-template (nth args 2)
           problem-template (nth args 3)
           readme-template (nth args 4)
           planner-template (nth args 5)
           project-root (nth args 6)
           project-dir (str project-root project-name "/")
           plan-script (str project-dir "plan")]
       
       (fs/mkdirs project-dir)
       (fs/mkdir (str project-dir "domains"))
       (fs/mkdir (str project-dir "solutions"))
       (fs/mkdir (str project-dir "problems"))

       ;; Only perform the following actions
       ;; if the template exists (when ...)
       
       ;; Create planning script
       (when (fs/exists? planner-template)
             (fs/copy planner-template plan-script)
             (fs/chmod "+x" plan-script))
       
       
       (when (fs/exists? readme-template)
         (create-PDDL-file readme-template
                           (str project-dir "README.md")
                           #""
                           ""))

       (when (fs/exists? domain-template)
         (create-PDDL-file domain-template
                           (str project-dir "domain.pddl")
                           #"domain-name"
                           project-name))

       (when (fs/exists? problem-template)
         (create-PDDL-file problem-template
                           (str project-dir "problems/" "p01.pddl")
                           #"domain-name"
                           project-name)))

       

       ;; myPDDL-distance: Calculate distances between PDDL objects
     (= "distance" tool)
     (let [location-file (nth args 1)
           location-predicate (symbol (nth args 2))
           content (pci/add-part-to-PDDL location-file
                                     'init
                                     (calc-distance
                                      (get-specified-predicate-in-pddl-file location-file
                                                                        location-predicate)))
           new-filename (clojure.string/replace-first location-file
                                                      #"(.+)\.pddl"
                                                      "$1-locations.pddl")]
       (pci/write->file new-filename (fipp content {:width 85})))

     ;; Write dot graph to file.
     (= "diagram" tool)
     (let [input-domain (nth args 1)
           revision-control (symbol (nth args 2))
           image-displayer (nth args 3)
           dot-template (nth args 4)
           new-dot-filename (pci/find-new-file-name "dot/dot-diagram" ".dot")
           new-png-filename (pci/find-new-file-name "diagrams/png-diagram" ".png")
           input-domain-filename (fs/name input-domain)
           domain-version (pci/find-new-file-name
                           (str "domains/" input-domain-filename)
                           (fs/extension input-domain))]
       (print new-dot-filename)
       
       ;; Save input domain version in folder domains.
       (if revision-control
         (fs/copy+ input-domain domain-version))     

       ;; Create folders for dot files and png diagrams
       (fs/mkdir "dot")
       (fs/mkdir "diagrams")
       
       ;; Create dot language file in dot folder.
       (doall
        (pci/write->file new-dot-filename
                     (print (PDDL->dot input-domain dot-template))))

       ;; Create a png file from dot
       (fs/exec "dot" "-Tpng" "-o" new-png-filename new-dot-filename)

       ;; Open image with external program or with built-in JFrame?
       (if-not (= image-displayer "auto")
         (doall
          (fs/exec image-displayer new-png-filename)
          (System/exit 0))
         (doall

       ;; Settings for displaying the generated diagram.
       (def img (ref nil))
       
       (defn setup []
         (quil/background 0)
         (dosync (ref-set img (quil/load-image new-png-filename))))
       
       (def img-size
         (with-open [r (java.io.FileInputStream. new-png-filename)]
           (let [image (javax.imageio.ImageIO/read r)
                 img-width (.geWidth image)
                 img-height (.getHeight image)]
             [img-width img-height])))
       
       (defn draw []
         (quil/image @img 0 0))

       ;; Display png file in JFrame.
       (exit-on-close
        (quil/sketch
         :title (str "PDDL Type Diagram - " input-domain-filename)
         :setup setup
         :draw draw
         :size (vec img-size)))))))))
