(defproject mypddl "1.0.0"
  :description "MyPDDL - A Modular Knowledge Engineering System for the Planning Domain Definition Language"
  :url "http://github.com/myPDDL"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [me.raynes/fs "1.4.4"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [pddl-clojure-interface "1.0.2"]
                 [fipp "0.4.1"]
                 [quil "1.7.0"]]
  :main ^:skip-aot mypddl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
