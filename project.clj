(defproject dojo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/java.jdbc "0.1.4"]
                 [ring "1.1.8"]
                 [net.cgrand/moustache "1.1.0"]
                 [enlive "1.0.0"]
                 [org.xerial/sqlite-jdbc "3.7.2"]]
  :plugins [[lein-swank "1.4.4"]]
  :main dojo.reversi)
