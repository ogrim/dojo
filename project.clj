(defproject dojo "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.reader "0.8.8"]
                 [org.clojure/java.jdbc "0.3.5"]
                 [ring "1.3.1"]
                 [net.cgrand/moustache "1.1.0"]
                 [enlive "1.1.5"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [instaparse "1.3.3"]]
  :main dojo.reversi)
