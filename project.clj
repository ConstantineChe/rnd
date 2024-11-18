(defproject dcnn "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [clj-http "3.12.3"]
                 [quil "3.1.0"]
                 [net.mikera/core.matrix "0.63.0"]
                 [adamdavislee.number-theory "1.0.0-alpha"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.bouncycastle/bcprov-jdk15on "1.68"]
                 [io.github.binance/binance-connector-java "3.2.0"]
                 [org.jopendocument/jOpenDocument "1.3"]
                 [org.clojure/data.csv "1.1.0"]]
  :repl-options {:init-ns dcnn.core})
