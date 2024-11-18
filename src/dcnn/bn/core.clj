(ns dcnn.bn.core
  (:import [com.binance.connector.client WebSocketStreamClient]
           (com.binance.connector.client.impl WebSocketStreamClientImpl)
           (com.binance.connector.client.utils.websocketcallback WebSocketMessageCallback)))

(defonce ws-stream-client (WebSocketStreamClientImpl.))

(defn start-trade-stream [s]
  (.aggTradeStream ws-stream-client s (reify WebSocketMessageCallback (onMessage [this e] (println e)))))

(defn stop-all []
  (.closeAllConnections ws-stream-client))


(comment
  (start-trade-stream "btcusdt")
  (stop-all))
