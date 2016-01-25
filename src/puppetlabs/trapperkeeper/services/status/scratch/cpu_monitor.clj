(ns puppetlabs.trapperkeeper.services.status.scratch.cpu-monitor
  (:require [clojure.java.jmx :as jmx]
            [puppetlabs.kitchensink.core :as ks])
  (:import (java.lang.management ManagementFactory)
           (java.util ArrayList)))

(defn get-process-cpu-time
  []
  (let [bean-cpu-time (jmx/read "java.lang:type=OperatingSystem" :ProcessCpuTime)
        ;; TODO: this value could be cached/memoized
        cpu-multiplier (if (contains?
                            (vec (jmx/attribute-names "java.lang:type=OperatingSystem"))
                            :ProcessingCapacity)
                         (jmx/read "java.lang:type=OperatingSystem" :ProcessingCapacity)
                         1)]
    (* bean-cpu-time cpu-multiplier)))

(defn get-collection-time
  []
  ;; TODO: could cache/memoize bean names?  should also add error handling in
  ;;  case it's possible that some bean w/o a :CollectionTime attribute ends up
  ;;  in the list?
  (let [gc-bean-names (jmx/mbean-names "java.lang:type=GarbageCollector,*")]
    (apply + (map #(jmx/read % :CollectionTime) gc-bean-names))))

(defn calculate-usage
  [process-time prev-process-time uptime-diff]
  (if (or (= -1 prev-process-time) (<= uptime-diff 0))
    0
    (let [process-time-diff (- process-time prev-process-time)]
      (min (* 100 (/ process-time-diff uptime-diff)) 100))))

(defn get-cpu-values
  [{prev-uptime :uptime
    prev-process-cpu-time :process-cpu-time
    prev-process-gc-time :process-gc-time
    :as prev-snapshot}]
  (let [runtime-bean (ManagementFactory/getRuntimeMXBean)
        ;; could cache / memoize num-cpus
        num-cpus (ks/num-cpus)
        uptime (* (.getUptime runtime-bean) 1000000)
        process-cpu-time (/ (get-process-cpu-time) num-cpus)
        process-gc-time (/ (* (get-collection-time) 1000000) num-cpus)
        uptime-diff (if (= -1 prev-uptime) uptime (- uptime prev-uptime))
        cpu-usage (calculate-usage process-cpu-time prev-process-cpu-time uptime-diff)
        gc-usage (calculate-usage process-gc-time prev-process-gc-time uptime-diff)]
    {:snapshot {:uptime uptime
                :process-cpu-time process-cpu-time
                :process-gc-time process-gc-time}
     :cpu-usage (float (max cpu-usage 0))
     :gc-usage (float (max gc-usage 0))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCRATCH CODE for playing around with this
(def last-snapshot (atom nil))

(def monitor-future (atom nil))
(def malloc-future (atom nil))

(defn start-monitor
  []
  (if-not (nil? @monitor-future)
    (future-cancel @monitor-future))
  (reset! last-snapshot {:uptime -1
                         :process-cpu-time -1
                         :process-gc-time -1})
  (reset! monitor-future (future
                          (while (not (Thread/interrupted))
                            (let [cpu-values (get-cpu-values @last-snapshot)]
                              (reset! last-snapshot (:snapshot cpu-values))
                              (println "GOT NEW VALUES")
                              (clojure.pprint/pprint cpu-values))
                            (Thread/sleep 1000)))))

(defn stop-monitor
  []
  (future-cancel @monitor-future)
  (reset! monitor-future nil))

(defn start-malloc
  []
  (if-not (nil? @malloc-future)
    (future-cancel @malloc-future))
  (reset! malloc-future (future (while (not (Thread/interrupted))
                                  (let [al (ArrayList.)]
                                    (doseq [i (range 2000000)]
                                      (.add al (Math/random))
                                      (Thread/yield)))))))

(defn stop-malloc
  []
  (future-cancel @malloc-future)
  (reset! malloc-future nil))