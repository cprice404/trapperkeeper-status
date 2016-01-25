(ns puppetlabs.trapperkeeper.services.status.scratch.cpu-monitor
  (:import (java.lang.management ManagementFactory)))

;long[] getValues(MonitoredData data) {
; long cpuUsage = -1;
; long gcUsage = -1;
;
; long upTime = data.getUpTime() * 1000000;
;
; long processCpuTime = cpuSupported ?
; data.getProcessCpuTime() / processorsCount : -1;
; long processGcTime  = gcSupported ?
; data.getCollectionTime() * 1000000 / processorsCount : -1;
;
; if (prevUpTime != -1) {
;  long upTimeDiff = upTime - prevUpTime;
;
;  if (cpuSupported && prevProcessCpuTime != -1) {
;   long processTimeDiff = processCpuTime - prevProcessCpuTime;
;   cpuUsage = upTimeDiff > 0 ? Math.min((long)(1000 * (float)processTimeDiff /
;    (float)upTimeDiff), 1000) : 0;
;  }
;
;  if (gcSupported && prevProcessGcTime != -1) {
;   long processGcTimeDiff = processGcTime - prevProcessGcTime;
;   gcUsage = upTimeDiff > 0 ? Math.min((long)(1000 * (float)processGcTimeDiff /
;    (float)upTimeDiff), 1000) : 0;
;   if (cpuUsage != -1 && cpuUsage < gcUsage) gcUsage = cpuUsage;
;  }
; }
;
; prevUpTime = upTime;
; prevProcessCpuTime = processCpuTime;
; prevProcessGcTime  = processGcTime;
;
; return new long[] {
;                   Math.max(cpuUsage, 0),
;                   Math.max(gcUsage, 0)
;                   };
;}

(defn get-cpu-values
  [prev-uptime prev-process-cpu-time prev-process-gc-time]
  (let [runtime-bean (ManagementFactory/getRuntimeMXBean)
        ;cpu-usage -1
        ;gc-usage -1
        uptime (* (.getUptime runtime-bean) 1000000)
        process-cpu-time (/ (get-process-cpu-time) num-cpus)
        process-gc-time (/ (* (get-collection-time) 1000000)
                           num-cpus)
        uptime-diff (if (= -1 prev-uptime) uptime (- uptime prev-uptime))
        cpu-usage (if (= -1 prev-process-cpu-time)
                    0
                    (let [process-time-diff (- process-cpu-time prev-process-cpu-time)]
                      (if (> uptime-diff 0)
                        (min (* 1000 (/ process-time-diff uptime-diff)) 1000)
                        0)))
        gc-usage (if (= -1 prev-process-gc-time)
                   0
                   (let [process-gc-time-diff (- process-gc-time prev-process-gc-time)]
                     (if (> uptime-diff 0)
                       (min (* 1000 (/ process-gc-time-diff uptime-diff)) 1000)
                       0)))]
    #_(when (not= -1 prev-uptime)
      (let [uptime-diff (- uptime prev-uptime)]
        (when (not= -1 prev-process-cpu-time)
          (let [process-time-diff (- process-cpu-time prev-process-cpu-time)
                ;; TODO: this is going to go out of scope
                cpu-usage (if (> uptime-diff 0)
                            (min (* 1000 (/ process-time-diff uptime-diff)) 1000)
                            0)]))
        (when (not= -1 prev-process-gc-time)
          (let [process-gc-time-diff (- process-gc-time prev-process-gc-time)
                ;; TODO: out of scope
                gc-usage (if (> uptime-diff 0)
                           (min (* 1000 (/ process-gc-time-diff uptime-diff)) 1000)
                           0)
                gc-usage (if (< cpu-usage gc-usage)
                           cpu-usage
                           gc-usage)]))))
    {:snapshot {:uptime uptime
                :process-cpu-time process-cpu-time
                :process-gc-time process-gc-time}
     :cpu-usage (max cpu-usage 0)
     :gc-usage (max gc-usage 0)}))

