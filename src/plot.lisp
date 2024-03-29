(in-package :cl-user)

(defpackage :cl-plotly
  (:use :cl :ec-json)
  ;; TODO: export symbols
  (:export
   print-as-html
   write-to-html
   to-string
   ))

(in-package :cl-plotly)

;;;;
;;; generate plot JS

(defvar *plotly-js-url* "https://cdn.plot.ly/plotly-latest.min.js"
  "The URL of the plotly minified JS script file.
Please refer to https://plotly.com/javascript/getting-started/ for the possible URLs.
You can also manually download it to your system, and set this variable to the local path.")

(defun print-plot-js (traces layout &optional (out *standard-output*) config)
  ;; traces
  (do ((i 0 (1+ i))
       (trs traces (cdr trs)))
      ((null trs))
    (format out "var trace~A = " i)
    (print-as-json (car trs) out)
    (format out ";~3&"))
  ;; layout
  (format out "var layout = ")
  (print-as-json layout out)
  (format out ";~3&")
  ;;
  (format out "var data = [")
  (dotimes (i (length traces))
    (format out "trace~A, " i))
  (format out "];~3&Plotly.newPlot('myDiv', data, layout")
  (when config
    (format out ", ")
    (print-as-json config out))
  (format out ");")
  )

;;;;
;;; generate html

(defun print-as-html (traces layout
                      &key
                        (out *standard-output*)
                        config
                        (plotly-js-url *plotly-js-url*))
  ;; Since the boilerplat html file is simple and fixed, we directly print it.
  (format out "<html>
<head>
<style>html { height: 100%; }
body { height: 100%; display: flex; justify-content: center; align-items:
center; }
</style>
<script type='text/javascript' src='~A' charset='utf-8'></script>
</head>
<body>
<div id='myDiv'></div>
<script type='text/javascript'>
" plotly-js-url)
  ;; generate the js for the traces and layout
  (print-plot-js traces layout out config)
  ;;
  (format out "
</script>
</body>
</html>"))

(defun write-to-html (traces layout out-file-path &key (plotly-js-url *plotly-js-url*) config)
  (ensure-directories-exist out-file-path)
  (with-open-file (out out-file-path
                       :direction :output
                       :if-exists :supersede)
    (print-as-html traces layout :out out :plotly-js-url plotly-js-url :config config)))

;;;;
;;; some convenient utility

(defun to-string (x)
  (format nil "~A" x))
