(defsystem "cl-plotly"
  :version "0.1.0"
  :author "Peter Lo"
  :license "MIT License"
  :depends-on ("ec-json")
  :serial t
  :components ((:module "src"
                :components
                ((:file "plot"))))
  :description "A thin wrapper of plotly for Common Lisp"
  )
