(in-package :cl-user)

;; the main point is to produce the correct JS output, which can be
;; done with ec-json through proper Lisp values, either constant or
;; generated with code.

;; For all the possible plot types and their allowed keys, refer to
;; https://plotly.com/javascript/reference/index/

;; Example reference: https://plotly.com/javascript/bar-charts/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic bar chart
(cl-plotly:write-to-html
 '(;; trace
   (:x #("giraffes" "orangutans" "monkeys")
    :y #(20 14 23)
    :type "bar"))
 ;; layout
 nil
 "test_basic_bar_chart.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grouped bar chart
(cl-plotly:write-to-html
 '(;; trace1
   (:x #("giraffes" "orangutans" "monkeys")
    :y #(20 14 23)
    :name "SF Zoo"
    :type "bar")
   ;; trace2
   (:x #("giraffes" "orangutans" "monkeys")
    :y #(12 18 29)
    :name "LA Zoo"
    :type "bar"))
 ;; layout
 '(:barmode "group")
 "test_grouped_bar_chart.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stacked bar chart
(cl-plotly:write-to-html
 '(;; trace1
   (:x #("giraffes" "orangutans" "monkeys")
    :y #(20 14 23)
    :name "SF Zoo"
    :type "bar")
   ;; trace2
   (:x #("giraffes" "orangutans" "monkeys")
    :y #(12 18 29)
    :name "LA Zoo"
    :type "bar"))
 ;; layout
 '(:barmode "stack")
 "test_stacked_bar_chart.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar chart with hover text
(cl-plotly:write-to-html
 '(;; trace1
   (:x #("Liam" "Sophie" "Jacob" "Mia" "William" "Olivia")
    :y #(8.0 8.0 12.0 12.0 13.0 20.0)
    :type "bar"
    :text #("4.17 below the mean"
            "4.17 below the mean"
            "0.17 below the mean"
            "0.17 below the mean"
            "0.83 above the mean"
            "7.83 above the mean")
    :marker (:color "rgb(142,124,195)")))
 ;; layout
 '(:title "Number of Graphs Made this Week"
   :font (:family "Raleway sans-serif")
   :showlegend :false
   :xaxis (:tickangle -45)
   :yaxis (:zeroline :false
           :gridwidth 2)
   :bargap 0.05)
 "test_bar_chart_with_hover_text.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar chart with direct labels
(let ((x-value #("Product A" "Product B" "Product C"))
      (y-value #(20 14 23)))
  (cl-plotly:write-to-html
   `(;; trace1
     (:x ,x-value
      :y ,y-value
      :type "bar"
      :text ,(map 'vector #'cl-plotly:to-string y-value)
      :textposition "auto"
      :hoverinfo "none"
      :marker (:color "rgb(158,202,225)"
               :opacity 0.6
               :line (:color "rgb(8,48,107)"
                      :width 1.5))))
   ;; layout
   '(:title "January 2013 Sales Report"
     :barmode "stack")
   "test_bar_chart_with_direct_labels.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grouped bar chart with direct labels
(let ((x-value #("Product A" "Product B" "Product C"))
      (y-value #(20 14 23))
      (y-value2 #(24 16 20)))
  (cl-plotly:write-to-html
   `(;; trace1
     (:x ,x-value
      :y ,y-value
      :type "bar"
      :text ,(map 'vector #'cl-plotly:to-string y-value)
      :textposition "auto"
      :hoverinfo "none"
      :opacity 0.5
      :marker (:color "rgb(158,202,225)"
               :line (:color "rgb(8,48,107)"
                      :width 1.5)))
     ;; trace2
     (:x ,x-value
      :y ,y-value2
      :type "bar"
      :text ,(map 'vector #'cl-plotly:to-string y-value2)
      :textposition "auto"
      :hoverinfo "none"
      :marker (:color "rgba(58,200,225,.5)"
               :line (:color "rgb(8,48,107)"
                      :width 1.5))))
   ;; layout
   '(:title "January 2013 Sales Report")
   "test_grouped_bar_chart_with_direct_labels.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar chart with rotated labels
(cl-plotly:write-to-html
 '(;; trace1
   (:x #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
    :y #(20 14 25 16 18 22 19 15 12 16 14 17)
    :type "bar"
    :name "Primary Product"
    :marker (:color "rgb(49,130,189)"
             :opacity 0.7))
   ;; trace2
   (:x #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
    :y #(19 14 22 14 16 19 15 14 10 12 12 16)
    :type "bar"
    :name "Secondary Product"
    :marker (:color "rgb(204,204,204)"
             :opacity 0.5)))
 ;; layout
 '(:title "2013 Sales Report"
   :xaxis (:tickangle -45)
   :barmode "group")
 "test_bar_chart_with_rotated_labels.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing individual bar colors
(cl-plotly:write-to-html
 '(;; trace1
   (:x #("Feature A" "Feature B" "Feature C" "Feature D" "Feature E")
    :y #(20 14 23 25 22)
    :marker (:color #("rgba(204,204,204,1)"
                      "rgba(222,45,38,0.8)"
                      "rgba(204,204,204,1)"
                      "rgba(204,204,204,1)"
                      "rgba(204,204,204,1)"))
    :type "bar"))
 ;; layout
 '(:title "Least Used Feature")
 "test_customizing_individual_bar_colors.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing individual bar widths
(cl-plotly:write-to-html
 '(;; trace0
   (:type "bar"
    :x #(1 2 3 5.5 10)
    :y #(10 8 6 4 2)
    :width #(0.8 0.8 0.8 3.5 4)))
   ;; layout
 nil
 "test_customizing_individual_bar_widths.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizing individual bar base
(cl-plotly:write-to-html
 '(;; trace0
   (:type "bar"
    :x #("2016" "2017" "2018")
    :y #(500 600 700)
    :base #(-500 -600 -700)
    :hovertemplate "%(base)"
    :marker (:color "red")
    :name "expenses")
   ;; trace1
   (:type "bar"
    :x #("2016" "2017" "2018")
    :y #(300 400 700)
    :base 0
    :marker (:color "blue")
    :name "revenue"))
 ;; layout
 nil
 "test_customizing_individual_bar_base.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; colored and styled bar chart
(cl-plotly:write-to-html
 '(;; trace1
   (:x #(1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012)
    :y #(219 146 112 127 124 180 236 207 236 263 350 430 474 526 488 537 500 439)
    :name "Rest of world"
    :marker (:color "rgb(55, 83, 109)")
    :type "bar")
   ;; trace2
   (:x #(1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012)
    :y #(16 13 10 11 28 37 43 55 56 88 105 156 270 299 340 403 549 499)
    :name "China"
    :marker (:color "rgb(26, 118, 255)")
    :type "bar"))
 ;; layout
 '(:title "US Export of Plastic Scrap"
   :xaxis (:tickfont (:size 14
                      :color "rgb(107, 107, 107)"))
   :yaxis (:title "USD (millions)"
           :titlefont (:size 16
                       :color "rgb(107, 107, 107)")
           :tickfont (:size 14
                      :color "rgb(107, 107, 107)"))
   :legend (:x 0
            :y 1.0
            :bgcolor "rgba(255, 255, 255, 0)"
            :bordercolor "rgba(255, 255, 255, 0)")
   :barmode "group"
   :bargap 0.15
   :bargroupgap 0.1
   )
 "test_colored_and_styled_bar_chart.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; waterfall bar chart
(let ((x-data #("Product<br>Revenue"
                "Services<br>Revenue"
                "Total<br>Revenue"
                "Fixed<br>Costs"
                "Variable<br>Costs"
                "Total<br>Costs"
                "Total"
                ))
      (y-data #(400 660 660 590 400 400 340))
      (text-list #("$430K" "$260K" "$690K" "$-120K" "$-200K" "$-320K" "$370K")))
  ;;
  (cl-plotly:write-to-html
   `(;; Base
     ;; trace1
     (:x ,x-data
      :y #(0 430 0 570 370 370 0)
      :marker (:color "rgba(1,1,1,0.0)")
      :type "bar")
     ;; Revenue
     ;; trace2
     (:x ,x-data
      :y #(430 260 690 0 0 0 0)
      :type "bar"
      :marker (
               :color "rgba(55,128,191,0.7)"
               :line (:color "rgba(55,128,191,1.0)" 
                      :width 2)))
     ;; Cost
     ;; trace3
     (:x ,x-data
      :y #(0 0 0 120 200 320 0)
      :type "bar"
      :marker (:color "rgba(219 64 82 0.7)"
               :line (:color "rgba(219 64 82 1.0)"
                      :width 2)))
     ;; Profit
     ;; trace4
     (:x ,x-data
      :y #(0 0 0 0 0 0 370)
      :type "bar"
      :marker (:color "rgba(50,171 96 0.7)"
               :line (:color "rgba(50,171,96,1.0)"
                      :width 2))))
   ;; layout
   `(:title "Annual Profit 2015"
     :barmode "stack"
     :paper_bgcolor "rgba(245,246,249,1)"
     :plot_bgcolor "rgba(245,246,249,1)"
     :width 600
     :height 600
     :showlegend :false
     :annotations ,(map 'vector
                        #'(lambda (x y text)
                            `(:x ,x
                              :y ,y
                              :text ,text
                              :font (:family "Arial"
                                     :size 14
                                     :color "rgba(245,246,249,1)")
                              :showarrow :false))
                        x-data y-data text-list))
   "test_waterfall_bar_chart.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bar chart with relative barmode
(cl-plotly:write-to-html
 '(;; trace1
   (:x #(1 2 3 4)
    :y #(1 4 9 16)
    :name "Trace1"
    :type "bar")
   ;; trace2
   (:x #(1 2 3 4)
    :y #(6 -8 -4.5 8)
    :name "Trace2"
    :type "bar")
   ;; trace3
   (:x #(1 2 3 4)
    :y #(-15 -3 4.5 -8)
    :name "Trace3"
    :type "bar")
   ;; trace4
   (:x #(1 2 3 4)
    :y #(-1 3 -3 -4)
    :name "Trace4"
    :type "bar"))
 ;; layout
 '(:xaxis (:title "X axis")
   :yaxis (:title "Y axis")
   :barmode "relative"
   :title "Relative Barmode")
 "test_bar_chart_with_relative_barmode.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
