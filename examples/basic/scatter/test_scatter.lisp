(in-package :cl-user)

;; the main point is to produce the correct JS output, which can be
;; done with ec-json through proper Lisp values, either constant or
;; generated with code.

;; For all the possible plot types and their allowed keys, refer to
;; https://plotly.com/javascript/reference/index/

;; Example reference: https://plotly.com/javascript/line-and-scatter/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line and scatter plot
(cl-plotly:write-to-html
 '(
   ;; trace1
   (:x #(1 2 3 4)
    :y #(10 15 13 17)
    :mode "markers"
    :type "scatter")
   ;; trace2
   (:x #(2 3 4 5)
    :y #(16 5 11 9)
    :mode "lines"
    :type "scatter")
   ;; trace3
   (:x #(1 2 3 4)
    :y #(12 9 15 12)
    :mode "lines+markers"
    :type "scatter"))
 ;; layout
 nil
 "test_line_and_scatter.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data labels hover
(cl-plotly:write-to-html
 '(
   ;; trace1
   (:x #(1 2 3 4 5)
    :y #(1 6 3 6 1)
    :mode "markers"
    :type "scatter"
    :name "Team A"
    :text #("A-1" "A-2" "A-3" "A-4" "A-5")
    :marker (:size 12))
   ;; trace2
   (:x #(1.5 2.5 3.5 4.5 5.5)
    :y #(4 1 7 1 4)
    :mode "markers"
    :type "scatter"
    :name "Team B"
    :text #("B-a" "B-b" "B-c" "B-d" "B-e")
    :marker (:size 12)))
 ;; layout
 '(:xaxis (:range #(0.75 5.25))
   :yaxis (:range #(0 8))
   :title "Data Labels Hover")
 "test_data_labels_hover.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data labels on the plot
(cl-plotly:write-to-html
 '(
   ;; trace1
   (:x #(1 2 3 4 5)
    :y #(1 6 3 6 1)
    :mode "markers+text"
    :type "scatter"
    :name "Team A"
    :text #("A-1" "A-2" "A-3" "A-4" "A-5")
    :textposition "top center"
    :textfont (:family  "Raleway sans-serif")
    :marker (:size 12))
   ;; trace2
   (:x #(1.5 2.5 3.5 4.5 5.5)
    :y #(4 1 7 1 4)
    :mode "markers+text"
    :type "scatter"
    :name "Team B"
    :text #("B-a" "B-b" "B-c" "B-d" "B-e")
    :textfont (:family "Times New Roman")
    :textposition "bottom center"
    :marker (:size 12)))
 ;; layout
 '(:xaxis (:range #(0.75 5.25))
   :yaxis (:range #(0 8))
   :legend (:y 0.5
            :yref "paper"
            :font (:family "Arial sans-serif"
                   :size 20
                   :color "grey"))
   :title "Data Labels on the Plot")
 "test_data_labels_on_the_plot.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scatter plot with a color dimension
(cl-plotly:write-to-html
 '(
   ;; trace1
   (:y #(5 5 5 5 5 5 5 5 5 5
         5 5 5 5 5 5 5 5 5 5
         5 5 5 5 5 5 5 5 5 5
         5 5 5 5 5 5 5 5 5 5)
    :mode "markers"
    :marker (:size 40
             :color #(0 1 2 3 4 5 6 7 8 9
                      10 11 12 13 14 15 16 17 18 19
                      20 21 22 23 24 25 26 27 28 29
                      30 31 32 33 34 35 36 37 38 39))))
 ;; layout
 '(:title "Scatter Plot with a Color Dimension")
 "test_scatter_plot_with_a_color_dimension.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grouped scatter plot
(cl-plotly:write-to-html
 '(
   ;; trace1
   (:x #("South Korea" "China" "Canada")
    :y #(24 10 9)
    :name "Gold"
    :type "scatter"
    :mode "markers")
   ;; trace2
   (:x #("South Korea" "China" "Canada")
    :y #(13 15 12)
    :name "Silver"
    :type "scatter"
    :mode "markers")
   ;; trace3
   (:x #("South Korea" "China" "Canada")
    :y #(11 8 12)
    :name "Bronze"
    :type "scatter"
    :mode "markers"))
 ;; layout
 '(:scattermode "group"
   :title "Grouped by Country"
   :xaxis (:title  "Country")
   :yaxis (:title  "Medals"))
 "test_grouped_scatter_plot.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grouped scatter plot with custom scatter gap
(cl-plotly:write-to-html
 '(
   ;; trace1
   (:x #("South Korea" "China" "Canada")
    :y #(24 10 9)
    :name "Gold"
    :type "scatter"
    :mode "markers")
   ;; trace2
   (:x #("South Korea" "China" "Canada")
    :y #(13 15 12)
    :name "Silver"
    :type "scatter"
    :mode "markers")
   ;; trace3
   (:x #("South Korea" "China" "Canada")
    :y #(11 8 12)
    :name "Bronze"
    :type "scatter"
    :mode "markers"))
 ;; layout
 '(:scattermode "group"
   :title "Grouped by Country"
   :xaxis (:title  "Country")
   :yaxis (:title  "Medals")
   :scattergap 0.7)
 "test_grouped_scatter_plot_with_custom_scatter_gap.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
