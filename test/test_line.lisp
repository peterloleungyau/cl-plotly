(in-package :cl-user)

(use-package :ec-json)

;; the main point is to produce the correct JS output, which can be
;; done with ec-json through proper Lisp values, either constant or
;; generated with code.

;; For all the possible plot types and their allowed keys, refer to
;; https://plotly.com/javascript/reference/index/

;; Example reference: https://plotly.com/javascript/line-charts/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic line plot

(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4)
    :y #(10 15 13 17)
    :type "scatter")
   (:x #(1 2 3 4)
    :y #(16 5 11 9)
    :type "scatter"))
 ;; layout
 nil
 "test_basic_line.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line and scatter plot

(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4)
    :y #(10 15 13 17)
    :mode "markers")
   (:x #(2 3 4 5)
    :y #(16 5 11 9)
    :mode "lines")
   (:x #(1 2 3 4)
    :y #(12 9 15 12)
    :mode "lines+markers"))
 ;; layout
 '(:title "Line and Scatter Plot")
 "test_line_and_scatter.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adding names to line and scatter plot
(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4)
    :y #(10 15 13 17)
    :mode "markers"
    :name "Scatter")
   (:x #(2 3 4 5)
    :y #(16 5 11 9)
    :mode "lines"
    :name "Lines")
   (:x #(1 2 3 4)
    :y #(12 9 15 12)
    :mode "lines+markers"
    :name "Scatter + Lines"))
 ;; layout
 '(:title "Adding Names to Line and Scatter Plot")
 "test_adding_names_to_line_and_scatter.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line and scatter styling
(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4)
    :y #(10 15 13 17)
    :mode "markers"
    :marker (:color "rgb(219, 64, 82)"
             :size 12))
   (:x #(2 3 4 5)
    :y #(16 5 11 9)
    :mode "lines"
    :line (:color "rgb(55, 128, 191)"
           :width 3))
   (:x #(1 2 3 4)
    :y #(12 9 15 12)
    :mode "lines+markers"
    :marker (:color "rgb(128, 0, 128)"
             :size 8)
    :line (:color "rgb(128, 0, 128)"
           :width 1)))
 ;; layout
 '(:title "Line and Scatter Styling")
 "test_line_and_scatter_styling.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; styling line plot
(cl-plotly:write-to-html
 ;; traces
 '((:type "scatter"
    :x #(1 2 3 4)
    :y #(10 15 13 17)
    :mode "lines"
    :name "Red"
    :line (:color "rgb(219, 64, 82)"
           :width 3))
   (:type "scatter"
    :x #(1 2 3 4)
    :y #(12 9 15 12)
    :mode "lines"
    :name "Blue"
    :line (:color "rgb(55, 128, 191)"
           :width 1)))
 ;; layout
 '(:width 500
   :height 500)
 "test_styling_line.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; colored and styled scatter plot
(cl-plotly:write-to-html
 ;; traces
 '((:x #(52698 43117)
    :y #(53 31)
    :mode "markers"
    :name "North America"
    :text #("United States" "Canada")
    :marker (:color "rgb(164, 194, 244)"
             :size 12
             :line (:color "white"
                    :width 0.5))
    :type "scatter")
   (:x #(39317 37236 35650 30066 29570 27159 23557 21046 18007)
    :y #(33 20 13 19 27 19 49 44 38)
    :mode "markers"
    :name "Europe"
    :text #("Germany" "Britain" "France" "Spain" "Italy" "Czech Rep." "Greece" "Poland")
    :marker (:color "rgb(255, 217, 102)"
             :size 12)
    :type "scatter")
   (:x #(42952 37037 33106 17478 9813 5253 4692 3899)
    :y #(23 42 54 89 14 99 93 70)
    :mode "markers"
    :name "Asia/Pacific"
    :text #("Australia" "Japan" "South Korea" "Malaysia" "China" "Indonesia" "Philippines" "India")
    :marker (:color "rgb(234, 153, 153)"
             :size 12)
    :type "scatter")
   (:x #(19097 18601 15595 13546 12026 7434 5419)
    :y #(43 47 56 80 86 93 80)
    :mode "markers"
    :name "Latin America"
    :text #("Chile" "Argentina" "Mexico" "Venezuela" "Venezuela" "El Salvador" "Bolivia")
    :marker (:color "rgb(142 124 195)"
             :size 12)
    :type "scatter"))
 ;; layout
 `(:title "Quarter 1 Growth"
   :xaxis (:title "GDP per Capita"
           :showgrid :false
           :zeroline :false)
   :yaxis (:title "Percent"
           :showline :false))
 "test_colored_and_styled_scatter.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line shape options for interpolation
(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4 5)
    :y #(1 3 2 3 1)
    :mode "lines+markers"
    :name "linear"
    :line (:shape "linear")
    :type "scatter")
   (:x #(1 2 3 4 5)
    :y #(6 8 7 8 6)
    :mode "lines+markers"
    :name "spline"
    :text #("tweak line smoothness<br>with 'smoothing' in line object"
            "tweak line smoothness<br>with 'smoothing' in line object"
            "tweak line smoothness<br>with 'smoothing' in line object"
            "tweak line smoothness<br>with 'smoothing' in line object"
            "tweak line smoothness<br>with 'smoothing' in line object"
            "tweak line smoothness<br>with 'smoothing' in line object")
    :line (:shape "spline")
    :type "scatter")
   (:x #(1 2 3 4 5)
    :y #(11 13 12 13 11)
    :mode "lines+markers"
    :name "vhv"
    :line (:shape "vhv")
    :type "scatter")
   (:x #(1 2 3 4 5)
    :y #(16 18 17 18 16)
    :mode "lines+markers"
    :name "hvh"
    :line (:shape "hvh")
    :type "scatter")
   (:x #(1 2 3 4 5)
    :y #(21 23 22 23 21)
    :mode "lines+markers"
    :name "vh"
    :line (:shape "vh")
    :type "scatter")
   (:x #(1 2 3 4 5)
    :y #(26 28 27 28 26)
    :mode "lines+markers"
    :name "hv"
    :line (:shape "hv")
    :type "scatter"))
 ;; layout
 '(:legend (:y 0.5
            :traceorder "reversed"
            :font (:size 16)
            :yref "paper"))
 "test_line_shape_options_for_interpolation.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graph and axes titles
(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4)
    :y #(10 15 13 17)
    :mode "markers"
    :name "Scatter")
   (:x #(2 3 4 5)
    :y #(16 5 11 9)
    :mode "lines"
    :name "Lines")
   (:x #(1 2 3 4)
    :y #(12 9 15 12)
    :mode "lines+markers"
    :name "Scatter and Lines"))
 ;; layout
 '(:title "Title of the Graph"
   :xaxis (:title "x-axis title")
   :yaxis (:title "y-axis title"))
 "test_graph_and_axes_titles.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line dash
(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4 5)
    :y #(1 3 2 3 1)
    :mode "lines"
    :name "Solid"
    :line (:dash "solid"
           :width 4))
   (:x #(1 2 3 4 5)
    :y #(6 8 7 8 6)
    :mode "lines"
    :name "dashdot"
    :line (:dash "dashdot"
           :width 4))
   (:x #(1 2 3 4 5)
    :y #(11 13 12 13 11)
    :mode "lines"
    :name "Solid"
    :line (:dash "solid"
           :width 4))
   (:x #(1 2 3 4 5)
    :y #(16 18 17 18 16)
    :mode "lines"
    :name "dot"
    :line (:dash "dot"
           :width 4)))
 ;; layout
 '(:title "Line Dash"
   :xaxis (:range #(0.75 5.25)
           :autorange :false)
   :yaxis (:range #(0 18.5)
           :autorange :false)
   :legend (:y 0.5
            :traceorder "reversed"
            :font (:size 16)))
 "test_line_dash.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connect gaps between data
(cl-plotly:write-to-html
 ;; traces
 '((:x #(1 2 3 4 5 6 7 8)
    :y #(10 15 :null 17 14 12 10 :null 15)
    :mode "lines+markers"
    :connectgaps :true)
   (:x #(1 2 3 4 5 6 7 8)
    :y #(16 :null 13 10 8 :null 11 12)
    :mode "lines"
    :connectgaps :true))
 ;; layout
 '(:title "Connect the Gaps Between Data"
   :showlegend :false)
 "test_connect_gaps_between_data.html")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; labelling lines with annotations

(let* ((x #(2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2013))
       (y-data
         #(#(74 82 80 74 73 72 74 70 70 66 66 69)
           #(45 42 50 46 36 36 34 35 32 31 31 28)
           #(13 14 20 24 20 24 24 40 35 41 43 50)
           #(18 21 18 21 16 14 13 18 17 16 19 23)))
       (colors
         #("rgba(67,67,67,1)"
           "rgba(115,115,115,1)"
           "rgba(49,130,189, 1)"
           "rgba(189,189,189,1)"))
       (line-size #(2 2 4 2))
       (line-labels #("Television" "Newspaper" "Internet" "Radio"))
       (traces
         (loop :for i :from 0 :below (length y-data)
               :collect `(:x ,x
                          :y ,(aref y-data i) 
                          :type "scatter"
                          :mode "lines"
                          :line (:color ,(aref colors i)
                                 :width ,(aref line-size i)))
               :collect `(:x ,(vector (aref x 0) (aref x 11))
                          :y ,(vector (aref (aref y-data i) 0)
                                      (aref (aref y-data i) 11))
                          :type "scatter"
                          :mode "markers"
                          :marker (:color ,(aref colors i)
                                          :size 12))))
       (layout
         `(:showlegend :false
           :height 600
           :width 600
           :xaxis (:showline :true
                   :showgrid :false
                   :showticklabels :true
                   :linecolor "rgb(204,204,204)"
                   :linewidth 2
                   :autotick :false
                   :ticks "outside"
                   :tickcolor "rgb(204,204,204)"
                   :tickwidth 2
                   :ticklen 5
                   :tickfont (:family "Arial"
                              :size 12
                              :color "rgb(82, 82, 82)"))
           :yaxis (:showgrid :false
                   :zeroline :false
                   :showline :false
                   :showticklabels :false)
           :autosize :false
           :margin (:autoexpand :false
                    :l 100
                    :r 20
                    :t 100)
           :annotations ,(concatenate
                          'vector
                          '((:xref "paper"
                             :yref "paper"
                             :x 0.0
                             :y 1.05
                             :xanchor "left"
                             :yanchor "bottom"
                             :text "Main Source for News"
                             :font(:family "Arial"
                                   :size 30
                                   :color "rgb(37,37,37)")
                             :showarrow :false)
                            (:xref "paper"
                             :yref "paper"
                             :x 0.5
                             :y -0.1
                             :xanchor "center"
                             :yanchor "top"
                             :text "Source: Pew Research Center & Storytelling with data"
                             :showarrow :false
                             :font (:family "Arial"
                                    :size 12
                                    :color "rgb(150,150,150)")))
                          ;;
                          (loop :for i :below (length y-data)
                                :collect `(:xref "paper"
                                           :x 0.05
                                           :y ,(aref (aref y-data i) 0)
                                           :xanchor "right"
                                           :yanchor "middle"
                                           :text ,(format nil "~A ~A%"
                                                          (aref line-labels i)
                                                          (aref (aref y-data i) 0))
                                           :showarrow :false
                                           :font (:family "Arial"
                                                  :size 16
                                                  :color "black"))
                                :collect `(:xref "paper"
                                           :x 0.95
                                           :y ,(aref (aref y-data i) 11)
                                           :xanchor "left"
                                           :yanchor "middle"
                                           :text ,(format nil "~A%"
                                                          (aref (aref y-data i) 11))
                                           :font (:family "Arial"
                                                  :size 16
                                                  :color "black")
                                           :showarrow :false))))))
  ;;
  (cl-plotly:write-to-html
   traces layout
   "test_labelling_lines_with_annotations.html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
