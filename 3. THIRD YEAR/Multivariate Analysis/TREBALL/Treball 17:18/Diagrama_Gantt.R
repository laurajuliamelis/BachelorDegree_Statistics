install.packages("viridisLite")
install.packages("DiagrammeR")

library(viridisLite)
library(DiagrammeR)

m <- mermaid("
        gantt
        dateFormat  YYYY-MM-DD
        title Diagrama de Gantt - Grup Booking
        
        section Entrega D1
        Portada                             :                                   done, des1,    2018-09-27,   2018-09-27
        Definicio del projecte i assignació :                                   done, des2,    2018-09-27,    2018-09-27

        section Entrega D2
        Pla de treball :                                                        done,  import_1,   after des2, 2018-10-04

        section Entrega D3 i D4
        Estructura de les dades, descriptiva i entrega final :                  done, import_2,   after des2, 2018-12-20
        

        
        Arbres de Decisió i Market Basket analysis :                            done, import_3, 2018-10-02, 2018-10-16
        Validació :                                                             done, import_4, 2018-10-16, 2018-10-25
        
        Procés de mineria de dades de la divisió 1 :                            done, import_5, 2018-10-30, 2018-12-13
        Procés de mineria de dades de la divisió 2 :                            done, import_6, 2018-10-30, 2018-12-13
        KNN :                                                                   done, import_7, 2018-10-30, 2018-11-06
        SVM :                                                                   done, import_8, 2018-11-06, 2018-11-13
        LDA :                                                                   done, import_9, 2018-11-13, 2018-11-20
        Naive Bayes :                                                           done, import_10,2018-11-20, 2018-11-27
        ANN :                                                                   done, import_11,2018-11-27, 2018-12-04
        Predictius, clustering i clustering avençat :                           done, import_12,2018-12-04, 2018-12-13
        

        Anàlisi comparativa i conclusions generals :                            done, import_13,2018-12-13, 2018-12-18
        Realització informe i presentació :                                     done, import_14,2018-12-13, 2018-12-18


        section Extras
        Pla de treball real :                                                   done, ex_1, 2018-12-10, 2018-12-18
        Scripts d'R utilitzats :                                                done, ex_2, 2018-12-10, 2018-12-18
")

m$x$config = list(ganttConfig = list(
  axisFormatter = list(list(
    "%b %d, %Y" 
    ,htmlwidgets::JS(
      'function(d){ return d.getDay() == 1 }' 
    )
  ))
))

m
