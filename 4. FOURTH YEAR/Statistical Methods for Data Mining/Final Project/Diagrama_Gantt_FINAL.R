install.packages("viridisLite")
install.packages("DiagrammeR")

library(viridisLite)
library(DiagrammeR)

m <- mermaid("
        gantt
        dateFormat  YYYY-MM-DD
        title Diagrama de Gantt Final - Grup Booking
        
        section Entrega D1
        Portada                             :                                   done, des1,    2018-09-27,   2018-09-27
        Definicio del projecte i assignació :                                   done, des2,    2018-09-27,    2018-09-27

        section Entrega D2
        Pla de treball :                                                        done,  import_1,   after des2, 2018-10-04

        section Entrega D3 i D4
        Estructura de les dades, descriptiva :                                  done, import_2,   after des2, 2018-12-20
        

        
        Anàlisi descriptiva univariant inicial :                                done, import_3, 2018-10-01, 2018-10-07
        Procés de preprocessing :                                               done, import_4, 2018-10-07, 2018-10-15
        Descriptiva univariant de les dades preprocessades :                    done, import_4, 2018-10-15, 2018-10-20
        
        Procés de mineria de dades de la divisió 1 :                            done, import_5, 2018-10-22, 2018-12-17
        1. Clustering jerarquitzat :                                            done, import_7, 2018-10-22, 2018-11-02
        2. ACP :                                                                done, import_8, 2018-11-04, 2018-11-10
        3. Arbres de decisió :                                                  done, import_9, 2018-11-12, 2018-11-29
        4. Mètode de la regressió :                                             done, import_9, 2018-11-29, 2018-12-12
        Procés de mineria de dades de la divisió 2 :                            done, import_6, 2018-10-22, 2018-12-17
        1. Clustering K-means :                                                 done, import_10,2018-10-22, 2018-11-03
        2. Regles d'associació:                                                 done, import_11,2018-11-05, 2018-11-14
        3. SVM :                                                                done, import_10,2018-11-14, 2018-11-28
        4. ANN :                                                                done, import_11,2018-11-28, 2018-12-13

        Anàlisi comparativa i conclusions generals :                            done, import_13,2018-12-13, 2018-12-17
        Realització informe i presentació :                                     done, import_14,2018-12-13, 2018-12-17


        section Extras
        Pla de treball real :                                                   done, ex_1, 2018-12-10, 2018-12-20
        Scripts d'R utilitzats :                                                done, ex_2, 2018-11-02, 2018-12-14
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
