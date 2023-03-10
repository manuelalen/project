## LIBRERIAS ---------------
library(DT)

## VARIABLES ----------
Produccion <- 25516952000
Aprovisionamientos <- 18871088000
Deterioro_y_amortizaciones <- 689801
Impuestos <- 156407000
Capital_Constante <- Aprovisionamientos + Deterioro_y_amortizaciones + Impuestos
Capital_Variable <- 3381530000

Plusvalia <- Produccion - Capital_Constante - Capital_Variable - Impuestos
Tasa_Plusvalia <- Plusvalia/Capital_Variable
Tasa_Ganancia <- Plusvalia/(Capital_Constante+Capital_Variable)
Acumulación_Capital <- Capital_Constante/Capital_Variable
Ciclos_Capital <- 1+Acumulación_Capital+Tasa_Plusvalia
Produccion_Estimada <- Capital_Constante+Capital_Variable+Plusvalia
Tasa_Acierto <- 100*(Produccion_Estimada/Produccion)

df_marxismo <- data.frame(
  Variable <- c("Plusvalia","Tasa de Plusvalia", "Tasa de Ganancia","Acumulación de Capital",
                "Ciclos del Capital","Tasa Acierto"),
  Valor <- c(Plusvalia,100*Tasa_Plusvalia,100*Tasa_Ganancia,Acumulación_Capital,Ciclos_Capital,Tasa_Acierto),
  Valor_Nacional <- c(806010793*1000,1.261195995*100,0.2686673796*100,
                      3.694265441,5.955461436,99.23)
)

colnames(df_marxismo) <- c("Variable","Valor","Valor_Nacional")

DATA_TABLE <- datatable(df_marxismo, options = list(
  columnDefs = list(list(className = 'dt-center', targets = 1:5),
                    list(targets = 5, visible = FALSE)),
  pageLength = 5,
  lengthMenu = c(5, 10, 15, 20)
))


DATA_TABLE


######### 2017 ####################
Produccion_4 <- 24680682000
Aprovisionamientos_4 <- 181470820000
Deterioro_y_amortizaciones_4 <- 630679000
Impuestos_4 <- 195424000
Capital_Constante_4 <- Aprovisionamientos_4 + Deterioro_y_amortizaciones_4 + Impuestos_4
Capital_Variable_4 <- 3265179000

Plusvalia_4 <- Produccion_4 - Capital_Constante_4 - Capital_Variable_4 - Impuestos_4
Tasa_Plusvalia_4 <- Plusvalia_4/Capital_Variable_4


Tasa_plusvalia <- c(Tasa_Plusvalia1,Tasa_Plusvalia_2,Tasa_Plusvalia_3,
                      0.89,Tasa_Plusvalia)

lim_sup <- mean(abs(Tasa_plusvalia*100))+1.96*sd(abs(Tasa_plusvalia*100))/sqrt(5)