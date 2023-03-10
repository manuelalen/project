#install.packages("metafor")
#install.packages("esc")
library(metafor)
library(esc)
media1 <- 20946
media2 <- 8528
sd1 <- 432.7647
sd2 <- 537.60
n1 <- 11
n2 <- 11
analisis<- escalc(measure='SMD', 
                  m1i = media1,
                  m2i = media2,
                  sd1i = sd1,
                  sd2i = sd2,
                  n1i = n1,
                  n2i = n2)
Obtencion_D<-analisis$yi
Obtencion_V<-analisis$vi
r_analisis<-rma(yi=Obtencion_D, vi=Obtencion_V,
                method = "DL",
                weighted = TRUE,
                level = 95,
                digits = 3)
paste0("El tamaño del efecto es: ",round(r_analisis$beta[1,1],digits = 4),
       " Con un pvalor de: ",round(r_analisis$pval,digits = 5))


#install.packages("pwr")
#library(pwr)
#pwr.t.test(power = 0.90, d = 0.90, sig.level = 0.05)
