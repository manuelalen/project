library("writexl")
n = 100
datalist = list()
# or pre-allocate for slightly more efficiency
datalist = vector("list", length = n)

for (i in 1:n) {
  # ... make some data
  dat <- data.frame(Trabajador <- paste0("Trabajador_",i))
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}

df_cooperativa <- do.call(rbind, datalist)

df_cooperativa$planificacion_semanal <- rep(25,100)
df_cooperativa$producido_semanal <- sample(19:32,100,replace=TRUE)

deciles_cooperativas <-
  data.frame(quantile(df_cooperativa$producido_semanal, probs = seq(0,1,1/10)))    # Deciles

as.data.frame(quantile(df_cooperativa$producido_semanal, probs = seq(0,1,1/10)))
colnames(deciles_cooperativas) <- c("Valor")

df_deciles <- data.frame(deciles_cooperativas$Valor)

df_deciles$Decil <- seq(0,100, by = 10)
df_deciles$Creditos <- c(rep(0,4),10,20,40,60,80,90,100)

colnames(df_deciles) <- c("Valor","Decil","Creditos")

colnames(df_cooperativa) <- c("Trabajador","ID","Planificación Semanal"," Producido Semanal")

write_xlsx(df_cooperativa,"df.xlsx")