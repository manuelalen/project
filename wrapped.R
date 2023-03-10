# LIBRERIAS -------------------------
library("readxl")
library("data.table")
library("ggradar")

#install.packages("devtools")
#devtools::install_github("ricardo-bion/ggradar")
# DATA ------------------------
wrapped <- read_excel("DATA/Wrapped.xlsx")

## Creamos el nick del usuario --------
wrapped$nick <- paste0(wrapped$first_name,"_",wrapped$last_name)

## Generados los deciles que vamos a necesitar ------
quantile(wrapped_badbonis$minutos_escuchados, probs = seq(0,1,1/10))    # Deciles


## Filtramos para conocer las escuchas a Bad Bunny ------
wrapped_badbonis <- wrapped %>%
  filter(artista == "Bad Bunny")

## Agrupamos por usuario y seleccionamos los minutos escuchados -------------
wrapped_porUser_badbonis <- wrapped_badbonis %>%
  group_by(wrapped_badbonis$nick) %>%
  select(minutos_escuchados)
  
## Crear datatable ------
DAT <- data.table(wrapped_porUser_badbonis)
setnames(DAT, "Top_fan", "Top Fan")
setnames(DAT, "wrapped_badbonis$nick", "Username")
setnames(DAT, "minutos_escuchados", "Minutos Escuchados")
datatable(DAT)


## Calculamos por usuario cómo de fans son ---------
wrapped_porUser_badbonis$Top_fan <-
100-((100*wrapped_porUser_badbonis$minutos_escuchados)/max(wrapped_porUser_badbonis$minutos_escuchados))

## Mostramos el historigrama para saber la distribución ---------
hist(wrapped_porUser_badbonis$minutos_escuchados)


## Genearmos un dataframe de un artista indie ----------
df_indie <- data.frame(
  minutos <- c(rep(100:400,30),
               rep(401:1000,19),
               rep(1001:3000,15),
               rep(3001:5000,5),
               rep(5001:6500,3))
)

colnames(df_indie) <- c("minutos")


## Historigrama de artista indie ---------
hist(df_indie$minutos)




df_rank <- data.frame(matrix(runif(30), ncol = 10))
df_rank[, 1] <- paste0("Persona_", 1:3)
colnames(df_rank) <- c("Habilidad", paste("Habilidad_", 1:9))



lcols <- c("#EEA236", "#5CB85C", "#46B8DA")

ggradar(df_rank,
        background.circle.colour = "white",
        gridline.min.linetype = 1,
        gridline.mid.linetype = 1,
        gridline.max.linetype = 1,
        group.colours = lcols)