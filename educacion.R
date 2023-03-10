library(skimr)
library(tidyverse)
library(ggplot2)
library(plotly)
df_alumnoX <- data.frame(
  tarea1 <- c(sample(0:1,4, replace=TRUE)),
  tarea2 <- c(sample(0:1,4, replace=TRUE)),
  tarea3 <- c(sample(0:1,4, replace=TRUE)),
  tarea4 <- c(sample(0:1,4, replace=TRUE)),
  examenes <- c(sample(0:1,4, replace=TRUE))
)

colnames(df_alumnoX) <- c("tarea1","tarea2","tarea3","tarea4","examenes")


creditos_alumnoX8 <- 
  ((10*sum(df_alumnoX$tarea1))+(20*sum(df_alumnoX$tarea2))+(30*sum(df_alumnoX$tarea3))+
  (40*sum(df_alumnoX$tarea4))+(100*sum(df_alumnoX$examenes)))-220

creditos_maximos <- (10*4)+(20*4)+(30*4)+(40*4)+(100*4)
  
df_evolución_alumnoX <- data.frame(
  tiempo<-c(1,2,3,4,5,6,7,8,9),
  creditos <- c(creditos_alumnoX,creditos_alumnoX1,creditos_alumnoX2,
                creditos_alumnoX3,creditos_alumnoX4,creditos_alumnoX5,
                creditos_alumnoX6,creditos_alumnoX7,creditos_alumnoX8),
  creditos_max <- c(rep(creditos_maximos-220,9))
)
colnames(df_evolución_alumnoX) <- c("tiempo","creditos","creditos_maximos")

ggplot(df_evolución_alumnoX,aes(x=tiempo))+
  geom_line(aes(y=creditos,color="red"))+
  geom_line(aes(y=creditos_maximos))+
  theme_light()+
  theme_minimal()


## From JSON
install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

res = GET("https://api.mockaroo.com/api/d2121dc0?count=1000&key=339bbf30")
rawToChar(res$content)
data <- fromJSON(readLines(rawToChar(res$content)), warn = F)
data

## From CSV
df_sisedu <- as.data.frame(read.csv(file = 'MOCK_DATA.csv')) 

mean(df_sisedu$Creditos_Mes)
median(df_sisedu$Creditos_Mes)

df_aprendizaje_genero <- data.frame(
  Genero <- c("Hombre","Mujer"),
  Media <- c(mean(df_sisedu$Creditos_Mes[df_sisedu$gender == "Male"]),
             mean(df_sisedu$Creditos_Mes[df_sisedu$gender == "Female"]))
)

colnames(df_aprendizaje_genero) <- c("Genero","Media")

ggplot(df_aprendizaje_genero, aes(x=Genero,y=Media, fill = Genero))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme_light()+
  theme(text = element_text(family = "Tahoma"),
        panel.background = element_rect(color = "white", # Border color
                                        size = 1, fill = "#FFFFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 25),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot" ,
        panel.border = element_blank(),
        axis.text = element_text(size=7),
        legend.position = "none",
        axis.line = element_line(colour = "black"))


mean_sisedu_m <- mean(df_sisedu$Creditos_Mes[df_sisedu$gender == "Male"])

mean_sisedu_f <- mean(df_sisedu$Creditos_Mes[df_sisedu$gender == "Female"])


sd_sisedu_m <- sd(df_sisedu$Creditos_Mes[df_sisedu$gender == "Male"])

sd_sisedu_f <- sd(df_sisedu$Creditos_Mes[df_sisedu$gender == "Female"])

length_sisedu_m <- length(df_sisedu$Creditos_Mes[df_sisedu$gender == "Male"])

length_sisedu_f <- length(df_sisedu$Creditos_Mes[df_sisedu$gender == "Female"])


 df_sisedu %>%
  select(gender) %>%
  filter(df_sisedu$Creditos_Mes == max(df_sisedu$Creditos_Mes))
 
length(df_sisedu$Creditos_Mes[df_sisedu$Creditos_Mes == max(df_sisedu$Creditos_Mes)])
 
 
df_evo_sisedu <- as.data.frame(read.csv(file = 'evo_educacion.csv'))

df_evo_sisedu_mean <- data.frame(
  Mes <- c("Mes 1","Mes 2","Mes 3","Mes 4","Mes 5","Mes 6","Mes 7","Mes 8","Mes 9","Mes 9_1"),
  Creditos <- c(mean(df_evo_sisedu$Creditos_septiembre),
                mean(df_evo_sisedu$Creditos_octubre),
                mean(df_evo_sisedu$Creditos_noviembre),
                mean(df_evo_sisedu$Creditos_diciembre),
                mean(df_evo_sisedu$Creditos_enero),
                mean(df_evo_sisedu$Creditos_febrero),
                mean(df_evo_sisedu$Creditos_marzo),
                mean(df_evo_sisedu$Creditos_abril),
                mean(df_evo_sisedu$Creditos_mayo),
                mean(df_evo_sisedu$Creditos_junio)
                )
)

colnames(df_evo_sisedu_mean) <- c("Mes","Creditos")

aprendizaje <- ggplot(df_evo_sisedu_mean,aes(x=Mes,group=1))+
  geom_line(aes(y=Creditos,color="steelblue"))+
  theme_minimal()+
  theme_light()+
  theme(text = element_text(family = "Tahoma"),
        panel.background = element_rect(color = "white", # Border color
                                        size = 1, fill = "#FFFFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold",hjust = 0.5,size = 25),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot" ,
        panel.border = element_blank(),
        axis.text = element_text(size=7),
        legend.position = "none",
        axis.line = element_line(colour = "black"))+
  labs(title = "Evolución de la media de créditos estudiantiles por mes académico",
       caption = "Author: Manuel Alén Sánchez",
       x = "Créditos", y="Mes")

api_aprender <- ggplotly(aprendizaje,tooltip=c("x","y"))
df_analisis_aprendizaje_edu <- data.frame(
  antes <- c(mean(df_evo_sisedu$Creditos_septiembre),
             mean(df_evo_sisedu$Creditos_octubre),
             mean(df_evo_sisedu$Creditos_noviembre)),
  ahora <- c(mean(df_evo_sisedu$Creditos_abril),
             mean(df_evo_sisedu$Creditos_mayo),
             mean(df_evo_sisedu$Creditos_junio))
)



colnames(df_analisis_aprendizaje_edu) <- c("antes","ahora")

mean(df_analisis_aprendizaje_edu$antes)