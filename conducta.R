## Sanción: Reparar el daño y evitar o disminuir la consecuencia de lo hecho.
## Acción negativa sobre quien lo ha realizado.


##De inmediato: Debe aplicarse con el menor retardo posible después de hecha la conducta, de no ser así, no quedará claro el motivo por el que se tiene que hacer la sanción.

##Con conocimiento:La persona debe de saber porqué se le está sancionando para que pueda corregirse.

##Analizada: Se debe comprender la conducta que se ha de extinguir, los motivos por los que se repite o se dio y las ganancias que se obtienen con ella. Porque teniendo éstas cosas claras se puede idear el cambio.


##Educativa: Como toda técnica conductista, debe atender al análisis anterior y procurar el cambio por medio de nuevos motivos y ganancias.


##Debe resarcir el daño: Busca además, el reparar lo que se hizo mal.

##Con nuevas alternativas: Finalmente, procura evitar que la conducta se repita buscando maneras alternas para satisfacer el motivo y obtener las ganancias
library(ggplot2)

molestia <- 7
m_sr <- molestia + 8
d_sr <- 2
k <- 1
m_cr <- molestia+4
d_cr <- 0.5
vr_sr <- m_sr/(1+k*d_sr)
vr_cr <- m_cr/(1+k*d_cr)
min <- 0.85
max <- 0.99
Valor_Reforzador_No_Rebaja <- runif(10,min=(vr_sr*min),max=(vr_sr*max))
Valor_Reforzador_Con_Rebaja <- runif(10,min=(vr_cr*min),max=(vr_cr*max))

df_conducta <- data.frame()
df_conducta <- data.frame(tiempo,Valor_Reforzador_No_Rebaja,Valor_Reforzador_Con_Rebaja)
colnames(df_conducta) <- c("Tiempo","Valor_Reforzador_No_Rebaja","Valor_Reforzador_Con_Rebaja")


## demora
d_cr_ana <- runif(10, min=d_cr*min, max=d_cr*max)
d_sr_ana <- runif(10, min=d_sr*min, max=d_sr*max)

## magnitud
m_cr_ana <- runif(10, min=m_cr*min, max=m_cr*max)
m_sr_ana <- runif(10, min=m_sr*min, max=m_sr*max)

## d_cr con v_cr es de 29.499 | d_sr con v_sr es de 15.98
## m_cr con v_cr es de 9.77 | m_sr con v_sr es de 17.98
ggplot(df_conducta,aes(x=Tiempo,group=1))+
  geom_line(aes(y=Valor_Reforzador_No_Rebaja,color="blue"))+
  geom_line(aes(y=Valor_Reforzador_Con_Rebaja,color="red"))+
  theme_light()+
  theme_minimal()+
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
  labs(title="Valor del reforzador de la conducta criminal",
       subtitle = "Línea azul con rebaja de pena, Línea roja sin rebaja de pena",
       x="Tiempo",y="Valor del Reforzador")