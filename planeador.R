## Creams nuestro dataframe
library(tidyverse)
library(DT)
df_input_output <- data.frame(
  product <- c(paste0("Product_",1:5)),
  product_1 <- c(runif(0.5:5, min=0.5,max=65)),
  product_2 <- c(runif(0.5:5, min=0.5,max=33)),
  product_3 <- c(runif(0.5:5, min=0.5,max=76)),
  product_4 <- c(runif(0.5:5, min=0.5,max=83)),
  product_5 <- c(runif(0.5:5, min=0.5,max=113))
)

datatable(df_demanda)
## Le asignmos nombre a las columnas
colnames(df_input_output) <- c("product","product_1","product_2","product_3","product_4","product_5")

##Comprobamos
df_input_output

#Miramos el número de rows
nrow(df_input_output)


#Creamos nuestra matrix identidad en función del tamaño de nuestra input-output table
I <- diag(nrow(df_input_output))
datatable(I)
## Tabla IO con solamente los datos.
df_tabla_IO <- 
  df_input_output %>%
  select(product_1:product_5)

#I-A
I - df_tabla_IO

#(I-A)^(-1)
solve(I - df_tabla_IO)

## Demanda
df_demanda <- data.frame(
  product <- c(paste0("Product_",1:5)),
  cantidad <- c(runif(5, min=100000,max=450000))
)

colnames(df_demanda) <- c("producto","cantidad")


df_M_demanda[2,] <- 
  df_demanda %>%
  select(cantidad)

#Resultado final del Cálculo.
datatable(format(solve(I - df_tabla_IO) %% df_M_demanda,scientific = F, digits = 4))

df_tabla_IO[5,]/sum(df_tabla_IO$product_1)