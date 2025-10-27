
# Carga de las librerías 
library(sf)
library(dplyr)


# Importar las bases de datos
mammals.1 <- st_read("C:/Users/mr214/Documents/Herramientas de ciencias de Datos I/Proyecto Individual/data/MAMMALS/MAMMALS_PART1.shp")
mammals.2 <- st_read("C:/Users/mr214/Documents/Herramientas de ciencias de Datos I/Proyecto Individual/data/MAMMALS/MAMMALS_PART2.shp")


# Transformar los datos a tipo data frame
datos.1 <- as.data.frame(mammals.1) |> 
  select(-geometry)
datos.2 <- as.data.frame(mammals.2) |> 
  select(-geometry)

# Unir ambos data frames en uno  
datos <- rbind(datos.1, datos.2)

# Verificar los nombres de las variables y sus tipos de datos
str(datos)

# Mostrar las primeras 100 filas de los datos
tabla.filtrada <- datos |> 
  head(100)

# Seleccionarlas columnas que serán utilizadas
datos.filtrados <- datos|> 
  head(200) |> 
  select(sci_name, category, kingdom, marine, terrestria, freshwater, origin, seasonal)


