
# Carga de las librerías 
library(sf)
library(dplyr)
library(ggplot2)

# Link de la carpeta en dropbox
url <- "https://www.dropbox.com/scl/fi/z1npogjqf3q6lohm4g3f3/MAMMALS.zip?rlkey=zzia7ftshjnzwef7ovfqiytfn&st=beetnd2f&dl=1"

# Crear carpeta data en caso de que no exista, si ya existe no pasará nada, solo que no se creará
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Descargar el archivo.zip, si ya existe en la ubicación entonces no lo descargará
if (!file.exists("data/MAMMALS.zip")) {
  options(timeout = 600)  # Tiempo máximo para la descarga, puse 600 segundos, 10 minutos
  download.file(url, "data/MAMMALS.zip", mode = "wb")
} 

# Descomprimir la carpeta MAMMALS en data
unzip("data/MAMMALS.zip", exdir = "data")

# Buscar los archivos shapefile, con extención .shp y los guardamos en archivos.shp
archivos.shp <- list.files("data", pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

# Leer los shapefiles
mammals.1 <- st_read(archivos.shp[1]) # Primera parte de los datos
mammals.2 <- st_read(archivos.shp[2]) # Segunda parte de los datos

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
  head(1000) |> 
  select(sci_name, category, kingdom, marine, terrestria, freshwater, origin, seasonal)


# Distribución de especies según su riesgo
datos |>  
  count(category, sort = T) |> 
  ggplot(aes(x = reorder(category, n), y = n, fill = category )) +
  geom_col() +
  labs(title = "Cantidad de especies según su categoría", 
       y = "Cantidad")



# Categorizar las especies por su hábitat
datos.hábitat <- datos |> 
  mutate(habitat = case_when(
    terrestria == "true" & marine != "true" & freshwater != "true" ~ "Terrestre", 
    terrestria != "true" & marine == "true" & freshwater != "true" ~ "Marino", 
    terrestria != "true" & marine != "true" & freshwater == "true" ~ "Agua Dulce", 
    terrestria == "true" & marine == "true" & freshwater != "true" ~ "Terrestre y Marino", 
    terrestria == "true" & marine != "true" & freshwater == "true" ~ "Terrestre y Agua Dulce", 
    terrestria != "true" & marine == "true" & freshwater == "true" ~ "Marino y Agua Dulce",
    TRUE ~ "Otros"
    )) |>
  count(habitat) |> 
  mutate(porcentaje = (n/sum(n)* 100)) |> 
  group_by(habitat)

datos.hábitat |> 
ggplot(aes(x = reorder(habitat, porcentaje), y = porcentaje, fill = habitat)) +
  geom_col() +
  geom_text(aes(label = paste0(round(porcentaje, 2), "%")), vjust = -0.3, 
            hjust = ifelse(datos.hábitat$habitat == "Terrestre", 1, -0.3), size = 3) +
  labs(title = "Distribución de las especies por tipo de hábitat",
       x = "Tipo de hábitat", 
       y = "Porcentaje",
       caption = "Fuente: IUCN Red List of Threatened Species") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()


# Gráfico sobre la Distribución de vulnerabilidad global de las especies estudiadas
mammals.1 |> 
  ggplot() +
  geom_sf(aes(fill = category), alpha = 0.7, size = 0.1) +
  scale_fill_manual(values = c("EX" = "black", "EW" = "#23DBDB", "CR" = "red", "EN" = "orange", 
                               "VU" = "yellow", "NT" = "lightblue", "LC" = "green", "DD" = "grey", "NE" = "white"), # Poner los colores manualmente
                    labels = c("EX" = "Extinto",
                               "EW" = "Extinto en Estado Silvestre",
                               "CR" = "En Peligro Crítico",
                               "EN" = "En Peligro",
                               "VU" = "Vulnerable",
                               "NT" = "Casi Amenazado",
                               "LC" = "Preocupación Menor",
                               "DD" = "Datos Insuficientes",
                               "NE" = "No Evaluado")) +  # Cambiar abreviación por palabras en el código de colores
  labs(title = "Distribución Global de Especies Amenazadas") +
  theme_void() # Borrar las coodenadas, fondos, nombres de los ejes, etc

# Códigos devulnerabilidad
# EX: Extinto
# EW: Extinto en estado silvestre
# CR: En peligro crítico
# EN: En peligro
# VU: Vulnerable
# NT: Casi amenazado
# LC: Preocupación menor
# DD: Datos insuficientes
# NE: No evaluado


# El size dentro del geom_sf() es para el grosor de las líneas de los polígonos
# El alpha es para la opacidad de los colores en el gráfico
# La columna geometry contiene coordenadas globales en donde se realizaron las observaciones, con esas coordendas se crean los polígonos

# Unir ambos shp
mammals.unido <- rbind(mammals.1, mammals.2)

# Valores unicos de las categorías de vulnerabilidad
unique(mammals.1$category)
