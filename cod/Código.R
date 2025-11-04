
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


