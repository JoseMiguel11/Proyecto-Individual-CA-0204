
# Carga de las librerías 
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

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

# Unir ambos shp
mammals.unido <- rbind(mammals.1, mammals.2)

# Valores unicos de las categorías de vulnerabilidad
unique(mammals.1$category)


# Transformar los datos a tipo data frame
datos.1 <- as.data.frame(mammals.1) |> 
  select(-geometry)
datos.2 <- as.data.frame(mammals.2) |> 
  select(-geometry)

# Unir ambos data frames en uno  
datos <- rbind(datos.1, datos.2)

# Data frame con la observación más reciente
datos.unicos <- datos |> 
  arrange(desc(yrcompiled)) |>  # Ordenar por año más reciente
  distinct(sci_name, .keep_all = TRUE) # Seleccionar por cada nombre científico, solo la primer observación (Como está ordenado a más reciente entonces toma la observación más reciente)


# Verificar los nombres de las variables y sus tipos de datos
str(datos)

# Mostrar las primeras 100 filas de los datos
tabla.filtrada <- datos |> 
  head(100)

# Seleccionarlas columnas que serán utilizadas
datos.filtrados <- datos|> 
  head(1000) |> 
  select(sci_name, category, kingdom, marine, terrestria, freshwater, origin, seasonal)


orden <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE")
# Distribución de especies según su riesgo
grafico.1 <- datos.unicos |>  
  count(category) |> 
  mutate(category = factor(category, levels = orden)) |> 
  ggplot(aes(x = category, y = n, fill = category )) +
  geom_col() +
  scale_fill_manual(values = c("EX" = "#180501", "EW" = "#440E03", "CR" = "#701705", "EN" = "#9C2007", 
                               "VU" = "#C82909", "NT" = "#F4320B", "LC" = "#F87C63", "DD" = "grey", "NE" = "white"),
                    labels = c("EX" = "Extinto",
                               "EW" = "Extinto en Estado Silvestre",
                               "CR" = "En Peligro Crítico",
                               "EN" = "En Peligro",
                               "VU" = "Vulnerable",
                               "NT" = "Casi Amenazado",
                               "LC" = "Preocupación Menor",
                               "DD" = "Datos Insuficientes",
                               "NE" = "No Evaluado")) +
  scale_y_continuous(breaks = seq(0,3500,500)) +
  geom_text(aes(label = n), vjust = -0.45) +
  labs(title = "Cantidad de especies según su categoría", 
       x = "Categoría",
       y = "Cantidad",
       fill = "Categoría",
       caption = "Fuente: IUCN Red List of Threatened Species") +
  theme_minimal()

ggsave("res/cantidad.especies.categoria.pdf", plot = grafico.1, width = 6, height = 3)
ggsave("res/cantidad.especies.categoria.png", plot = grafico.1, width = 6, height = 3)

# Categorizar las especies por su hábitat
datos.hábitat <- datos.unicos |> 
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

grafico.2 <- datos.hábitat |> 
ggplot(aes(x = reorder(habitat, porcentaje), y = porcentaje, fill = habitat)) +
  geom_col() +
  geom_text(aes(label = paste0(round(porcentaje, 2), "%")), vjust = -0.3, 
            hjust = ifelse(datos.hábitat$habitat == "Terrestre", 1, -0.3), size = 3) +
  labs(title = "Distribución de las especies por tipo de hábitat",
       x = "Tipo de hábitat", 
       y = "Porcentaje",
       caption = "Fuente: IUCN Red List of Threatened Species") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0)) +
  coord_flip()
ggsave("res/distribucion.habitat.pdf", plot = grafico.2, width = 6, height = 3)
ggsave("res/distribucion.habitat.png", plot = grafico.2, width = 6, height = 3)



# Gráfico sobre la Distribución de vulnerabilidad global de las especies estudiadas para la base de datos 1
grafico.3 <- mammals.1 |> 
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
  theme_void() # Borrar las coordenadas, fondos, nombres de los ejes, etc

ggsave("res/distribucion.mundial.categorías.1.pdf", plot = grafico.3, width = 8, height = 3)
ggsave("res/distribucion.mundial.categorías.1.png", plot = grafico.3, width = 8, height = 3)


mammals.ordenado <- mammals.unido |> 
  arrange(yrcompiled)
  
# Gráfico sobre la distribución global del estado de las especies investigadas, ordenado de más antiguo a más reciente
grafico.4 <- mammals.ordenado |> 
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
                               "NE" = "No Evaluado")) + 
  labs(title = "Distribución Global de Especies Amenazadas") +
  theme_void()

ggsave("res/distribucion.mundial.categorías.pdf", plot = grafico.4, width = 8, height = 3)
ggsave("res/distribucion.mundial.categorías.png", plot = grafico.4, width = 8, height = 3)


#Crear el mapamundi
mapa.mundi <- ne_countries(scale = "medium", returnclass = "sf")

# Gráfico sobre la Distribución de especies en estado crítico
grafico.5 <- mammals.unido |> 
  filter(category == "CR") |> 
  ggplot() +
  geom_sf(data = mapa.mundi, fill = "white", color = "gray", size = 0.2) +
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
                               "NE" = "No Evaluado")) +  
  labs(title = "Distribución Global de Especies en Categoría Crítica",
       caption = "Fuente: IUCN Red List of Threatened Species",
       fill = "Categoría") +
  theme_void() 

ggsave("res/distribucion.mundial.críticas.pdf", plot = grafico.5, width = 8, height = 3)
ggsave("res/distribucion.mundial.críticas.png", plot = grafico.5, width = 8, height = 3)


# Códigos de vulnerabilidad
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


frecuencia.amenaza.por.categoria <- datos.unicos |> 
  filter(category %in% c("CR", "EN", "VU")) |> 
  count(order_, category) |>  # Contar cuántos ordenes hay en cada categoría
  group_by(order_) |> 
  mutate(total.amenazadas = sum(n)) |> # Cuenta el total de amenazas por cada especie 
  filter(n() == 3) |> # Seleccionar solamente los que tienen las 3 categorías
  ungroup() |> 
  arrange(desc(total.amenazadas))  |> 
  head(36) # 12 órdenes taxonómicos (Se pone 36 como parámetro pues hay 3 filas por cada orden)
# Solo se escogieron 12 órdenes taxonómicos pues solo hay 12 grupos con las 3 categorías


grafico.6 <- frecuencia.amenaza.por.categoria |> 
  ggplot(aes(x = reorder(order_, total.amenazadas), y = n, fill = category)) +
  geom_col() +
  scale_fill_manual(values = c("CR" = "#FF6B6B", "EN" = "#FFA726", "VU" = "#FFD93D"),
                    labels = c("CR" = "En peligro Crítico",
                               "EN" = "En Peligro",
                               "VU" = "Vulnerable")) +
  scale_y_continuous(breaks = seq(0,600,100)) +
  scale_x_discrete(labels = c("PRIMATES" = "Primates",
                              "RODENTIA" = "Roedores",
                              "CHIROPTERA" = "Quirópteros",
                              "ARTIODACTYLA" = "Artiodáctilos",
                              "EULIPOTYPHLA" = "Eulipotiflos",
                              "CARNIVORA" = "Carnívoros",
                              "DIPROTODONTIA" = "Dipodrontos",
                              "LAGOMORPHA" = "Lagomorfos",
                              "AFROSORICIDA" = "Afrosorícidos",
                              "PERISSODACTYLA" = "Perisodáctilos",
                              "PHOLIDOTA" = "Folidotos",
                              "PILOSA" = "Pilosos")) +
  coord_flip() +
  labs(title = "Órdenes Taxonómicos con mayor número de especies amenazadas",
       x = "Orden taxonómico", 
       y = "Número de especies", 
       fill = "Categoría",
       caption = "Fuente: IUCN Red List of Threatened Species") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("res/ordenes.taxonomico.pdf", plot = grafico.6, width = 6, height = 3)
ggsave("res/ordenes.taxonomico.png", plot = grafico.6, width = 6, height = 3)
