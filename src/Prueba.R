########################################################################
# Análisis de Prestadores de Salud en Colombia - Versión Final
########################################################################

## 1. Carga de Librerías ----
library(tidyverse)    
library(sf)           
library(tmap)         
library(geodata)      
library(readxl)       
library(ggrepel)      
library(sqldf)
library(dplyr)
library(car)   
library(lmtest) 
library(sandwich) 
library(broom) 

## 2. Configuración Inicial ----
setwd("/Users/mac/Documents/Pruebas/IETS/src/")
tmap_mode("view")

## 3. Carga y Preparación de Datos ----
# 3.1 Datos tabulares (conservando nombres originales)
municipios <- read_excel("../input/Municipios.xlsx") 
prestadores <- read_excel("../input/Prestadores.xlsx")

# 3.2 Unión de datos manteniendo nombres originales
resultado <- sqldf(
  "SELECT m.*, r.*
   FROM municipios AS m
   LEFT JOIN prestadores AS r ON m.MPIO = r.Codigo"
) %>% 
  mutate(
    Superficie = as.numeric(Superficie),
    PopTot = as.numeric(PopTot),
    Rural = as.numeric(Rural)
  )

## 4. Análisis Descriptivo ----
# 4.1 Conteo de prestadores (nombres originales)
conteo_prestadores <- resultado %>%  
  group_by(MPIO, Municipio_Nombre_Ajustado) %>%  
  summarise(
    num_prestadores = n(),
    num_habilitados = sum(habilitado == 'SI', na.rm = TRUE),
    .groups = 'drop'
  )

# 4.2 Municipios sin prestadores
sin_prestadores <- resultado %>%  
  filter(is.na(nombre_prestador)) %>%  
  distinct(MPIO, Municipio_Nombre_Ajustado)

# 4.3 Estadísticas municipales
resumen_municipios <- resultado %>%  
  group_by(MPIO, Municipio_Nombre_Ajustado, Departamento) %>%  
  summarise(
    Superficie_media = mean(Superficie, na.rm = TRUE),
    Poblacion_total = mean(PopTot, na.rm = TRUE),
    Poblacion_rural = mean(Rural, na.rm = TRUE),
    .groups = 'drop'
  )


# 4.4 Dataset consolidado (sin modificar nombres)
resumen_completo <- resumen_municipios %>% 
  left_join(conteo_prestadores, by = c("MPIO", "Municipio_Nombre_Ajustado"))

# 4.4.1. Ajustando el nombre de Bogotá D.C. para que se realice correctamente el pegue más adelante

resumen_completo <- resumen_completo %>%  mutate(
  Departamento = ifelse(Departamento == "Bogotá, D.C.", "Bogotá D.C.", Departamento),
  Municipio_Nombre_Ajustado = ifelse(Municipio_Nombre_Ajustado == "Bogotá, D.C.", "Bogotá D.C.", Municipio_Nombre_Ajustado)
)

resumen1 <- resumen_completo %>%
  summarise(
    across(c(Superficie_media, Poblacion_total, Poblacion_rural),
           list(
             Media = ~mean(., na.rm = TRUE),
             Mediana = ~median(., na.rm = TRUE),
             SD = ~sd(., na.rm = TRUE),
             Mínimo = ~min(., na.rm = TRUE),
             Máximo = ~max(., na.rm = TRUE),
             Q1 = ~quantile(., 0.25, na.rm = TRUE),
             Q3 = ~quantile(., 0.75, na.rm = TRUE)
           )
    ))

# Histogramas
ggplot(resumen_completo, aes(x = Superficie_media)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribución de Superficie Municipal")

ggplot(resumen_completo, aes(x = Poblacion_total)) +
  geom_histogram(fill = "forestgreen", bins = 30) + 
  scale_x_log10() + # Para mejor visualización por distribución desigual
  labs(title = "Distribución de Población Municipal (escala logarítmica)")

ggplot(resumen_completo, aes(x = Poblacion_rural)) +
  geom_histogram(fill = "brown", bins = 30) +
  labs(title = "Distribución de % Población Rural")

# Relación Superficie-Población
ggplot(resumen_completo, aes(x = Superficie_media, y = Poblacion_total)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  labs(title = "Relación Superficie-Población")

# Relación Población-%Rural
ggplot(resumen_completo, aes(x = Poblacion_total, y = Poblacion_rural)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  scale_x_log10() +
  labs(title = "Relación Población Total vs % Rural")

# Matriz de correlación
cor_matrix <- resumen_completo %>%
  select(Superficie_media, Poblacion_total, Poblacion_rural) %>%
  cor(use = "complete.obs")

# Por quintiles de población
resumen_completo %>%
  mutate(Quintil_Poblacion = ntile(Poblacion_total, 5)) %>%
  group_by(Quintil_Poblacion) %>%
  summarise(
    Avg_Superficie = mean(Superficie_media, na.rm = TRUE),
    Avg_Ruralidad = mean(Poblacion_rural, na.rm = TRUE)
  )

## 5. Visualización de Datos ----
# 5.1 Top 30 municipios (nombres originales)
top_30 <- resumen_completo %>%
  slice_max(num_prestadores, n = 30)

ggplot(top_30, aes(x = reorder(Municipio_Nombre_Ajustado, -num_prestadores), 
                   y = num_prestadores)) +
  geom_col(fill = "#4682B4", alpha = 0.8) +
  geom_text(aes(label = num_prestadores), vjust = -0.5, size = 3) +
  labs(title = "Top 30 Municipios por Número de Prestadores",
       x = NULL, y = "Número de Prestadores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.2 Relación superficie-prestadores
ggplot(top_30, aes(x = Superficie_media, y = num_prestadores,
                   label = Municipio_Nombre_Ajustado)) +
  geom_point(color = "#8B0000", size = 3) +
  geom_smooth(method = "lm", color = "#1E90FF", se = FALSE) +
  geom_text_repel(size = 3, max.overlaps = 20) +
  labs(title = "Relación: Superficie vs Número de Prestadores",
       x = "Superficie Media (km²)", y = "Número de Prestadores") +
  theme_minimal()

## 6. Análisis Espacial ----
# 6.1 Descarga de shapefile manteniendo nombres originales
colombia_sf <- gadm(country = "COL", level = 2, path = tempdir()) %>% 
  st_as_sf() %>% 
  rename(
    Departamento = NAME_1,
    Municipio_Nombre_Ajustado = NAME_2
  )

# 6.2 Integración conservando nombres originales
map_data <- colombia_sf %>% 
  left_join(
    resumen_completo,
    by = c("Departamento", "Municipio_Nombre_Ajustado")
  ) %>% 
  mutate(
    num_prestadores = replace_na(num_prestadores, 0)
  )

# 6.3 Visualización interactiva

tm_shape(map_data) +
  tm_polygons(
    col = "num_prestadores",
    palette = "Blues",
    style = "fixed",
    breaks = c(0, 1, 2, 6, 16, 51, 201, 1001, 13761),  # Nuevos intervalos
    labels = c(
      "0 (sin prestadores)",
      "1 prestador", 
      "2-5 prestadores",
      "6-15 prestadores",
      "16-50 prestadores",
      "51-200 prestadores",
      "201-1,000 prestadores",
      "Más de 1,000 prestadores"
    ),
    title = "Prestadores por municipio",
    border.col = "gray40",
    border.alpha = 0.5,
    id = "Municipio_Nombre_Ajustado",  # Nombre corregido (sin .x)
    popup.vars = c(
      "Departamento" = "Departamento",
      "Prestadores" = "num_prestadores",
      "Población" = "Poblacion_total",
      "Superficie" = "Superficie_media"
    ),
    textNA = "Sin datos",  # Manejo explícito de valores NA
    colorNA = "lightgray"  # Color para valores faltantes
  ) +
  tm_layout(
    title = "Distribución de Prestadores de Salud en Colombia",
    title.size = 0.9,  # Aumenté ligeramente el tamaño
    legend.position = c("right", "bottom"),
    legend.bg.color = "white",  # Fondo para mejor legibilidad
    legend.frame = TRUE,
    frame = FALSE,
    inner.margins = c(0.02, 0.02, 0.02, 0.02)  # Márgenes internos ajustados
  ) +
  tm_view(
    set.view = c(-74, 4, 6),
    view.legend.position = c("right", "bottom"),
    legend.width = 0.3  # Ancho de la leyenda ajustado
  ) +
  tm_scale_bar(position = c("left", "bottom"))  # Añadí escala para referencia

## 7. Resultados Clave ----
list(
  top_municipios = top_30,
  municipios_sin_prestadores = sin_prestadores,
  resumen_estadistico = summary(select(resumen_completo, where(is.numeric)))
)

## 8. Modelo


# Filtrar y seleccionar las variables relevantes
modelo_data <- map_data %>%
  select(num_prestadores, Poblacion_total, Superficie_media, Poblacion_rural, Departamento) %>%
  filter(!is.na(num_prestadores), !is.na(Poblacion_total), !is.na(Superficie_media), !is.na(Poblacion_rural))

# Verificar estructura de los datos
str(modelo_data)
summary(modelo_data)

# Aplicar logaritmo (se suma 1 para evitar log(0))
modelo_data <- modelo_data %>%
  mutate(
    log_prestadores = log(num_prestadores + 1),
    log_poblacion = log(Poblacion_total + 1),
    log_superficie = log(Superficie_media + 1)
  )

# Modelo base con efectos fijos por Departamento
modelo <- lm(
  log_prestadores ~ log_poblacion + log_superficie + Poblacion_rural + factor(Departamento),
  data = modelo_data
)

# Resumen del modelo
summary(modelo)

# Gráfico de residuos
par(mfrow = c(2, 2))
plot(modelo)




