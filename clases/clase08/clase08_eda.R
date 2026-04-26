# ============================================================
# Intro a EDA de nycflights13
# Curso: Ciencia de Datos para Economía y Negocios - FCE-UBA
# ============================================================

# ---- 1. Cargar paquetes ----

# Estos no los tenia instalados, asi que primero los instalo y luego cargo las librerias
#install.packages('naniar')
#install.packages('visdat')

library(nycflights13)   # Base de datos
library(tidyverse)      # Manipulación de datos
library(janitor)        # Limpieza y tabulaciones
library(skimr)          # Resúmenes detallados
library(DataExplorer)   # EDA automatizado con reportes
library(summarytools)   # dfSummary, freq, descr
library(naniar)         # Análisis de valores faltantes
library(visdat)         # Visualización automática de estructura
library(GGally)         # Matrices de correlación

# ---- 2. Conocer las tablas disponibles ----
# nycflights13 contiene 5 tablas relacionadas
data(package = "nycflights13")

# Tablas principales:
# - flights:  vuelos desde NYC en 2013
# - airlines: nombres de aerolíneas
# - airports: metadata de aeropuertos
# - planes:   metadata de aviones
# - weather:  clima por hora en origen

# ---- 3. Inspección inicial de cada tabla ----
glimpse(flights)
glimpse(airlines)
glimpse(airports)
glimpse(planes)
glimpse(weather)

# Dimensiones
lista_df <- list(flights = flights, airlines = airlines, airports = airports,
                 planes = planes, weather = weather)
map(lista_df, dim)

# Tipos de variables
map(list(flights, airlines, airports, planes, weather), 
    ~ introduce(.x)) 
# esta funcion (map) le aplica a una serie de dataframes la misma funcion
# (introduce del paquete DataExplorer)

# Veamoslo con uno solo y abramoslo como ventana para no tener que verlo en la consola
View(introduce(flights))

# ============================================================
# 4. EDA de la tabla principal: flights
# ============================================================

# OJO ESTAS FUNCIONES AUTOMATICAS SIRVEN CUANDO EL CSV NO ES GIGANTESCO
# PARA CASOS COMO EL DE COMERCIO INTERNACIONAL (QUE VAMOS A VER PRONTO)
# U OTRAS BASES CON MUCHISIMAS OBSERVACIONES, CONVIENE HACERLO POR NUESTRA CUENTA
# PERO PODEMOS SEGUIR LA IDEA DETRAS DE ESTAS FUNCIONES PARA CALCULAR LAS PRIMERAS
# ESTADISTICAS DESCRIPTIVAS

# ---- 4.1 Resumen general ----
skim(flights)

# Resumen alternativo con summarytools
dfSummary(flights, graph.col = FALSE)

# Descriptivos de numéricas
descr(flights, stats = "common")

# ---- 4.2 Diagnóstico automatizado con dlookr ----
library(dlookr)
diagnose(flights)              # tipos, NAs, únicos
diagnose_numeric(flights)      # mín, Q1, mediana, media, Q3, máx, outliers
diagnose_category(flights)     # niveles, frecuencias
diagnose_outlier(flights)      # cantidad y proporción de outliers

# ---- 4.3 Valores faltantes ---- (Esto lo vamos a ver mas adelante en el curso con mas detalle)
miss_var_summary(flights)         # tabla de NAs por variable
miss_case_summary(flights)        # NAs por fila
gg_miss_var(flights)              # gráfico automático
gg_miss_upset(flights)            # patrones de faltantes combinados
vis_miss(flights, warn_large_data = FALSE)
vis_dat(flights, warn_large_data = FALSE)

# ---- 4.4 Variables categóricas ----
flights %>% tabyl(carrier) %>% adorn_pct_formatting()
flights %>% tabyl(origin)
flights %>% tabyl(dest) %>% arrange(desc(n))
flights %>% tabyl(origin, carrier) %>% 
  adorn_totals(where = c("row", "col"))

# Frecuencias con summarytools
freq(flights$carrier)
freq(flights$origin)

# ---- 4.5 Variables numéricas: distribuciones ----
# DataExplorer: histogramas y densidades automáticas
plot_histogram(flights)
plot_density(flights)
plot_bar(flights)            # barras para categóricas
plot_qq(flights)             # QQ-plots de normalidad

# Boxplots automáticos por variable categórica
plot_boxplot(flights, by = "origin")
plot_boxplot(flights, by = "carrier")

# ---- 4.6 Correlaciones ----
plot_correlation(flights, type = "continuous")

# ============================================================
# 5. EDA de tablas secundarias
# ============================================================

# ---- 5.1 airlines ----
skim(airlines)

# ---- 5.2 airports ----
skim(airports)
diagnose(airports)
plot_histogram(airports)

# ---- 5.3 planes ----
skim(planes)
diagnose(planes)
plot_bar(planes)
plot_histogram(planes)
freq(planes$manufacturer)
freq(planes$engine)

# ---- 5.4 weather ----
skim(weather)
diagnose(weather)
diagnose_outlier(weather)
plot_histogram(weather)
plot_correlation(weather %>% select(where(is.numeric)), 
                 type = "continuous")

# ============================================================
# 6. EDA relacional: flights enriquecido con joins
# ============================================================

flights_full <- flights %>%
  left_join(airlines, by = "carrier") %>%
  left_join(planes, by = "tailnum", suffix = c("_flight", "_plane")) %>%
  left_join(weather, by = c("origin", 'year_flight'="year", "month", "day", "hour")) %>%
  left_join(airports, by = c("dest" = "faa"))

glimpse(flights_full)
introduce(flights_full)

# Resumen sobre la tabla unida
skim(flights_full)
diagnose(flights_full)

# ============================================================
# 7. Análisis focalizado en la variable objetivo: arr_delay
# ============================================================

# ---- 7.1 Distribución del retraso ----
descr(flights$arr_delay)
diagnose_outlier(flights %>% select(arr_delay))

# ---- 7.2 Relación con categóricas ----
plot_boxplot(flights, by = "carrier")
plot_boxplot(flights, by = "origin")

# ---- 7.3 Correlación con numéricas ----
flights %>%
  select(where(is.numeric)) %>%
  correlate(target = arr_delay) %>%  # de dlookr
  arrange(desc(abs(coef_corr)))