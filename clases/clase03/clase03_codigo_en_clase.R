# =============================================================================
# Clase 03 — Código en vivo
# Ciencia de Datos para Economía y Negocios | FCE-UBA
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------

# Cargar tidyverse.
# Definir instub y outstub con la ruta del repositorio.
# Construir la ruta al CSV con file.path() y leerlo con read_csv().
# Guardar en un objeto llamado "establecimientos".
# corregir el nombre de las variables 

# --- 1. Exploración inicial --------------------------------------------------

# Usar glimpse() para ver la estructura.
# cuántas filas, cuántas columnas, qué tipo tiene cada una.
# Ver las primeras fillas, las ultimas y los nombres de las columnas


# --- 2. select() -------------------------------------------------------------

# Seleccionar: anio, provincia, letra, Empleo, Establecimientos.
# Guardar en est_trabajo.

# Excluir columnas clae6 y clae2

# starts_with():# Despues vamos a profundizar sobre esto en unas clases
# Todos los verbos del tidyverse tienen modificadores 

# --- 3. filter() -------------------------------------------------------------

# Filtrar solamente las filas de CABA en 2022.

# Filtrar las filas donde la letra sea "C" (Industria manufacturera)
# y el empleo sea mayor a 100.

# Filtrar varias provincias con %in%: Buenos Aires, Cordoba y Santa Fe.


# --- 5. mutate() -------------------------------------------------------------

# Sobre df, crear una columna "emp_por_estab" que calcule
# Empleo / Establecimientos. Redondear a 1 decimal.

# Crear una columna "gran_empleador" con ifelse():
# si Empleo > 500, poner "Sí", sino "No".


# --- 6. arrange() ------------------------------------------------------------

# Filtrar año 2022 y letra "C" (industria), luego ordenar por Empleo descendente.

# Ordenar por provincia (ascendente) y dentro de cada provincia
# por Empleo (descendente)


# --- 7. summarise() + group_by() --------------------------------------------

# Calcular el empleo total nacional para 2022.

# Ahora calcular empleo total por provincia para 2022.
# Ordenar de mayor a menor. ¿Cuáles son las 5 provincias con más empleo?

# Calcular por provincia: empleo total, cantidad de establecimientos
# y el promedio de empleados por establecimiento.

# Calcular el share de empleo por provincia sobre el total nacional

# --- 8. Cadena completa con pipe ---------------------------------------------

# Armar una cadena que en un solo bloque:
#   1. Filtre año 2022
#   2. Agrupe por provincia
#   3. Calcule empleo total y establecimientos totales
#   4. Cree emp_por_estab con mutate
#   5. Ordene de mayor a menor por empleo


# --- 9. Guardar resultados ---------------------------------------------------

# Mostrar dir.create() con recursive = TRUE para crear output/clase03.
# La sintaxis no es obvia, así que va resuelta:
dir.create(file.path(outstub, "clase03"), recursive = TRUE, showWarnings = FALSE)

# Guardar el resultado de la cadena anterior con write_csv().
# Construir la ruta con file.path().
