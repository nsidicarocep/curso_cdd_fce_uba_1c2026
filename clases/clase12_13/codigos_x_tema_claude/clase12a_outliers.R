# =============================================================================
# Ciencia de Datos para Economía y Negocios - FCE-UBA
# Clase 12a: Outliers - detección, diagnóstico y tratamiento
# -----------------------------------------------------------------------------
# Contenidos:
#   1. Exploración visual: boxplot e histograma
#   2. Detección por la regla del IQR (Tukey)
#   3. Detección por z-score
#   4. Outliers genuinos vs errores de carga
#   5. Decisión: eliminar, winsorizar o mantener
#   6. Justificación e impacto: comparar antes vs después
# -----------------------------------------------------------------------------
# Datos: indicadores del Banco Mundial (paquete {WDI}) para el año 2022.
#        Lo usamos porque tiene outliers genuinos (Luxemburgo en PIB pc,
#        Venezuela en inflación, etc.) que permiten discutir criterios.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Preparación del entorno
# -----------------------------------------------------------------------------

library(tidyverse)   # dplyr, ggplot2, tidyr, etc.
library(WDI)         # indicadores del Banco Mundial
library(scales)      # para formatear ejes

options(scipen = 999)   # evita notación científica en los prints
set.seed(42)            # reproducibilidad


# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================

indicadores <- c(
  pib_pc       = "NY.GDP.PCAP.CD",        # PIB per cápita (USD corrientes)
  poblacion    = "SP.POP.TOTL",           # Población total
  esperanza    = "SP.DYN.LE00.IN",        # Esperanza de vida al nacer
  inflacion    = "FP.CPI.TOTL.ZG",        # Inflación anual (% IPC)
  internet     = "IT.NET.USER.ZS"         # Usuarios de internet (% población)
)

wdi_raw <- WDI(
  country   = "all",
  indicator = indicadores,
  start     = 2022,
  end       = 2022,
  extra     = TRUE
)

paises <- wdi_raw |>
  filter(region != "Aggregates") |>
  select(pais = country, iso = iso3c, region, income,
         pib_pc, poblacion, esperanza, inflacion, internet)

glimpse(paises)


# =============================================================================
# 2. EXPLORACIÓN VISUAL: EL PRIMER PASO SIEMPRE
# =============================================================================
#
# Antes de aplicar cualquier regla automática conviene MIRAR los datos.
# Un boxplot y un histograma alcanzan para detectar visualmente la asimetría
# y los valores extremos.
# =============================================================================

# Boxplot del PIB per cápita: muestra la asimetría brutal y la cola larga.
paises |>
  filter(!is.na(pib_pc)) |>
  ggplot(aes(y = pib_pc)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, width = 0.4) +
  scale_y_continuous(labels = label_dollar()) +
  labs(y = "PIB per cápita (USD)",
       title = "Distribución del PIB per cápita - países, 2022") +
  theme_minimal(base_size = 12)

# Histograma: ayuda a ver la cola derecha.
paises |>
  filter(!is.na(pib_pc)) |>
  ggplot(aes(x = pib_pc)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "PIB per cápita (USD)", y = "Frecuencia") +
  theme_minimal(base_size = 12)


# =============================================================================
# 3. MÉTODO 1: REGLA DEL IQR (TUKEY)
# =============================================================================
#
# Procedimiento:
#   1. Calcular Q1 (percentil 25) y Q3 (percentil 75).
#   2. IQR = Q3 - Q1.
#   3. Son outliers los valores fuera de [Q1 - 1.5*IQR ; Q3 + 1.5*IQR].
# Es exactamente la misma regla que usa geom_boxplot() para los puntos.
# =============================================================================

pib_vec <- paises$pib_pc[!is.na(paises$pib_pc)]

q1  <- quantile(pib_vec, 0.25)
q3  <- quantile(pib_vec, 0.75)
iqr <- IQR(pib_vec)

lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

cat("Rango aceptable (IQR):",
    round(lim_inf, 0), "a", round(lim_sup, 0), "USD\n")

# ¿Qué países quedan fuera?
outliers_iqr <- paises |>
  filter(!is.na(pib_pc),
         pib_pc < lim_inf | pib_pc > lim_sup) |>
  select(pais, region, income, pib_pc) |>
  arrange(desc(pib_pc))

outliers_iqr
cat("Outliers detectados por IQR:", nrow(outliers_iqr), "\n")

# Vamos a ver quiénes son: típicamente Luxemburgo, Suiza, Noruega, Mónaco.
# Pregunta de criterio: ¿son ERRORES o son DATOS REALES?
# Respuesta: son datos reales. Países chicos y muy ricos. La regla del IQR
# los marca como atípicos, pero NO deberíamos eliminarlos sin pensarlo.


# =============================================================================
# 4. MÉTODO 2: Z-SCORE
# =============================================================================
#
# z_i = (x_i - media) / desvío.
# Convención: |z| > 3 -> outlier, asumiendo distribución aprox. normal.
#
# Limitación importante: si la distribución es muy asimétrica (como el PIB
# pc), la propia media y el desvío están "tirados" por los outliers, así
# que el método pierde sensibilidad. Para datos asimétricos, es mejor IQR
# o trabajar sobre la transformación logarítmica (ver clase12c).
# =============================================================================

paises_z <- paises |>
  filter(!is.na(pib_pc)) |>
  mutate(z_pib = (pib_pc - mean(pib_pc)) / sd(pib_pc))

outliers_z <- paises_z |>
  filter(abs(z_pib) > 3) |>
  select(pais, region, pib_pc, z_pib) |>
  arrange(desc(z_pib))

outliers_z

# Comparación: el z-score suele detectar MENOS outliers que el IQR cuando
# los datos son muy asimétricos, porque el desvío está inflado.


# =============================================================================
# 5. OUTLIERS GENUINOS: EL CASO DE LA INFLACIÓN
# =============================================================================
#
# No todo outlier es un error. Algunos son los casos más informativos del
# análisis. Mirar la inflación deja en claro este punto.
# =============================================================================

# Países con mayor inflación reportada en 2022:
paises |>
  filter(!is.na(inflacion)) |>
  arrange(desc(inflacion)) |>
  select(pais, region, inflacion) |>
  head(10)

# Argentina, Venezuela, Líbano, Turquía... son outliers, pero NO son errores:
# son países que efectivamente tuvieron inflación de tres dígitos. La decisión
# de qué hacer con ellos depende del objetivo del análisis:
#   - Si queremos describir la inflación mundial "típica": quizás convenga
#     winsorizar o trabajar en logaritmo.
#   - Si queremos estudiar regímenes de alta inflación: son nuestros casos
#     más informativos. Eliminarlos sería absurdo.


# =============================================================================
# 6. COMPARACIÓN VISUAL DE CRITERIOS
# =============================================================================

paises |>
  filter(!is.na(pib_pc)) |>
  mutate(z       = abs((pib_pc - mean(pib_pc)) / sd(pib_pc)),
         iqr_out = pib_pc < lim_inf | pib_pc > lim_sup,
         z_out   = z > 3,
         tipo    = case_when(
           iqr_out & z_out ~ "Ambos",
           iqr_out         ~ "Solo IQR",
           z_out           ~ "Solo Z",
           TRUE            ~ "Normal"
         )) |>
  ggplot(aes(x = pib_pc, fill = tipo)) +
  geom_histogram(bins = 40, color = "white") +
  scale_fill_manual(values = c("Ambos"    = "red",
                               "Solo IQR" = "orange",
                               "Solo Z"   = "purple",
                               "Normal"   = "steelblue")) +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "PIB per cápita (USD)", y = "Frecuencia", fill = NULL,
       title = "Outliers detectados según el método") +
  theme_minimal(base_size = 11)


# =============================================================================
# 7. DECISIÓN: ELIMINAR, WINSORIZAR O MANTENER
# =============================================================================
#
# Tres opciones, ninguna automática:
#   1. ELIMINAR    : descartar la fila o el valor problemático.
#   2. WINSORIZAR  : reemplazar el extremo por el percentil 1 o 99.
#   3. MANTENER    : dejarlo como está (a veces transformando, ver clase12c).
#
# La decisión depende de:
#   - si el outlier es error o dato genuino,
#   - cuántos datos perdemos al eliminar,
#   - el objetivo del análisis.
# =============================================================================


# -----------------------------------------------------------------------------
# 7.1 Eliminar
# -----------------------------------------------------------------------------
# Cuándo es razonable eliminar:
#   - Outliers que son evidentemente errores de carga (edad = 999,
#     ventas negativas, etc.).
#   - Hay datos suficientes y no se altera la representatividad.
# -----------------------------------------------------------------------------

# Eliminar SOLO los outliers extremos por IQR (manteniendo los NAs por ahora,
# para no mezclar dos decisiones distintas en una sola línea):
paises_sin_out <- paises |>
  filter(is.na(pib_pc) | (pib_pc >= lim_inf & pib_pc <= lim_sup))

# Atención: el filtro "is.na(pib_pc) | ..." es importante. Si pusiéramos
# solo "pib_pc >= lim_inf & pib_pc <= lim_sup", también eliminaríamos las
# filas con NA, mezclando decisiones.

nrow(paises) - nrow(paises_sin_out)   # cuántas filas perdimos


# -----------------------------------------------------------------------------
# 7.2 Winsorizar (recortar sin eliminar)
# -----------------------------------------------------------------------------
# La winsorización REEMPLAZA los valores extremos por el valor del percentil
# elegido (típicamente 1 y 99, o 5 y 95). Conservamos la observación pero
# limitamos su influencia. Útil cuando los outliers son genuinos pero
# distorsionan medias/regresiones.
# -----------------------------------------------------------------------------

# Función general (la dejamos definida para usarla en varios lados).
winsorizar <- function(x, p_inf = 0.01, p_sup = 0.99) {
  lims <- quantile(x, probs = c(p_inf, p_sup), na.rm = TRUE)
  x[!is.na(x) & x < lims[1]] <- lims[1]
  x[!is.na(x) & x > lims[2]] <- lims[2]
  x
}

# Aplicación: winsorizar PIB pc al 1-99 %.
paises_wins <- paises |>
  mutate(pib_pc_w = winsorizar(pib_pc, 0.01, 0.99))

# Comparación de estadísticas:
paises_wins |>
  summarise(
    media_orig    = round(mean(pib_pc,   na.rm = TRUE), 0),
    media_wins    = round(mean(pib_pc_w, na.rm = TRUE), 0),
    sd_orig       = round(sd(pib_pc,     na.rm = TRUE), 0),
    sd_wins       = round(sd(pib_pc_w,   na.rm = TRUE), 0),
    n_no_NA       = sum(!is.na(pib_pc))
  )

# Lo que esperamos ver: la media baja un poco, el desvío baja mucho, y el
# n NO cambia (esa es la gracia respecto a eliminar).


# -----------------------------------------------------------------------------
# 7.3 Mantener: variable indicadora de "extremo"
# -----------------------------------------------------------------------------
# Si los outliers son genuinos pero queremos identificarlos, los marcamos
# con una variable indicadora. Después, en una regresión, podemos incluir
# esa variable como control o usarla para colorear gráficos.
# -----------------------------------------------------------------------------

paises_marcado <- paises |>
  mutate(es_outlier_pib = !is.na(pib_pc) & (pib_pc < lim_inf | pib_pc > lim_sup))

paises_marcado |>
  count(es_outlier_pib)


# =============================================================================
# 8. JUSTIFICACIÓN E IMPACTO: ANTES VS DESPUÉS
# =============================================================================
#
# Cualquier decisión de limpieza cambia los datos. Hay que MEDIR Y REPORTAR
# cuánto cambian las estadísticas clave para verificar que la decisión es
# razonable. Esto vale tanto para el TP como para cualquier informe profesional.
# =============================================================================


# -----------------------------------------------------------------------------
# 8.1 Función para comparar estadísticas descriptivas
# -----------------------------------------------------------------------------

comparar_stats <- function(original, modificado, nombre_mod) {

  # Sacamos NAs de cada vector porque las estadísticas no los manejan bien.
  o <- original[!is.na(original)]
  m <- modificado[!is.na(modificado)]

  tibble(
    version = c("Original", nombre_mod),
    n       = c(length(o), length(m)),
    media   = round(c(mean(o),     mean(m)),     1),
    mediana = round(c(median(o),   median(m)),   1),
    desvio  = round(c(sd(o),       sd(m)),       1),
    p05     = round(c(quantile(o, .05), quantile(m, .05)), 1),
    p95     = round(c(quantile(o, .95), quantile(m, .95)), 1)
  )
}


# -----------------------------------------------------------------------------
# 8.2 Aplicación al PIB per cápita
# -----------------------------------------------------------------------------

pib_orig  <- paises$pib_pc
pib_wins  <- winsorizar(pib_orig, 0.01, 0.99)
pib_clean <- pib_orig[!is.na(pib_orig) &
                        pib_orig >= lim_inf &
                        pib_orig <= lim_sup]

bind_rows(
  comparar_stats(pib_orig, pib_wins,  "Winsorizado (1-99%)"),
  comparar_stats(pib_orig, pib_clean, "Sin outliers (IQR)") |>
    filter(version != "Original")
)

# Lectura típica de esta tabla:
#   - Si la MEDIA cambia mucho pero la MEDIANA casi no -> los outliers
#     estaban tirando la media. Limpiar fue una decisión razonable.
#   - Si el N cae mucho -> estamos perdiendo demasiada información.
#     Conviene winsorizar en lugar de eliminar.
#   - Si el desvío baja mucho -> ganamos estabilidad pero podemos estar
#     ocultando variabilidad real.


# -----------------------------------------------------------------------------
# 8.3 Buena práctica para el TP
# -----------------------------------------------------------------------------
#
# En el informe, incluir una tabla como la de arriba más una frase del tipo:
#
#   "Se identificaron 25 países como outliers del PIB per cápita por la
#    regla del IQR (1.5x). Tras inspección manual, se trata de países
#    pequeños y de altos ingresos (Luxemburgo, Mónaco, Suiza, etc.), por
#    lo que NO se eliminaron. En su lugar, se aplicó winsorización al
#    1-99 %, lo que redujo la media de USD 21.500 a USD 18.700 y el
#    desvío de 27.000 a 19.000, manteniendo el N = 215 sin cambios."
#
# La clave no es elegir una técnica "correcta", sino DOCUMENTAR la
# decisión y mostrar que se evaluó el efecto.


# =============================================================================
# 9. CIERRE Y PREGUNTAS PARA PENSAR
# =============================================================================
#
# Resumen del flujo de trabajo:
#   1. Mirar (boxplot + histograma).
#   2. Detectar (IQR, z-score; en distribuciones asimétricas, IQR es mejor).
#   3. Diagnosticar (¿errores o datos genuinos?).
#   4. Decidir (eliminar / winsorizar / mantener).
#   5. Documentar el impacto (antes vs después).
#
# -----------------------------------------------------------------------------
# Preguntas para pensar / hacer en casa:
# -----------------------------------------------------------------------------
#
#   1. Apliquen winsorización a inflacion (con percentiles 5-95). ¿Cuánto
#      cambia la media mundial de inflación? ¿Tiene sentido winsorizar en
#      este caso, o se está borrando información económicamente relevante?
#
#   2. Apliquen los dos métodos (IQR y z-score) a log(pib_pc). ¿Cambia la
#      cantidad de outliers detectados respecto a aplicarlos sobre el PIB pc
#      sin transformar? ¿Por qué?
#
#   3. Construyan una tabla comparativa para inflacion: original vs sin
#      outliers (IQR) vs winsorizado al 5-95 %. ¿Cuál preferirían reportar
#      como "media mundial de inflación 2022"?
#
# -----------------------------------------------------------------------------
# Ideas clave para llevarse:
# -----------------------------------------------------------------------------
#
#   - No hay regla automática para decidir qué hacer con outliers: hay que
#     mirar los datos y entender el contexto.
#   - Eliminar es la opción más cara: pierde información. Winsorizar es una
#     alternativa que conserva el N y atenúa el efecto.
#   - El z-score pierde potencia en distribuciones asimétricas. Para PIB,
#     ingresos o ventas, IQR (o log + z-score) suele funcionar mejor.
#   - Toda decisión de limpieza debe estar DOCUMENTADA y su impacto MEDIDO.
#
# =============================================================================
