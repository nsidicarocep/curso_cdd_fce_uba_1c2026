# =============================================================================
# Ciencia de Datos para Economía y Negocios - FCE-UBA
# Clase 12 (virtual): Datos faltantes, outliers y transformaciones
# -----------------------------------------------------------------------------
# Contenidos:
#   1. Datos faltantes: detección, visualización y tipos (MCAR, MAR, MNAR)
#   2. Outliers: detección por IQR y z-score
#   3. Framework de decisión: eliminar, imputar, mantener (winsorizar)
#   4. Imputación múltiple con {mice} (idea general)
#   5. Transformaciones: logaritmo, estandarización, normalización min-max
#   6. Justificación e impacto: comparar estadísticas antes vs después
# -----------------------------------------------------------------------------
# Datos: indicadores del Banco Mundial (paquete {WDI}) para el año 2022.
#        Lo usamos porque tiene NAs reales (no todos los países reportan todo)
#        y outliers genuinos (Luxemburgo en PIB pc, Venezuela en inflación, etc.).
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Preparación del entorno
# -----------------------------------------------------------------------------

# Librerías que vamos a usar. Si alguna no la tienen, la instalan con
# install.packages("nombre_del_paquete").

library(tidyverse)   # dplyr, ggplot2, tidyr, etc.
library(WDI)         # indicadores del Banco Mundial
library(scales)      # para formatear ejes
library(mice)        # imputación múltiple (sección 4.4)

options(scipen = 999)   # evita notación científica en los prints
set.seed(42)            # reproducibilidad para los pasos aleatorios


# =============================================================================
# 1. CARGA Y EXPLORACIÓN INICIAL DE LOS DATOS
# =============================================================================
#
# Vamos a bajar siete indicadores económicos y sociales del Banco Mundial
# para todos los países disponibles, en el año 2022. La elección de los
# indicadores no es casual: los elegimos justamente porque traen problemas
# de calidad de datos típicos de la vida real.
# =============================================================================


# -----------------------------------------------------------------------------
# 1.1 Descarga del dataset desde WDI
# -----------------------------------------------------------------------------

# Cada indicador del Banco Mundial tiene un código único. Los buscamos en:
# https://data.worldbank.org/  o con WDIsearch("palabra clave")

indicadores <- c(
  pib_pc       = "NY.GDP.PCAP.CD",        # PIB per cápita (USD corrientes)
  poblacion    = "SP.POP.TOTL",           # Población total
  esperanza    = "SP.DYN.LE00.IN",        # Esperanza de vida al nacer
  inflacion    = "FP.CPI.TOTL.ZG",        # Inflación anual (% IPC)
  gasto_edu    = "SE.XPD.TOTL.GD.ZS",     # Gasto público en educación (% PIB)
  gasto_salud  = "SH.XPD.CHEX.GD.ZS",     # Gasto en salud (% PIB)
  internet     = "IT.NET.USER.ZS"         # Usuarios de internet (% población)
)

# Descargamos el panel para 2022 con metadata regional incluida.
wdi_raw <- WDI(
  country   = "all",
  indicator = indicadores,
  start     = 2022,
  end       = 2022,
  extra     = TRUE          # trae región e ingreso, útil para diagnosticar MAR
)

# Filtramos solo países (WDI también devuelve agregados como "World",
# "Latin America", etc., que tienen region = "Aggregates").
paises <- wdi_raw |>
  filter(region != "Aggregates") |>
  select(pais = country, iso = iso3c, region, income,
         pib_pc, poblacion, esperanza, inflacion,
         gasto_edu, gasto_salud, internet)

# Veamos qué tenemos.
glimpse(paises)
nrow(paises)


# -----------------------------------------------------------------------------
# 1.2 Primera mirada
# -----------------------------------------------------------------------------

# Resumen rápido de las variables numéricas.
paises |>
  select(where(is.numeric)) |>
  summary()

# Lo primero que salta a la vista en summary() es que TODAS las variables
# numéricas tienen NAs (la última línea de cada columna dice "NA's : ...").
# Eso es típico cuando se trabaja con datos comparativos internacionales:
# no todos los países reportan todo todos los años.


# =============================================================================
# 2. DATOS FALTANTES (MISSING VALUES)
# =============================================================================
#
# Antes de tocar nada, lo primero es DIAGNOSTICAR: ¿cuántos faltan, dónde
# están, y por qué faltan? La decisión de qué hacer (eliminar, imputar,
# mantener) viene DESPUÉS del diagnóstico, no antes.
# =============================================================================


# -----------------------------------------------------------------------------
# 2.1 Detección: ¿cuántos NAs hay y dónde?
# -----------------------------------------------------------------------------

# Dos preguntas básicas: cantidad y proporción de NAs por columna.

# Cantidad absoluta:
paises |>
  summarise(across(everything(), ~ sum(is.na(.))))

# Proporción (en %), solo para variables numéricas:
paises |>
  summarise(across(where(is.numeric),
                   ~ round(mean(is.na(.)) * 100, 1),
                   .names = "pct_na_{.col}"))

# Lectura rápida: el gasto público en educación es el más problemático;
# inflación y esperanza de vida tienen pocos NAs; PIB per cápita y población
# están casi completos.


# -----------------------------------------------------------------------------
# 2.2 Visualizar el patrón de faltantes
# -----------------------------------------------------------------------------

# Un mapa de calor nos permite ver si los NAs están concentrados en ciertos
# países (filas) o ciertas variables (columnas). Si hay filas con muchos
# NAs juntos, probablemente son países con poca capacidad estadística.

paises |>
  mutate(fila = row_number()) |>
  select(fila, pib_pc, poblacion, esperanza, inflacion,
         gasto_edu, gasto_salud, internet) |>
  pivot_longer(-fila, values_transform = as.character) |>
  mutate(es_na = is.na(value)) |>
  ggplot(aes(x = name, y = fila, fill = es_na)) +
  geom_tile() +
  scale_fill_manual(values = c("grey90", "tomato"),
                    labels = c("Presente", "Faltante")) +
  labs(x = NULL, y = "País (orden alfabético)", fill = NULL,
       title = "Mapa de faltantes en el panel WDI 2022") +
  theme_minimal(base_size = 11)


# -----------------------------------------------------------------------------
# 2.3 Diagnóstico del tipo de missing (MCAR / MAR / MNAR)
# -----------------------------------------------------------------------------

# Recordemos los tres tipos:
#   MCAR (Missing Completely At Random): la falta no depende de nada.
#   MAR  (Missing At Random):             la falta depende de variables
#                                          OBSERVADAS (ej: región, ingreso).
#   MNAR (Missing Not At Random):         la falta depende del PROPIO valor
#                                          faltante (ej: países con alta
#                                          inflación que la ocultan).
#
# No existe un test definitivo para distinguirlos, pero podemos buscar
# patrones cruzando el missingness con otras variables.

# Hipótesis a chequear: ¿los NAs en gasto en educación se concentran en
# países de bajo ingreso? Si sí, es evidencia de MAR (la falta depende del
# nivel de ingreso, que sí observamos).

paises |>
  mutate(falta_edu = is.na(gasto_edu)) |>
  group_by(income) |>
  summarise(
    n           = n(),
    pct_na_edu  = round(mean(falta_edu) * 100, 1),
    .groups     = "drop"
  ) |>
  arrange(desc(pct_na_edu))

# Lo mismo por región:
paises |>
  mutate(falta_edu = is.na(gasto_edu)) |>
  group_by(region) |>
  summarise(
    n           = n(),
    pct_na_edu  = round(mean(falta_edu) * 100, 1),
    .groups     = "drop"
  ) |>
  arrange(desc(pct_na_edu))

# Si vemos diferencias claras entre grupos, hay evidencia de MAR.
# Eso nos habilita a imputar usando esas variables (income, region).

# Ejemplo de razonamiento MNAR (no testeable directamente, pero importante):
#   - Países con inflación muy alta a veces dejan de reportar el indicador
#     oficial (Venezuela, Argentina en distintos momentos).
#   - Si eso pasa, la falta depende del propio valor (alto) que no vemos.
#   - Eliminar esas filas SUBESTIMA el nivel de inflación mundial.


# -----------------------------------------------------------------------------
# 2.4 Pruebas formales: ¿podemos rechazar MCAR?
# -----------------------------------------------------------------------------
#
# La sección anterior fue exploratoria: vimos que el % de NAs varía entre
# grupos. Ahora vamos a formalizar esa intuición con TESTS DE HIPÓTESIS,
# los mismos que vieron en la clase 9.
#
# La idea es simple: creamos una variable indicadora de missingness y la
# testeamos contra las otras variables del dataset.
#
#   H0: la probabilidad de que falte el dato es independiente de la otra
#       variable -> compatible con MCAR.
#   H1: hay asociación -> evidencia de MAR (o MNAR).
#
# Tres herramientas, según el tipo de variable:
#   A) Chi-cuadrado:        missingness vs variable categórica.
#   B) Test t (Welch):      missingness vs variable numérica.
#   C) Test de Little:      test ómnibus sobre todas las variables a la vez.
# -----------------------------------------------------------------------------


# A. Chi-cuadrado: ¿la falta de gasto_edu depende del nivel de ingreso?
# ------------------------------------------------------------------

# Paso 1: indicadora de missingness.
paises_test <- paises |>
  mutate(falta_edu = is.na(gasto_edu))

# Paso 2: tabla de contingencia entre missingness e income.
tabla_inc <- table(paises_test$income, paises_test$falta_edu)
tabla_inc

# Paso 3: el test.
chi_inc <- chisq.test(tabla_inc)
chi_inc

# Lectura del resultado:
#   - X-squared: estadístico de prueba.
#   - df:        grados de libertad ((filas-1) x (columnas-1)).
#   - p-value:   probabilidad de observar una asociación al menos tan
#                fuerte como ésta si H0 fuese cierta.
#
# Si p-value < 0.05 -> rechazamos H0 -> la falta NO es independiente del
# nivel de ingreso -> NO es MCAR -> hay evidencia de MAR.

# Lo mismo contra la región:
chi_reg <- chisq.test(table(paises_test$region, paises_test$falta_edu))
chi_reg


# B. Test t: ¿la falta de gasto_edu depende del PIB per cápita?
# -------------------------------------------------------------

# Comparamos la media de PIB pc entre el grupo "tiene NA en gasto_edu" y
# el grupo "tiene dato". Si las medias son distintas, la falta NO es
# independiente del PIB pc.
#
#   H0: media(pib_pc | falta) = media(pib_pc | no falta)   -> MCAR-compatible.
#   H1: medias distintas                                    -> MAR.

t_pib <- t.test(pib_pc ~ falta_edu, data = paises_test)
t_pib

# Foto descriptiva previa (igual que en la clase 9):
paises_test |>
  group_by(falta_edu) |>
  summarise(n            = n(),
            media_pib    = round(mean(pib_pc, na.rm = TRUE), 0),
            mediana_pib  = round(median(pib_pc, na.rm = TRUE), 0),
            .groups      = "drop")

# Si los países con datos faltantes en educación tienen sistemáticamente
# menor PIB pc, la asociación es clara: el missingness depende del nivel
# de desarrollo (variable observable) -> MAR.


# C. Test ómnibus de Little (paquete {naniar})
# --------------------------------------------
# Lo anterior chequea de a una variable por vez. El test de Little (1988)
# es un test CONJUNTO: testea si los patrones de NAs en TODO el dataset
# son consistentes con MCAR.
#
#   H0: todos los datos son MCAR.
#   H1: al menos una variable no es MCAR.
#
# Lo implementa el paquete {naniar} en la función mcar_test().
# Si no lo tienen, instalar con: install.packages("naniar").

# library(naniar)
#
# # Le pasamos solo las variables numéricas (es el caso de uso típico):
# paises |>
#   select(pib_pc, poblacion, esperanza, inflacion,
#          gasto_edu, gasto_salud, internet) |>
#   mcar_test()
#
# El resultado tiene un p-value: si es < 0.05, rechazamos MCAR globalmente.
# En este dataset es prácticamente seguro que se rechace, justamente porque
# los países con menor capacidad estadística reportan menos cosas.


# -----------------------------------------------------------------------------
# 2.5 Aclaraciones importantes sobre estos tests
# -----------------------------------------------------------------------------
#
#   1. No rechazar H0 NO prueba que sea MCAR. Solo dice que no encontramos
#      evidencia en contra. Es la asimetría clásica de los tests: rechazar
#      es informativo, no rechazar es ambiguo (puede ser que H0 sea cierta
#      o que el test tenga poca potencia).
#
#   2. Estos tests distinguen MCAR de MAR, pero NO distinguen MAR de MNAR.
#      Esa es la dificultad fundamental de MNAR: como el valor faltante
#      es justamente lo que no observamos, no podemos testear si la falta
#      depende de él. Para MNAR hay que recurrir a ARGUMENTOS DE DOMINIO
#      (ej: "sabemos que los países con alta inflación dejan de reportar")
#      o a modelos de selección (Heckman) que asumen una estructura
#      paramétrica.
#
#   3. Los tests pueden dar resultados distintos según la variable contra
#      la que se chequee. Es esperable: la falta puede depender de income
#      pero no de población. Hay que mirar el patrón completo.
#
#   4. Como siempre con tests, p-value depende del N. Con 200+ países,
#      diferencias chicas pueden dar p < 0.05. Combinar el test con el
#      tamaño del efecto (cuán distintas son las medias o las proporciones)
#      es buena práctica.


# =============================================================================
# 3. OUTLIERS: DETECCIÓN Y DIAGNÓSTICO
# =============================================================================
#
# Un outlier es una observación que se aleja mucho del resto. Puede ser:
#   - un error de carga (edad = 999, ventas negativas), o
#   - un dato genuinamente extremo (Luxemburgo factura mucho más que el
#     promedio porque ES atípico, no porque haya un error).
#
# Vamos a usar dos métodos clásicos: IQR y z-score.
# =============================================================================


# -----------------------------------------------------------------------------
# 3.1 Exploración visual: el primer paso siempre
# -----------------------------------------------------------------------------

# Un boxplot del PIB per cápita ya nos muestra la asimetría brutal y los
# valores extremos en la parte de arriba.

paises |>
  filter(!is.na(pib_pc)) |>
  ggplot(aes(y = pib_pc)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, width = 0.4) +
  scale_y_continuous(labels = label_dollar()) +
  labs(y = "PIB per cápita (USD)",
       title = "Distribución del PIB per cápita - países, 2022") +
  theme_minimal(base_size = 12)

# Un histograma muestra la cola derecha:
paises |>
  filter(!is.na(pib_pc)) |>
  ggplot(aes(x = pib_pc)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = label_dollar()) +
  labs(x = "PIB per cápita (USD)", y = "Frecuencia") +
  theme_minimal(base_size = 12)


# -----------------------------------------------------------------------------
# 3.2 Método 1: regla del IQR (Tukey)
# -----------------------------------------------------------------------------
#
# Procedimiento:
#   1. Calcular Q1 (percentil 25) y Q3 (percentil 75).
#   2. IQR = Q3 - Q1.
#   3. Son outliers los valores fuera de [Q1 - 1.5*IQR ; Q3 + 1.5*IQR].
# Es exactamente la misma regla que usa geom_boxplot() para los puntos.
# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------
# 3.3 Método 2: z-score
# -----------------------------------------------------------------------------
#
# z_i = (x_i - media) / desvío.
# Convención: |z| > 3 -> outlier, asumiendo distribución aprox. normal.
#
# Limitación importante: si la distribución es muy asimétrica (como el PIB
# pc), la propia media y el desvío están "tirados" por los outliers, así
# que el método pierde sensibilidad. Para datos asimétricos, es mejor IQR
# o trabajar sobre la transformación logarítmica (lo vemos en la sección 5).
# -----------------------------------------------------------------------------

paises_z <- paises |>
  filter(!is.na(pib_pc)) |>
  mutate(z_pib = (pib_pc - mean(pib_pc)) / sd(pib_pc))

outliers_z <- paises_z |>
  filter(abs(z_pib) > 3) |>
  select(pais, region, pib_pc, z_pib) |>
  arrange(desc(z_pib))

outliers_z

# Comparación: el z-score suele detectar MENOS outliers que el IQR cuando
# los datos son muy asimétricos, porque el desvío está inflado. Probemos
# también con la inflación, que tiene casos extremos genuinos.


# -----------------------------------------------------------------------------
# 3.4 Outliers en inflación: el caso de los datos genuinamente extremos
# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------
# 3.5 Comparación visual de criterios sobre PIB pc
# -----------------------------------------------------------------------------

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
# 4. FRAMEWORK DE DECISIÓN: ELIMINAR, IMPUTAR O MANTENER
# =============================================================================
#
# Tres opciones, ninguna automática:
#   1. ELIMINAR : descartar la fila o el valor problemático.
#   2. IMPUTAR  : reemplazar por una estimación.
#   3. MANTENER : dejarlo como está (a veces con una variable indicadora,
#                 a veces winsorizado).
#
# La decisión depende de:
#   - el tipo de missing (MCAR vs MAR vs MNAR),
#   - si el outlier es error o dato genuino,
#   - cuántos datos perdemos al eliminar,
#   - el objetivo del análisis.
# =============================================================================


# -----------------------------------------------------------------------------
# 4.1 Eliminar
# -----------------------------------------------------------------------------
# Cuándo es razonable eliminar:
#   - NAs MCAR y poco frecuentes (< 5 %).
#   - Outliers que son evidentemente errores de carga.
#   - Hay datos suficientes y no se altera la representatividad.
# -----------------------------------------------------------------------------

# Eliminar filas con NA en una variable específica:
paises_completos <- paises |>
  filter(!is.na(pib_pc), !is.na(esperanza))

nrow(paises) - nrow(paises_completos)   # cuántas filas perdimos

# Eliminar SOLO los outliers extremos por IQR (manteniendo los NAs por ahora,
# para no mezclar dos decisiones distintas en una sola línea):
paises_sin_out <- paises |>
  filter(is.na(pib_pc) | (pib_pc >= lim_inf & pib_pc <= lim_sup))

# Atención: el filtro "is.na(pib_pc) | ..." es importante. Si pusiéramos
# solo "pib_pc >= lim_inf & pib_pc <= lim_sup", también eliminaríamos las
# filas con NA, mezclando decisiones.


# -----------------------------------------------------------------------------
# 4.2 Imputar - opción A: por la mediana global
# -----------------------------------------------------------------------------
# La mediana es preferible a la media porque es robusta a outliers.
# Esta imputación es simple pero IGNORA las relaciones entre variables.
# -----------------------------------------------------------------------------

mediana_edu <- median(paises$gasto_edu, na.rm = TRUE)

paises_imp_simple <- paises |>
  mutate(gasto_edu_imp = ifelse(is.na(gasto_edu),
                                mediana_edu,
                                gasto_edu))

# Verificación: ahora no quedan NAs en la versión imputada.
paises_imp_simple |>
  summarise(na_orig = sum(is.na(gasto_edu)),
            na_imp  = sum(is.na(gasto_edu_imp)))


# -----------------------------------------------------------------------------
# 4.3 Imputar - opción B: por la mediana del grupo (mejor)
# -----------------------------------------------------------------------------
# Si diagnosticamos MAR (la falta depende del nivel de ingreso o región),
# imputar usando la mediana DEL MISMO GRUPO captura mejor las diferencias.
# Es una imputación "informada" por una variable observada.
# -----------------------------------------------------------------------------

paises_imp_grupo <- paises |>
  group_by(income) |>
  mutate(gasto_edu_imp = ifelse(is.na(gasto_edu),
                                median(gasto_edu, na.rm = TRUE),
                                gasto_edu)) |>
  ungroup()

# Comparemos: si imputamos por grupo, los países low-income reciben un
# valor distinto al que reciben los high-income. Eso preserva la
# heterogeneidad estructural.

paises_imp_grupo |>
  group_by(income) |>
  summarise(
    n          = n(),
    n_imputado = sum(is.na(gasto_edu)),
    media_imp  = round(mean(gasto_edu_imp, na.rm = TRUE), 2),
    .groups    = "drop"
  )

# Regla útil: si imputamos más del 20-30 % de una variable, conviene
# preguntarse si esa variable es realmente confiable para el análisis.


# -----------------------------------------------------------------------------
# 4.4 Imputar - opción C: imputación múltiple con {mice}
# -----------------------------------------------------------------------------
# La imputación simple "inventa" un valor puntual y subestima la incertidumbre.
# La imputación múltiple genera m versiones del dataset, corre el análisis
# en cada una y combina los resultados (reglas de Rubin) para obtener IC
# que reflejen honestamente la incertidumbre de la imputación.
#
# Hay DOS flujos posibles según el objetivo:
#
#   FLUJO 1 (descriptivo): quiero un dataset completo para hacer
#                          gráficos, tablas o pasarlo a otra etapa
#                          de análisis. Imputo y "colapso" a un dataset.
#
#   FLUJO 2 (inferencial): quiero correr un modelo (regresión, t-test)
#                          con intervalos de confianza honestos. Imputo,
#                          corro el modelo en cada imputación, y combino
#                          los resultados con reglas de Rubin.
#
# Si no lo tienen instalado: install.packages("mice")
# -----------------------------------------------------------------------------

library(mice)


# Paso 0: preparar el dataset
# ---------------------------
# Importante: NO incluir variables tipo identificador (pais, iso) ni cosas
# que no queremos que se usen como predictoras. mice usa TODAS las columnas
# del data frame como potenciales predictoras de las imputaciones.

datos_imp <- paises |>
  select(pib_pc, esperanza, inflacion,
         gasto_edu, gasto_salud, internet,
         income, region)
# Dejamos income y region porque ayudan a imputar mejor (recordemos que
# diagnosticamos MAR contra ellas). mice maneja categóricas automáticamente
# con métodos distintos (logreg, polyreg).


# Paso 1: inspeccionar los patrones de NAs
# ----------------------------------------
# md.pattern() muestra qué COMBINACIONES de NAs aparecen en el dataset.
# Cada fila es un patrón; los 1 son datos presentes y los 0 son faltantes.

md.pattern(datos_imp, rotate.names = TRUE)

# Lo más útil de este gráfico: ver si hay variables que SIEMPRE faltan
# juntas. Si dos variables tienen exactamente el mismo patrón de NAs,
# una no puede imputar a la otra (no hay información cruzada).


# Paso 2: correr la imputación
# ----------------------------
# mice() genera m datasets imputados. Argumentos clave:
#   m       : cantidad de imputaciones (default = 5; usar 10-20 si hay
#             muchos NAs o si el análisis es importante).
#   method  : método por variable. Si lo dejamos vacío, mice elige según
#             el tipo: "pmm" para numéricas, "logreg" para binarias,
#             "polyreg" para categóricas con varios niveles.
#   seed    : semilla para reproducibilidad (igual que set.seed).
#   maxit   : iteraciones del algoritmo (default = 5; suele alcanzar).

imp <- mice(datos_imp, m = 5, method = "pmm", seed = 42, printFlag = FALSE)

# pmm = "predictive mean matching": para cada NA, mice predice un valor
# con regresión y después busca el valor REAL más cercano en los datos
# observados, y lo usa como imputación. Ventaja: nunca imputa valores
# imposibles (negativos, fuera de rango).

# printFlag = FALSE solo silencia los mensajes de progreso; sin él,
# mice imprime una línea por iteración.


# Paso 3: inspeccionar el objeto mice
# -----------------------------------

summary(imp)            # resumen general

imp$method              # qué método usó para cada variable

# Los valores imputados están en imp$imp$<variable>. Es una matriz con
# m columnas (una por imputación) y una fila por cada NA original.

head(imp$imp$gasto_edu)    # primeros NAs imputados, las 5 versiones

# Para cada país que tenía NA en gasto_edu, vemos cinco valores imputados
# distintos. Si las cinco versiones son MUY parecidas, la imputación es
# "estable" (poca incertidumbre). Si difieren mucho, la incertidumbre es
# alta y se va a reflejar en intervalos más anchos al final.


# Paso 4: chequeo visual (¿las imputaciones son razonables?)
# ----------------------------------------------------------
# stripplot() y densityplot() comparan distribuciones de valores
# observados (azul) vs imputados (rojo). Lo que queremos ver: que se
# parezcan. Si los rojos están todos del mismo lado, mice está
# extrapolando hacia una zona poco poblada y conviene revisar.

stripplot(imp, gasto_edu ~ .imp, pch = 20, cex = 1.2)
# .imp es la "imputación 0" (datos observados) más las 5 imputaciones.

densityplot(imp, ~ gasto_edu)
# Idem, en versión densidad.


# -----------------------------------------------------------------------------
# FLUJO 1: extraer un dataset completo para análisis descriptivo
# -----------------------------------------------------------------------------

# Opción A: usar UNA imputación específica (típicamente la primera).
datos_completos <- complete(imp, action = 1)

nrow(datos_completos)
sum(is.na(datos_completos$gasto_edu))   # debería ser 0

# Opción B: apilar las m imputaciones en formato "long".
datos_long <- complete(imp, action = "long")
# Cada fila aparece m veces (una por imputación), con una columna .imp
# que indica cuál es. Útil para mostrar variabilidad entre imputaciones.

datos_long |>
  group_by(.imp) |>
  summarise(media_edu = round(mean(gasto_edu), 2))
# Cada imputación da una media ligeramente distinta -> esa diferencia
# ES la incertidumbre que la imputación simple ocultaba.

# Opción C: promediar las m imputaciones para tener un único valor por NA.
# OJO: esto subestima la incertidumbre (pierde el sentido de imputación
# múltiple). Usar SOLO si lo único que queremos es un valor "central" para
# graficar o describir.
datos_promedio <- complete(imp, action = "long") |>
  group_by(.id) |>           # .id identifica la observación original
  summarise(across(where(is.numeric), mean), .groups = "drop")


# -----------------------------------------------------------------------------
# Mergear los imputados al dataset ORIGINAL (paises)
# -----------------------------------------------------------------------------
# Lo que devuelve complete() tiene SOLO las columnas que le pasamos a mice
# (pib_pc, esperanza, inflacion, gasto_edu, gasto_salud, internet, income,
# region). Si queremos conservar pais e iso del dataset original, hay que
# mergear. Como complete() preserva el orden y la cantidad de filas del
# input, podemos hacerlo de forma directa.
# -----------------------------------------------------------------------------

datos_completos <- complete(imp, action = 1)

# Antes de pisar las columnas, MARCAMOS qué valores eran NA en el original.
# Esto es buena práctica: deja documentado dentro del propio dataset qué
# fue dato observado y qué fue imputado. Sirve después para filtrar,
# colorear gráficos, o reportar resultados con y sin imputados.

paises_imp <- paises |>
  mutate(
    # Indicadoras de "fue imputado" (TRUE = el valor original era NA)
    pib_pc_imp_flag      = is.na(pib_pc),
    esperanza_imp_flag   = is.na(esperanza),
    inflacion_imp_flag   = is.na(inflacion),
    gasto_edu_imp_flag   = is.na(gasto_edu),
    gasto_salud_imp_flag = is.na(gasto_salud),
    internet_imp_flag    = is.na(internet),
    
    # Reemplazo de las columnas con los valores imputados
    pib_pc      = datos_completos$pib_pc,
    esperanza   = datos_completos$esperanza,
    inflacion   = datos_completos$inflacion,
    gasto_edu   = datos_completos$gasto_edu,
    gasto_salud = datos_completos$gasto_salud,
    internet    = datos_completos$internet
  )

# Verificación 1: las columnas identificatorias siguen ahí
head(paises_imp |> select(pais, iso, region, pib_pc, gasto_edu))

# Verificación 2: ya no hay NAs en las variables imputadas
paises_imp |>
  summarise(across(c(pib_pc, esperanza, inflacion,
                     gasto_edu, gasto_salud, internet),
                   ~ sum(is.na(.))))

# Verificación 3: cuántos valores fueron imputados por variable
paises_imp |>
  summarise(across(ends_with("_imp_flag"), sum))


# Ejemplo de uso de las indicadoras: comparar la distribución de
# valores observados vs imputados para gasto en educación.
paises_imp |>
  ggplot(aes(x = gasto_edu, fill = gasto_edu_imp_flag)) +
  geom_histogram(bins = 25, color = "white", position = "identity",
                 alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato"),
                    labels = c("Observado", "Imputado")) +
  labs(x = "Gasto público en educación (% PIB)", y = "Frecuencia",
       fill = NULL,
       title = "Valores observados vs imputados por mice") +
  theme_minimal(base_size = 11)

# Si los rojos (imputados) caen razonablemente dentro del rango de los
# azules (observados), la imputación es plausible. Si caen todos en una
# punta, mice está extrapolando hacia una zona poco sustentada por datos.


# -----------------------------------------------------------------------------
# Variante: dataset original con las m imputaciones apiladas
# -----------------------------------------------------------------------------
# Si más adelante queremos usar TODAS las imputaciones (no quedarnos con
# una), podemos crear una versión "long" del dataset original. Esto es lo
# que pool() hace internamente, pero a veces es útil tenerlo a mano para
# graficar la variabilidad entre imputaciones.

paises_imp_long <- complete(imp, action = "long", include = TRUE) |>
  as_tibble() |>
  # .id es el número de fila original; lo usamos para mergear con paises
  mutate(.id = as.integer(.id)) |>
  left_join(
    paises |> mutate(.id = row_number()) |> select(.id, pais, iso),
    by = ".id"
  )

# include = TRUE agrega también la versión con NAs originales (.imp = 0),
# útil para comparar.
paises_imp_long |>
  count(.imp)

# Ahora podemos ver, por ejemplo, las cinco imputaciones para Argentina:
paises_imp_long |>
  filter(pais == "Argentina") |>
  select(.imp, pais, gasto_edu, gasto_salud)


# -----------------------------------------------------------------------------
# FLUJO 2: análisis inferencial con reglas de Rubin (with + pool)
# -----------------------------------------------------------------------------

# Pregunta: ¿cómo varía la esperanza de vida con el PIB pc, controlando
# por gasto en salud y educación?
#
# Si corremos la regresión sobre cada imputación, obtenemos m versiones de
# cada coeficiente. Las reglas de Rubin combinan esas m versiones en un
# único estimador con su error estándar, sumando dos fuentes de variabilidad:
#   - la varianza WITHIN  (la que tendríamos en un solo dataset),
#   - la varianza BETWEEN (la diferencia entre imputaciones).
# El SE final es más grande que el de una sola imputación, y eso es lo
# correcto: refleja que NO sabemos cuál era el valor faltante real.

# Paso 1: ajustar el modelo en cada uno de los m datasets imputados.
fit_imp <- with(imp, lm(esperanza ~ log(pib_pc) + gasto_salud + gasto_edu))

# Paso 2: combinar resultados (reglas de Rubin).
resultado_pool <- pool(fit_imp)
summary(resultado_pool, conf.int = TRUE)

# La salida tiene los coeficientes habituales más:
#   - fmi  : fraction of missing information (qué porcentaje de la
#            varianza del coeficiente viene de la incertidumbre por
#            imputación). Si es alto (> 0.3-0.4), la imputación está
#            agregando mucho ruido y conviene revisar.
#   - lambda: proporción de la varianza atribuible a los NAs.


# -----------------------------------------------------------------------------
# Comparación: imputación simple vs múltiple
# -----------------------------------------------------------------------------
# Para que se vea por qué importa: el SE de una imputación simple
# subestima la incertidumbre. Comparemos los IC del coeficiente de log(pib_pc).

# (a) Imputación simple por mediana del grupo:
datos_simple <- paises |>
  group_by(income) |>
  mutate(across(c(gasto_edu, gasto_salud, pib_pc, esperanza),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) |>
  ungroup() |>
  filter(!is.na(pib_pc), pib_pc > 0, !is.na(esperanza))

fit_simple <- lm(esperanza ~ log(pib_pc) + gasto_salud + gasto_edu,
                 data = datos_simple)
confint(fit_simple)["log(pib_pc)", ]

# (b) Imputación múltiple (lo que ya calculamos):
summary(resultado_pool, conf.int = TRUE) |>
  filter(term == "log(pib_pc)") |>
  select(estimate, `2.5 %`, `97.5 %`)

# Lo esperable: el IC de la imputación múltiple es MÁS ANCHO. Esa diferencia
# en ancho es exactamente la incertidumbre que la imputación simple oculta.


# -----------------------------------------------------------------------------
# Cuándo vale la pena la imputación múltiple
# -----------------------------------------------------------------------------
#
#   - NAs MAR con más del 5-10 % de los datos.
#   - Análisis inferencial: tests, IC, regresiones que se reportan.
#   - Trabajo profesional o académico publicable.
#
# Para descripción simple (un boxplot, una tabla resumen), la imputación
# por grupo de la sección 4.3 alcanza. Lo importante es DOCUMENTAR qué
# método se usó y por qué.
#
# Limitación recordatoria: mice asume MAR. Si el missing es MNAR, ningún
# método de imputación basado solo en variables observadas lo arregla.
# En ese caso, la mejor estrategia es combinar imputación + variable
# indicadora + análisis de sensibilidad.


# -----------------------------------------------------------------------------
# 4.5 Mantener: variable indicadora
# -----------------------------------------------------------------------------
# A veces el missing es informativo: que el dato falte ya nos dice algo.
# En lugar de imputar (que oculta esa información), creamos una variable
# que marca qué países reportan y cuáles no. Después podemos incluirla
# como variable explicativa en un modelo.
# -----------------------------------------------------------------------------

paises_ind <- paises |>
  mutate(reporta_edu = !is.na(gasto_edu))

# ¿Hay diferencias en otras variables entre países que reportan y no reportan?
paises_ind |>
  group_by(reporta_edu) |>
  summarise(
    n               = n(),
    pib_pc_mediano  = round(median(pib_pc, na.rm = TRUE), 0),
    esperanza_med   = round(median(esperanza, na.rm = TRUE), 1),
    .groups         = "drop"
  )

# Si los países que NO reportan tienen menor PIB pc o menor esperanza,
# eso confirma el diagnóstico de MAR y respalda la idea de imputar por grupo.


# -----------------------------------------------------------------------------
# 4.6 Mantener: winsorización (recortar sin eliminar)
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


# =============================================================================
# 5. TRANSFORMACIONES DE VARIABLES
# =============================================================================
#
# Hasta acá decidimos QUÉ HACER con cada problema. Ahora preparamos las
# variables para que sean más manejables analíticamente. Tres
# transformaciones clásicas:
#   - Logaritmo:        cambia la FORMA (asimetría -> simetría).
#   - Estandarización:  cambia la ESCALA (media 0, desvío 1).
#   - Min-Max:          cambia la ESCALA (rango [0, 1]).
# =============================================================================


# -----------------------------------------------------------------------------
# 5.1 Transformación logarítmica
# -----------------------------------------------------------------------------
# Útil cuando la variable es positiva y tiene cola derecha larga.
# Muy común en economía: PIB, ingresos, ventas, precios.
# Ventaja extra: en regresión, los coeficientes se interpretan como
# elasticidades o cambios porcentuales.
# Cuidado:
#   - log(0) = -Inf -> si hay ceros, usar log(x + 1).
#   - log(negativo) no existe.
# -----------------------------------------------------------------------------

paises_log <- paises |>
  filter(!is.na(pib_pc), pib_pc > 0) |>
  mutate(log_pib = log(pib_pc))

# Comparación gráfica: original vs log.
paises_log |>
  pivot_longer(cols = c(pib_pc, log_pib),
               names_to = "version", values_to = "valor") |>
  mutate(version = ifelse(version == "pib_pc",
                          "Original (USD)",
                          "Logaritmo natural")) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ version, scales = "free") +
  labs(x = NULL, y = "Frecuencia",
       title = "Efecto del logaritmo sobre el PIB per cápita") +
  theme_minimal(base_size = 11)

# Conclusión visual: la versión en log es mucho más simétrica. Esa es la
# razón por la que en economía se trabaja casi siempre con log de ingresos
# o de PIB.


# -----------------------------------------------------------------------------
# 5.2 Estandarización (z-score)
# -----------------------------------------------------------------------------
# z_i = (x_i - media) / desvío. Resultado: media 0, desvío 1.
# Cuándo usarla:
#   - Comparar variables con distintas unidades (PIB en USD vs esperanza
#     de vida en años).
#   - Modelos donde la escala importa: clustering, PCA, regularización.
#   - Identificar valores extremos.
# Importante: NO cambia la forma de la distribución, solo la escala.
# -----------------------------------------------------------------------------

paises_std <- paises |>
  filter(!is.na(pib_pc), !is.na(esperanza)) |>
  mutate(
    pib_z       = (pib_pc    - mean(pib_pc))    / sd(pib_pc),
    esperanza_z = (esperanza - mean(esperanza)) / sd(esperanza)
  )

# También se puede usar la función scale() de base R, que hace lo mismo:
# paises$pib_z <- as.numeric(scale(paises$pib_pc))

# Verificamos: media ~ 0, desvío ~ 1.
paises_std |>
  summarise(
    media_pib_z   = round(mean(pib_z), 4),
    sd_pib_z      = round(sd(pib_z), 4),
    media_esp_z   = round(mean(esperanza_z), 4),
    sd_esp_z      = round(sd(esperanza_z), 4)
  )


# -----------------------------------------------------------------------------
# 5.3 Normalización min-max
# -----------------------------------------------------------------------------
# x' = (x - min) / (max - min). Resultado: rango [0, 1].
# Cuándo usarla:
#   - Cuando se necesita que todas las variables estén en el mismo rango.
#   - Algoritmos sensibles a escala (KNN, redes neuronales).
# Cuidado: es MUY sensible a outliers (un valor extremo comprime todo el
# resto contra el cero).
# -----------------------------------------------------------------------------

normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

paises_norm <- paises |>
  filter(!is.na(pib_pc), !is.na(esperanza)) |>
  mutate(
    pib_norm       = normalizar(pib_pc),
    esperanza_norm = normalizar(esperanza)
  )

# Verificación: el mínimo es 0, el máximo es 1.
paises_norm |>
  summarise(across(ends_with("_norm"),
                   list(min = min, max = max),
                   .names = "{.fn}_{.col}"))


# -----------------------------------------------------------------------------
# 5.4 Comparación visual de las cuatro versiones
# -----------------------------------------------------------------------------

paises |>
  filter(!is.na(pib_pc), pib_pc > 0) |>
  transmute(
    Original  = pib_pc,
    Log       = log(pib_pc),
    `Z-score` = (pib_pc - mean(pib_pc)) / sd(pib_pc),
    `Min-Max` = normalizar(pib_pc)
  ) |>
  pivot_longer(everything(),
               names_to  = "Transformación",
               values_to = "valor") |>
  mutate(Transformación = factor(Transformación,
                                 levels = c("Original", "Log", "Z-score", "Min-Max"))) |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ Transformación, scales = "free") +
  labs(x = NULL, y = NULL,
       title = "Cuatro versiones del PIB per cápita") +
  theme_minimal(base_size = 10)

# Idea clave para llevarse: el LOG cambia la FORMA (la asimetría desaparece).
# Z-score y Min-Max cambian la ESCALA pero conservan la asimetría original.


# =============================================================================
# 6. JUSTIFICACIÓN E IMPACTO: ANTES VS DESPUÉS
# =============================================================================
#
# Cualquier decisión de limpieza (eliminar, imputar, winsorizar, transformar)
# cambia los datos. Hay que MEDIR Y REPORTAR cuánto cambian las estadísticas
# clave para verificar que la decisión es razonable. Esto vale tanto para
# el TP como para cualquier informe profesional.
# =============================================================================


# -----------------------------------------------------------------------------
# 6.1 Función para comparar estadísticas descriptivas
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
# 6.2 Aplicación al PIB per cápita
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
# 6.3 Buena práctica para el TP
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
# 7. CIERRE Y PREGUNTAS PARA PENSAR
# =============================================================================
#
# Volvamos al principio: la calidad de los datos es responsabilidad del
# analista. NA, outlier, transformación: tres decisiones que parecen
# técnicas pero son sustantivas y afectan las conclusiones.
#
# Resumen del flujo de trabajo:
#   1. Diagnosticar (cuántos NAs, qué tipo, cuántos outliers).
#   2. Decidir (eliminar / imputar / mantener / winsorizar).
#   3. Transformar si hace falta (log, z-score, min-max).
#   4. Documentar y medir el impacto (antes vs después).
#
# -----------------------------------------------------------------------------
# Preguntas para pensar / hacer en casa:
# -----------------------------------------------------------------------------
#
#   1. Repitan el diagnóstico de la sección 2 con la variable gasto_salud
#      en lugar de gasto_edu, incluyendo el chi-cuadrado contra income y
#      el test t contra pib_pc (sección 2.4). ¿Rechazan MCAR? ¿El patrón
#      es más fuerte o más débil que con gasto_edu?
#
#   2. Apliquen winsorización a inflacion (con percentiles 5-95). ¿Cuánto
#      cambia la media mundial de inflación? ¿Tiene sentido winsorizar en
#      este caso, o se está borrando información económicamente relevante?
#
#   3. Comparen la regresión esperanza ~ pib_pc con la regresión
#      esperanza ~ log(pib_pc). ¿Cuál ajusta mejor (R^2)? ¿Cómo se
#      interpreta el coeficiente en cada caso?
#
#   4. Imputen gasto_edu por la mediana de la región (en lugar de income).
#      ¿Cambian las medias resultantes? Discutir cuál criterio de
#      agrupación es más razonable y por qué.
#
#   5. (Avanzado) En la sección 4.4 corrimos pool() sobre la regresión
#      esperanza ~ log(pib_pc) + gasto_salud + gasto_edu. Repitan el
#      ejercicio agregando inflacion como predictor. ¿Cambia el fmi
#      (fraction of missing information)? ¿Tiene sentido el resultado?
#
# -----------------------------------------------------------------------------
# Ideas clave para llevarse:
# -----------------------------------------------------------------------------
#
#   - Un NA no es un cero ni un vacío: es la AUSENCIA de información, y su
#     patrón importa (MCAR, MAR, MNAR).
#   - No hay regla automática para decidir qué hacer con outliers: hay que
#     mirar los datos y entender el contexto.
#   - Eliminar es la opción más cara: pierde información. Imputar y
#     winsorizar son alternativas que conservan el N.
#   - El LOG cambia la forma de la distribución; el z-score y el min-max
#     solo cambian la escala.
#   - Toda decisión de limpieza debe estar DOCUMENTADA y su impacto
#     MEDIDO. Esto aplica al TP y a cualquier trabajo profesional.
#
# =============================================================================