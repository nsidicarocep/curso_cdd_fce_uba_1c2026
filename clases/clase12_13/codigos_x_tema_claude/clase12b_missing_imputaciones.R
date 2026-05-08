# =============================================================================
# Ciencia de Datos para Economía y Negocios - FCE-UBA
# Clase 12b: Datos faltantes - diagnóstico e imputación
# -----------------------------------------------------------------------------
# Contenidos:
#   1. Detección y visualización de NAs
#   2. Tipos de missing: MCAR, MAR, MNAR
#   3. Pruebas formales: chi-cuadrado, t-test, test de Little
#   4. Tratamiento simple: eliminar, imputar por mediana global y por grupo
#   5. Imputación múltiple con {mice}: flujo descriptivo e inferencial
#   6. Mantener: variable indicadora de "missing informativo"
# -----------------------------------------------------------------------------
# Datos: indicadores del Banco Mundial (paquete {WDI}) para el año 2022.
#        Lo usamos porque tiene NAs reales: no todos los países reportan todos
#        los indicadores todos los años.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Preparación del entorno
# -----------------------------------------------------------------------------

library(tidyverse)   # dplyr, ggplot2, tidyr, etc.
library(WDI)         # indicadores del Banco Mundial
library(scales)      # para formatear ejes
library(mice)        # imputación múltiple

options(scipen = 999)
set.seed(42)


# =============================================================================
# 1. CARGA Y EXPLORACIÓN INICIAL
# =============================================================================
#
# Bajamos siete indicadores del Banco Mundial para todos los países en 2022.
# La elección no es casual: los elegimos justamente porque traen problemas
# de calidad de datos típicos de la vida real.
# =============================================================================


# -----------------------------------------------------------------------------
# 1.1 Descarga del dataset desde WDI
# -----------------------------------------------------------------------------

indicadores <- c(
  pib_pc       = "NY.GDP.PCAP.CD",        # PIB per cápita (USD corrientes)
  poblacion    = "SP.POP.TOTL",           # Población total
  esperanza    = "SP.DYN.LE00.IN",        # Esperanza de vida al nacer
  inflacion    = "FP.CPI.TOTL.ZG",        # Inflación anual (% IPC)
  gasto_edu    = "SE.XPD.TOTL.GD.ZS",     # Gasto público en educación (% PIB)
  gasto_salud  = "SH.XPD.CHEX.GD.ZS",     # Gasto en salud (% PIB)
  internet     = "IT.NET.USER.ZS"         # Usuarios de internet (% población)
)

wdi_raw <- WDI(
  country   = "all",
  indicator = indicadores,
  start     = 2022,
  end       = 2022,
  extra     = TRUE          # trae región e ingreso, útil para diagnosticar MAR
)

paises <- wdi_raw |>
  filter(region != "Aggregates") |>
  select(pais = country, iso = iso3c, region, income,
         pib_pc, poblacion, esperanza, inflacion,
         gasto_edu, gasto_salud, internet)

glimpse(paises)
nrow(paises)


# -----------------------------------------------------------------------------
# 1.2 Primera mirada
# -----------------------------------------------------------------------------

paises |>
  select(where(is.numeric)) |>
  summary()

# Lo primero que salta a la vista en summary() es que TODAS las variables
# numéricas tienen NAs (la última línea de cada columna dice "NA's : ...").
# Eso es típico cuando se trabaja con datos comparativos internacionales.


# =============================================================================
# 2. DETECCIÓN Y VISUALIZACIÓN DE FALTANTES
# =============================================================================
#
# Antes de tocar nada, lo primero es DIAGNOSTICAR: ¿cuántos faltan, dónde
# están, y por qué faltan? La decisión de qué hacer (eliminar, imputar,
# mantener) viene DESPUÉS del diagnóstico, no antes.
# =============================================================================


# -----------------------------------------------------------------------------
# 2.1 Cantidad y proporción de NAs por columna
# -----------------------------------------------------------------------------

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
# 2.2 Mapa de calor del patrón de faltantes
# -----------------------------------------------------------------------------

# Si hay filas con muchos NAs juntos, probablemente son países con poca
# capacidad estadística.

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


# =============================================================================
# 3. DIAGNÓSTICO DEL TIPO DE MISSING (MCAR / MAR / MNAR)
# =============================================================================
#
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
# =============================================================================


# -----------------------------------------------------------------------------
# 3.1 Hipótesis: ¿los NAs en gasto_edu se concentran por nivel de ingreso?
# -----------------------------------------------------------------------------

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


# =============================================================================
# 4. PRUEBAS FORMALES: ¿PODEMOS RECHAZAR MCAR?
# =============================================================================
#
# La sección anterior fue exploratoria. Ahora formalizamos la intuición con
# TESTS DE HIPÓTESIS, los mismos que vieron en la clase 9.
#
# La idea es simple: creamos una variable indicadora de missingness y la
# testeamos contra las otras variables del dataset.
#
#   H0: la probabilidad de que falte el dato es independiente de la otra
#       variable -> compatible con MCAR.
#   H1: hay asociación -> evidencia de MAR (o MNAR).
#
# Tres herramientas según el tipo de variable:
#   A) Chi-cuadrado:        missingness vs variable categórica.
#   B) Test t (Welch):      missingness vs variable numérica.
#   C) Test de Little:      test ómnibus sobre todas las variables a la vez.
# =============================================================================


# -----------------------------------------------------------------------------
# 4.1 Chi-cuadrado: ¿la falta de gasto_edu depende del nivel de ingreso?
# -----------------------------------------------------------------------------

# Paso 1: indicadora de missingness.
paises_test <- paises |>
  mutate(falta_edu = is.na(gasto_edu))

# Paso 2: tabla de contingencia entre missingness e income.
tabla_inc <- table(paises_test$income, paises_test$falta_edu)
tabla_inc

# Paso 3: el test.
chi_inc <- chisq.test(tabla_inc)
chi_inc

# Si p-value < 0.05 -> rechazamos H0 -> la falta NO es independiente del
# nivel de ingreso -> NO es MCAR -> hay evidencia de MAR.

# Lo mismo contra la región:
chi_reg <- chisq.test(table(paises_test$region, paises_test$falta_edu))
chi_reg


# -----------------------------------------------------------------------------
# 4.2 Test t: ¿la falta de gasto_edu depende del PIB per cápita?
# -----------------------------------------------------------------------------

# Comparamos la media de PIB pc entre el grupo "tiene NA en gasto_edu" y el
# grupo "tiene dato". Si las medias son distintas, la falta NO es
# independiente del PIB pc.

t_pib <- t.test(pib_pc ~ falta_edu, data = paises_test)
t_pib

# Foto descriptiva previa:
paises_test |>
  group_by(falta_edu) |>
  summarise(n            = n(),
            media_pib    = round(mean(pib_pc, na.rm = TRUE), 0),
            mediana_pib  = round(median(pib_pc, na.rm = TRUE), 0),
            .groups      = "drop")

# Si los países con datos faltantes en educación tienen sistemáticamente
# menor PIB pc, la asociación es clara: el missingness depende del nivel
# de desarrollo (variable observable) -> MAR.


# -----------------------------------------------------------------------------
# 4.3 Test ómnibus de Little (paquete {naniar})
# -----------------------------------------------------------------------------
# Lo anterior chequea de a una variable por vez. El test de Little (1988)
# es un test CONJUNTO: testea si los patrones de NAs en TODO el dataset
# son consistentes con MCAR.
#
#   H0: todos los datos son MCAR.
#   H1: al menos una variable no es MCAR.
#
# Si no tienen el paquete: install.packages("naniar")
# -----------------------------------------------------------------------------

library(naniar)

paises |>
  select(pib_pc, poblacion, esperanza, inflacion,
         gasto_edu, gasto_salud, internet) |>
  mcar_test()


# -----------------------------------------------------------------------------
# 4.4 Aclaraciones sobre los tests
# -----------------------------------------------------------------------------
#
#   1. No rechazar H0 NO prueba que sea MCAR. Solo dice que no encontramos
#      evidencia en contra. Es la asimetría clásica de los tests.
#
#   2. Estos tests distinguen MCAR de MAR, pero NO distinguen MAR de MNAR.
#      Para MNAR hay que recurrir a ARGUMENTOS DE DOMINIO (ej: "sabemos que
#      los países con alta inflación dejan de reportar") o a modelos de
#      selección (Heckman) que asumen una estructura paramétrica.
#
#   3. Como siempre con tests, p-value depende del N. Con 200+ países,
#      diferencias chicas pueden dar p < 0.05. Combinar el test con el
#      tamaño del efecto es buena práctica.


# =============================================================================
# 5. ELIMINAR
# =============================================================================
#
# Cuándo es razonable eliminar:
#   - NAs MCAR y poco frecuentes (< 5 %).
#   - Hay datos suficientes y no se altera la representatividad.
# =============================================================================

# Eliminar filas con NA en una variable específica:
paises_completos <- paises |>
  filter(!is.na(pib_pc), !is.na(esperanza))

nrow(paises) - nrow(paises_completos)   # cuántas filas perdimos

# Si descartamos por TODAS las variables el N cae mucho. Probémoslo:
paises |>
  drop_na() |>
  nrow()

# La diferencia entre "completar caso a caso" (descartar filas con NA en la
# variable que importa) y "completar conjunto completo" (drop_na global)
# suele ser grande. Documentar SIEMPRE qué versión se usó.


# =============================================================================
# 6. IMPUTACIÓN SIMPLE
# =============================================================================


# -----------------------------------------------------------------------------
# 6.1 Por la mediana global
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
# 6.2 Por la mediana del grupo (mejor)
# -----------------------------------------------------------------------------
# Si diagnosticamos MAR (la falta depende del nivel de ingreso o región),
# imputar usando la mediana DEL MISMO GRUPO captura mejor las diferencias.
# Es una imputación "informada" por una variable observada.
# -----------------------------------------------------------------------------

paises_imp_grupo <- paises |>
  group_by(region) |>
  mutate(gasto_edu_imp = ifelse(is.na(gasto_edu),
                                median(gasto_edu, na.rm = TRUE),
                                gasto_edu)) |>
  ungroup()

# Comparemos: si imputamos por grupo, los países low-income reciben un
# valor distinto al que reciben los high-income. Eso preserva la
# heterogeneidad estructural.

paises_imp_grupo |>
  group_by(region) |>
  summarise(
    n          = n(),
    n_imputado = sum(is.na(gasto_edu)),
    media_sin_imp = round(mean(gasto_edu, na.rm = TRUE), 2),
    media_imp  = round(mean(gasto_edu_imp, na.rm = TRUE), 2),
    .groups    = "drop"
  )

# Regla útil: si imputamos más del 20-30 % de una variable, conviene
# preguntarse si esa variable es realmente confiable para el análisis.


# =============================================================================
# 7. IMPUTACIÓN MÚLTIPLE CON {mice}
# =============================================================================
#
# La imputación simple "inventa" un valor puntual y subestima la incertidumbre.
# La imputación múltiple genera m versiones del dataset, corre el análisis en
# cada una y combina los resultados (reglas de Rubin) para obtener IC que
# reflejen honestamente la incertidumbre de la imputación.
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
# =============================================================================


# -----------------------------------------------------------------------------
# 7.1 Paso 0: preparar el dataset
# -----------------------------------------------------------------------------
# Importante: NO incluir variables tipo identificador (pais, iso) ni cosas
# que no queremos que se usen como predictoras. mice usa TODAS las columnas
# del data frame como potenciales predictoras de las imputaciones.
# -----------------------------------------------------------------------------

datos_imp <- paises |>
  select(pib_pc, esperanza, inflacion,
         gasto_edu, gasto_salud, internet,
         income, region)
# Dejamos income y region porque ayudan a imputar mejor (recordemos que
# diagnosticamos MAR contra ellas). mice maneja categóricas automáticamente
# con métodos distintos (logreg, polyreg).


# -----------------------------------------------------------------------------
# 7.2 Paso 1: inspeccionar los patrones de NAs
# -----------------------------------------------------------------------------
# md.pattern() muestra qué COMBINACIONES de NAs aparecen en el dataset.
# Cada fila es un patrón; los 1 son datos presentes y los 0 son faltantes.
# -----------------------------------------------------------------------------

md.pattern(datos_imp, rotate.names = TRUE)

# Lo más útil: ver si hay variables que SIEMPRE faltan juntas. Si dos
# variables tienen exactamente el mismo patrón de NAs, una no puede imputar
# a la otra (no hay información cruzada).


# -----------------------------------------------------------------------------
# 7.3 Paso 2: correr la imputación
# -----------------------------------------------------------------------------
# Argumentos clave:
#   m       : cantidad de imputaciones (default = 5; usar 10-20 si hay
#             muchos NAs o si el análisis es importante).
#   method  : método por variable. Si lo dejamos vacío, mice elige según
#             el tipo: "pmm" para numéricas, "logreg" para binarias,
#             "polyreg" para categóricas con varios niveles.
#   seed    : semilla para reproducibilidad.
#   maxit   : iteraciones del algoritmo (default = 5).
# -----------------------------------------------------------------------------

imp <- mice(datos_imp, m = 5, method = "pmm", seed = 42, printFlag = FALSE)

# pmm = "predictive mean matching": para cada NA, mice predice un valor
# con regresión y después busca el valor REAL más cercano en los datos
# observados, y lo usa como imputación. Ventaja: nunca imputa valores
# imposibles (negativos, fuera de rango).


# -----------------------------------------------------------------------------
# 7.4 Paso 3: inspeccionar el objeto mice
# -----------------------------------------------------------------------------

summary(imp)            # resumen general
imp$method              # qué método usó para cada variable

# Los valores imputados están en imp$imp$<variable>. Es una matriz con
# m columnas (una por imputación) y una fila por cada NA original.
head(imp$imp$gasto_edu)

# Si las cinco versiones son MUY parecidas, la imputación es "estable".
# Si difieren mucho, la incertidumbre es alta y se va a reflejar en
# intervalos más anchos al final.


# -----------------------------------------------------------------------------
# 7.5 Paso 4: chequeo visual (¿las imputaciones son razonables?)
# -----------------------------------------------------------------------------
# stripplot() y densityplot() comparan distribuciones de valores
# observados (azul) vs imputados (rojo). Lo que queremos ver: que se
# parezcan. Si los rojos están todos del mismo lado, mice está
# extrapolando hacia una zona poco poblada y conviene revisar.
# -----------------------------------------------------------------------------

stripplot(imp, gasto_edu ~ .imp, pch = 20, cex = 1.2)
densityplot(imp, ~ gasto_edu)


# -----------------------------------------------------------------------------
# 7.6 FLUJO 1: extraer un dataset completo para análisis descriptivo
# -----------------------------------------------------------------------------

# Opción A: usar UNA imputación específica (típicamente la primera).
datos_completos <- complete(imp, action = 1)

nrow(datos_completos)
sum(is.na(datos_completos$gasto_edu))   # debería ser 0

# Opción B: apilar las m imputaciones en formato "long".
datos_long <- complete(imp, action = "long")

datos_long |>
  group_by(.imp) |>
  summarise(media_edu = round(mean(gasto_edu), 2))
# Cada imputación da una media ligeramente distinta -> esa diferencia
# ES la incertidumbre que la imputación simple ocultaba.

# Opción C: promediar las m imputaciones para tener un único valor por NA.
# OJO: esto subestima la incertidumbre. Usar SOLO si lo único que queremos
# es un valor "central" para graficar o describir.
datos_promedio <- complete(imp, action = "long") |>
  group_by(.id) |>
  summarise(across(where(is.numeric), mean), .groups = "drop")


# -----------------------------------------------------------------------------
# 7.7 Mergear los imputados al dataset ORIGINAL (paises)
# -----------------------------------------------------------------------------
# complete() devuelve solo las columnas que le pasamos a mice. Si queremos
# conservar pais e iso del dataset original, hay que mergear. Como complete()
# preserva el orden y la cantidad de filas del input, podemos hacerlo de
# forma directa.
# -----------------------------------------------------------------------------

datos_completos <- complete(imp, action = 1)

# Antes de pisar las columnas, MARCAMOS qué valores eran NA en el original.
# Esto deja documentado dentro del propio dataset qué fue dato observado y
# qué fue imputado.

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

# Verificaciones:
head(paises_imp |> select(pais, iso, region, pib_pc, gasto_edu))

paises_imp |>
  summarise(across(c(pib_pc, esperanza, inflacion,
                     gasto_edu, gasto_salud, internet),
                   ~ sum(is.na(.))))

paises_imp |>
  summarise(across(ends_with("_imp_flag"), sum))


# Comparar la distribución de valores observados vs imputados:
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


# -----------------------------------------------------------------------------
# 7.8 Variante: dataset original con las m imputaciones apiladas
# -----------------------------------------------------------------------------

paises_imp_long <- complete(imp, action = "long", include = TRUE) |>
  as_tibble() |>
  mutate(.id = as.integer(.id)) |>
  left_join(
    paises |> mutate(.id = row_number()) |> select(.id, pais, iso),
    by = ".id"
  )

paises_imp_long |>
  count(.imp)

# Las cinco imputaciones para Argentina:
paises_imp_long |>
  filter(pais == "Argentina") |>
  select(.imp, pais, gasto_edu, gasto_salud)


# -----------------------------------------------------------------------------
# 7.9 FLUJO 2: análisis inferencial con reglas de Rubin (with + pool)
# -----------------------------------------------------------------------------
#
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
# -----------------------------------------------------------------------------

# Paso 1: ajustar el modelo en cada uno de los m datasets imputados.
fit_imp <- with(imp, lm(esperanza ~ log(pib_pc) + gasto_salud + gasto_edu))

# Paso 2: combinar resultados (reglas de Rubin).
resultado_pool <- pool(fit_imp)
summary(resultado_pool, conf.int = TRUE)

# La salida tiene los coeficientes habituales más:
#   - fmi  : fraction of missing information.
#   - lambda: proporción de la varianza atribuible a los NAs.


# -----------------------------------------------------------------------------
# 7.10 Comparación: imputación simple vs múltiple
# -----------------------------------------------------------------------------

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

# (b) Imputación múltiple:
summary(resultado_pool, conf.int = TRUE) |>
  filter(term == "log(pib_pc)") |>
  select(estimate, `2.5 %`, `97.5 %`)

# Lo esperable: el IC de la imputación múltiple es MÁS ANCHO. Esa diferencia
# en ancho es exactamente la incertidumbre que la imputación simple oculta.


# -----------------------------------------------------------------------------
# 7.11 Cuándo vale la pena la imputación múltiple
# -----------------------------------------------------------------------------
#
#   - NAs MAR con más del 5-10 % de los datos.
#   - Análisis inferencial: tests, IC, regresiones que se reportan.
#   - Trabajo profesional o académico publicable.
#
# Para descripción simple (un boxplot, una tabla resumen), la imputación
# por grupo de la sección 6.2 alcanza. Lo importante es DOCUMENTAR qué
# método se usó y por qué.
#
# Limitación recordatoria: mice asume MAR. Si el missing es MNAR, ningún
# método de imputación basado solo en variables observadas lo arregla.


# =============================================================================
# 8. MANTENER: VARIABLE INDICADORA
# =============================================================================
#
# A veces el missing es informativo: que el dato falte ya nos dice algo.
# En lugar de imputar (que oculta esa información), creamos una variable
# que marca qué países reportan y cuáles no. Después podemos incluirla
# como variable explicativa en un modelo.
# =============================================================================

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


# =============================================================================
# 9. CIERRE Y PREGUNTAS PARA PENSAR
# =============================================================================
#
# Resumen del flujo de trabajo:
#   1. Diagnosticar (cuántos NAs, qué tipo).
#   2. Decidir (eliminar / imputar simple / imputar múltiple / mantener).
#   3. Documentar y medir el impacto (antes vs después).
#
# -----------------------------------------------------------------------------
# Preguntas para pensar / hacer en casa:
# -----------------------------------------------------------------------------
#
#   1. Repitan el diagnóstico de la sección 3 con la variable gasto_salud
#      en lugar de gasto_edu, incluyendo el chi-cuadrado contra income y
#      el test t contra pib_pc. ¿Rechazan MCAR?
#
#   2. Imputen gasto_edu por la mediana de la región (en lugar de income).
#      ¿Cambian las medias resultantes? Discutir cuál criterio de
#      agrupación es más razonable y por qué.
#
#   3. (Avanzado) En la sección 7.9 corrimos pool() sobre la regresión
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
#   - Eliminar es la opción más cara: pierde información. Imputar
#     conserva el N pero requiere supuestos.
#   - La imputación simple "inventa" un valor único; la múltiple genera m
#     versiones y combina los resultados, propagando la incertidumbre.
#   - mice asume MAR. Si el missing es MNAR, ningún método basado solo en
#     variables observadas lo arregla.
#
# =============================================================================
