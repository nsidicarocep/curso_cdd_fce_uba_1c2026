# =============================================================================
# Clase 9 - Práctica: Tests de hipótesis en R
# Ciencia de Datos para Economía y Negocios | FCE-UBA
# -----------------------------------------------------------------------------
# Contenidos:
#   1) t-test: diferencia de ingreso total promedio varones vs mujeres
#   2) t-test: diferencia de ingreso HORARIO promedio varones vs mujeres
#   3) Versiones ponderadas de (1) y (2) con el paquete {survey}
#   4) Test pareado: ¿cambió la esperanza de vida en los últimos 10 años?
#   5) ANOVA: ¿la región del mundo afecta el PBI per cápita?
#   6) Chi-cuadrado: ¿hay asociación entre género y nivel educativo?
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Paquetes y opciones
# -----------------------------------------------------------------------------
# {tidyverse} para manipulación, {survey} para inferencia con ponderadores,
# {WDI} para descargar los indicadores del Banco Mundial.
library(tidyverse)
library(survey)
library(WDI)
library(eph)
options(scipen = 999)  # evita notación científica en los prints


# =============================================================================
# BLOQUE A: EPH - preparación de la base
# =============================================================================
# Trabajamos con asalariados registrados del sector privado, con ingreso de
# ocupación principal (P21) positivo y horas de trabajo razonables.
# P47T es el ingreso TOTAL individual (incluye no laborales). Para los puntos
# 1 y 2 usamos P47T (total) e ing_horario (P21 / horas) respectivamente.
# -----------------------------------------------------------------------------

# Cargamos la EPH individual del T3 2025
eph <- get_microdata(year=2025,period = 3,'individual')
# Filtros y variables derivadas
# - ESTADO == 1     : ocupados
# - CAT_OCUP == 3   : asalariados
# - P21 > 0         : ingreso laboral positivo
# - PP3E_TOT        : horas semanales en la ocupación principal
# - CH04            : género (1 = varón, 2 = mujer)
# - P47T            : ingreso total individual (mensual)
base <- eph %>%
  filter(ESTADO == 1, CAT_OCUP == 3,
         !is.na(P21), P21 > 0,
         !is.na(PP3E_TOT), PP3E_TOT > 0, PP3E_TOT <= 84,
         !is.na(CH04),
         !is.na(P47T), P47T > 0) %>%
  mutate(
    genero      = factor(CH04, levels = c(1, 2),
                         labels = c("Varón", "Mujer")),
    horas_mes   = PP3E_TOT * 4.33,        # de semanales a mensuales
    ing_horario = P21 / horas_mes,        # ingreso laboral por hora
    ing_total   = P47T                    # ingreso total mensual
  ) %>%
  filter(ing_horario > 0)

# Revisamos tamaños muestrales por grupo
base %>% count(genero)


# =============================================================================
# 1) Diferencia de medias: ingreso TOTAL de varones vs mujeres
# =============================================================================
# H0: mu_varones = mu_mujeres     (no hay brecha en la población)
# H1: mu_varones != mu_mujeres    (bilateral; hay brecha, en cualquier sentido)
#
# Usamos Welch t-test (var.equal = FALSE, default en R): no asume varianzas
# iguales entre grupos. Por TCL, con n grande la media muestral es ~ Normal,
# así que el estadístico t es válido aunque la distribución de ingresos sea
# asimétrica.
# -----------------------------------------------------------------------------

# Medias y desvíos por grupo (foto descriptiva previa al test)
base %>%
  group_by(genero) %>%
  summarise(n        = n(),
            media    = mean(ing_total),
            sd       = sd(ing_total),
            mediana  = median(ing_total))

# Test t de dos muestras (Welch)
t_total <- t.test(ing_total ~ genero, data = base)
t_total

# Interpretación de los elementos clave del output:
# - t, df         : estadístico de prueba y grados de libertad (Welch ajusta df)
# - p-value       : probabilidad de observar una diferencia al menos tan grande
#                   como la muestral si H0 fuese cierta
# - conf.int      : IC 95% para la diferencia de medias (Varón - Mujer)
# - sample estimates: medias muestrales por grupo
#
# Regla de decisión: si p-value < 0.05 rechazamos H0 al 5%.


# =============================================================================
# 2) Diferencia de medias: ingreso HORARIO de varones vs mujeres
# =============================================================================
# Mismo esquema que (1), pero con ing_horario. Esto "limpia" el efecto de la
# cantidad de horas trabajadas: compara cuánto se paga cada hora de trabajo,
# no cuánto se cobra a fin de mes.
#
# H0: mu_varones = mu_mujeres  (en ingreso por hora)
# H1: mu_varones != mu_mujeres
# -----------------------------------------------------------------------------

# Descriptivos por grupo
base %>%
  group_by(genero) %>%
  summarise(n        = n(),
            media_h  = mean(ing_horario),
            sd_h     = sd(ing_horario),
            mediana_h= median(ing_horario))

# Test t (Welch) sobre ingreso horario
t_horario <- t.test(ing_horario ~ genero, data = base)
t_horario

# Interpretación: la comparación con el test (1) permite ver cuánto de la
# brecha en ingreso total viene por diferencia horaria y cuánto por diferencias
# en horas trabajadas.


# =============================================================================
# 3) Tests ponderados con {survey}
# =============================================================================
# La EPH es una muestra con diseño complejo: cada persona tiene un ponderador
# (PONDERA = cuántas personas de la población representa). Ignorar el
# ponderador sesga tanto el punto estimado como los errores estándar.
#
# Pasos:
#   (a) Definir el objeto de diseño con svydesign() indicando los pesos.
#   (b) Usar svyttest() en lugar de t.test().
#
# Nota práctica: svydesign admite también estratos y conglomerados. Para la
# EPH los usuarios avanzados usan CODUSU/NRO_HOGAR como PSU y aglomerado como
# estrato. Acá trabajamos con un diseño simplificado (solo pesos) para que la
# comparación con los tests no ponderados sea directa.
# -----------------------------------------------------------------------------

# Base para el diseño: agregamos PONDERA (ponderador individual)
base_svy <- eph %>%
  filter(ESTADO == 1, CAT_OCUP == 3,
         !is.na(P21), P21 > 0,
         !is.na(PP3E_TOT), PP3E_TOT > 0, PP3E_TOT <= 84,
         !is.na(CH04),
         !is.na(P47T), P47T > 0,
         !is.na(PONDERA), PONDERA > 0) %>%
  mutate(
    genero      = factor(CH04, levels = c(1, 2),
                         labels = c("Varón", "Mujer")),
    horas_mes   = PP3E_TOT * 4.33,
    ing_horario = P21 / horas_mes,
    ing_total   = P47T
  ) %>%
  filter(ing_horario > 0)

# Definimos el objeto de diseño muestral:
#   ids = ~1  -> no declaramos clusters (diseño simple con pesos)
#   weights = ~PONDERA -> el ponderador de la EPH
#   data = base_svy
dis_eph <- svydesign(ids = ~1, weights = ~PONDERA, data = base_svy)

# --- 3.a) Ingreso total, ponderado -------------------------------------------
# Medias ponderadas por grupo (para contrastar con las no ponderadas)
svyby(~ing_total, ~genero, design = dis_eph, FUN = svymean)

# Test t ponderado. La sintaxis es la misma que t.test pero con design.
t_total_w <- svyttest(ing_total ~ genero, design = dis_eph)
t_total_w

# --- 3.b) Ingreso horario, ponderado -----------------------------------------
svyby(~ing_horario, ~genero, design = dis_eph, FUN = svymean)

t_horario_w <- svyttest(ing_horario ~ genero, design = dis_eph)
t_horario_w

# Ejercicio---
# comparar el p-valor y el IC de las versiones ponderadas con las no
# ponderadas. ¿Que sucede?


# =============================================================================
# BLOQUE B: WDI - preparación de la base de países
# =============================================================================
# Vamos a usar dos indicadores del Banco Mundial:
#   - SP.DYN.LE00.IN : esperanza de vida al nacer (años)
#   - NY.GDP.PCAP.KD : PBI per cápita a precios constantes de 2015 (USD)
# Pedimos un rango de años amplio y después elegimos los años que queremos.
# -----------------------------------------------------------------------------

# Descarga
wdi_raw <- WDI(country   = "all",
               indicator = c("esp_vida" = "SP.DYN.LE00.IN",
                             "pbi_pc"   = "NY.GDP.PCAP.KD"),
               start     = 2013, end = 2023,
               extra     = TRUE)

# Nos quedamos con países reales (no agregados regionales del BM)
wdi_paises <- wdi_raw %>%
  filter(region != "Aggregates")


# =============================================================================
# 4) Test pareado: ¿cambió la esperanza de vida en los últimos 10 años?
# =============================================================================
# Planteo: comparamos esperanza de vida del país i en t vs t-10. Cada país
# aparece DOS veces (una por año), y las observaciones NO son independientes
# entre sí (Japón en 2013 y Japón en 2023 están correlacionados). Por eso
# corresponde un test PAREADO (paired = TRUE): equivalente a un t de una
# muestra sobre las diferencias d_i = X_{i, t} - X_{i, t-10}.
#
# H0: media de las diferencias = 0   (no hubo cambio en promedio)
# H1: media de las diferencias != 0  (sí hubo cambio)
#
# Elegimos 2013 y 2023 como extremos del período (10 años completos).
# -----------------------------------------------------------------------------

# Armamos la base ancha: una fila por país, columnas para cada año
esp_vida <- wdi_paises %>%
  filter(year %in% c(2013, 2023), !is.na(esp_vida)) %>%
  select(iso3c, country, region, year, esp_vida) %>%
  pivot_wider(names_from = year, values_from = esp_vida,
              names_prefix = "ev_") %>%
  filter(!is.na(ev_2013), !is.na(ev_2023)) %>%   # países con ambos años
  mutate(diff = ev_2023 - ev_2013)

# Descriptivos de la diferencia (es sobre esto que corre el test pareado)
esp_vida %>%
  summarise(n           = n(),
            media_diff  = mean(diff),
            sd_diff     = sd(diff),
            min_diff    = min(diff),
            max_diff    = max(diff))

# Test pareado. Dos formas equivalentes:
# (a) Pasando los dos vectores con paired = TRUE
t_pareado <- t.test(esp_vida$ev_2023, esp_vida$ev_2013, paired = TRUE)
t_pareado

# (b) Equivalentemente, un t de una muestra sobre las diferencias
t.test(esp_vida$diff, mu = 0)

# Interpretación:
# - mean of the differences: cambio promedio en años de esperanza de vida.
# - p-value: probabilidad de observar un cambio al menos tan grande si H0
#   (no hubo cambio) fuese cierta.
# - conf.int: IC 95% para el cambio promedio de esperanza de vida por país.

# =============================================================================
# 5) ANOVA: ¿la región afecta al PBI per cápita?
# =============================================================================
# Comparamos medias de PBI per cápita entre las regiones definidas por el
# Banco Mundial. Con más de dos grupos NO usamos t-test (inflaría el error
# tipo I por múltiples comparaciones): usamos ANOVA de un factor.
#
# H0: mu_region1 = mu_region2 = ... = mu_regionK  (todas iguales)
# H1: al menos una media regional difiere del resto
#
# El estadístico F compara varianza ENTRE grupos vs DENTRO de grupos:
#   F grande -> los grupos son muy distintos entre sí respecto de su
#               dispersión interna -> tiende a rechazar H0.
#
# Elegimos el último año disponible (2023) para tener una foto transversal.
# -----------------------------------------------------------------------------

# Base para ANOVA: un país por fila, en 2023, con PBI per cápita y región
df_anova <- wdi_paises %>%
  filter(year == 2023, !is.na(pbi_pc), !is.na(region)) %>%
  select(iso3c, country, region, pbi_pc) %>%
  mutate(region = factor(region))

# Descriptivos por región (foto previa)
df_anova %>%
  group_by(region) %>%
  summarise(n     = n(),
            media = mean(pbi_pc),
            sd    = sd(pbi_pc),
            min   = min(pbi_pc),
            max   = max(pbi_pc)) %>%
  arrange(desc(media))

# Ajuste del ANOVA
modelo_anova <- aov(pbi_pc ~ region, data = df_anova)
summary(modelo_anova)

# Interpretación del summary():
# - Df              : grados de libertad (entre = k-1, residuales = n-k)
# - Sum Sq, Mean Sq : sumas y cuadrados medios
# - F value         : estadístico F
# - Pr(>F)          : p-valor. Si < 0.05 rechazamos H0 al 5%.
#
# Si rechazamos H0, sabemos que HAY diferencias, pero no ENTRE CUÁLES regiones.
# Para eso hacemos comparaciones múltiples post-hoc con Tukey HSD, que
# controla el error tipo I familywise.
tukey <- TukeyHSD(modelo_anova)
tukey

# Cada fila del Tukey muestra:
# - diff   : diferencia de medias entre dos regiones
# - lwr/upr: IC 95% ajustado para esa diferencia
# - p adj  : p-valor ajustado por múltiples comparaciones
# Si el IC no contiene 0 (o p adj < 0.05), esas dos regiones difieren
# significativamente.

# Chequeo de supuestos:
# - Homocedasticidad: ANOVA clásico asume varianzas iguales entre grupos.
#   Si las varianzas son muy distintas (mirar los sd por grupo), conviene
#   oneway.test(pbi_pc ~ region, data = df_anova) que es la versión Welch
#   y no asume igualdad de varianzas.
oneway.test(pbi_pc ~ region, data = df_anova)   # Welch-ANOVA
# Para Tukey
library(rstatix)
df_anova %>%
  games_howell_test(pbi_pc ~ region)


# =============================================================================
# 6) Test Chi-cuadrado de independencia: variables categóricas
# =============================================================================
# Todos los tests anteriores (t, ANOVA) comparan MEDIAS de una variable
# numérica entre grupos. Pero a veces queremos testear si dos variables
# CATEGÓRICAS están asociadas entre sí, sin que haya una variable numérica
# de por medio. Para eso usamos el test chi-cuadrado de independencia.
#
# Pregunta: ¿Hay asociación entre el GÉNERO y el NIVEL EDUCATIVO entre los
# asalariados argentinos? ¿O son independientes?
#
# H0: las variables género y nivel educativo son INDEPENDIENTES
#     (la distribución de nivel educativo es la misma para varones y mujeres)
# H1: las variables NO son independientes (hay asociación)
#
# Intuición del estadístico:
#   Chi² = Σ (observado - esperado)² / esperado
#   donde "esperado" es lo que veríamos si las variables fueran independientes.
#   Si Chi² es grande, las frecuencias observadas difieren mucho de las
#   esperadas bajo independencia -> rechazamos H0.
#
# Supuestos:
#   - Observaciones independientes (cada fila es un individuo distinto).
#   - Frecuencias esperadas suficientemente grandes (regla práctica: todas >= 5).
#     Si alguna celda tiene frecuencia esperada < 5, R emite un warning.
#     En ese caso conviene agrupar categorías o usar test exacto de Fisher.
# -----------------------------------------------------------------------------

# --- 6.a) Preparación: creamos la variable de nivel educativo ----------------
# NIVEL_ED en la EPH codifica:
#   1 = Sin instrucción / Primaria incompleta
#   2 = Primaria completa
#   3 = Secundaria incompleta
#   4 = Secundaria completa
#   5 = Superior / Universitaria incompleta
#   6 = Superior / Universitaria completa
#   7 = Sin información / Ns-Nr
#   9 = Sin información (otra codificación según versión)

base_chi <- base %>%
  filter(NIVEL_ED %in% 1:6) %>%       # excluimos sin información
  mutate(
    nivel_ed = factor(NIVEL_ED, levels = 1:6,
                      labels = c("< Primaria",
                                 "Primaria",
                                 "Sec. inc.",
                                 "Sec. comp.",
                                 "Sup. inc.",
                                 "Sup. comp."))
  )

# --- 6.b) Tabla de contingencia (frecuencias observadas) ---------------------
# Es la base de todo test chi-cuadrado: una tabla cruzada de las dos variables.
tabla <- table(base_chi$genero, base_chi$nivel_ed)
tabla

# Agregamos totales marginales para la lectura
addmargins(tabla)

# Proporciones por fila (distribución de nivel educativo DENTRO de cada género)
# Esto nos da una primera idea visual: si las filas se parecen, las variables
# podrían ser independientes; si difieren, podría haber asociación.
prop.table(tabla, margin = 1) %>% round(3)

# --- 6.c) Test chi-cuadrado de Pearson ---------------------------------------
# chisq.test() calcula el estadístico Chi², los grados de libertad
# ((filas-1)*(columnas-1)) y el p-valor asociado.

test_chi <- chisq.test(tabla)
test_chi

# Interpretación del output:
# - X-squared : valor del estadístico Chi² de Pearson
# - df        : grados de libertad = (nfilas - 1) * (ncols - 1)
#               Con 2 géneros y 6 niveles: df = (2-1)*(6-1) = 5
# - p-value   : probabilidad de observar un Chi² al menos tan grande si H0
#               (independencia) fuese cierta
#
# Regla de decisión: si p-value < 0.05 rechazamos H0 al 5% -> hay evidencia
# de asociación entre género y nivel educativo.

# --- 6.d) Inspección de las frecuencias esperadas ---------------------------
# Recordamos: el test necesita que las frecuencias esperadas sean >= 5 en
# todas las celdas. chisq.test() las guarda en $expected.
test_chi$expected %>% round(1)

# Si alguna fuera < 5, podríamos:
#   (a) Colapsar categorías poco frecuentes (ej: juntar "< Primaria" con "Primaria")
#   (b) Usar test exacto de Fisher: fisher.test(tabla, simulate.p.value = TRUE)

# --- 6.e) Residuos estandarizados (¿dónde está la asociación?) --------------
# El test chi-cuadrado solo dice SI hay asociación, no DÓNDE. Los residuos de
# Pearson estandarizados nos dicen qué celdas contribuyen más al rechazo:
#   residuo > 0 : hay MÁS casos de los esperados bajo independencia
#   residuo < 0 : hay MENOS casos de los esperados
#   abs(residuo) > 2 : la celda contribuye significativamente al Chi²

test_chi$residuals %>% round(2)

# Ejemplo de lectura: si el residuo de (Mujer, Sup. comp.) es positivo y
# grande, significa que hay más mujeres con educación superior completa de
# las que esperaríamos si género y educación fueran independientes.

# --- 6.f) Medida de asociación: V de Cramér ---------------------------------
# El Chi² depende del tamaño de muestra (con n grande, casi todo es
# significativo). La V de Cramér normaliza el Chi² para medir la INTENSIDAD
# de la asociación en una escala de 0 a 1:
#   V = sqrt(Chi² / (n * (min(filas, cols) - 1)))
#   0 = independencia perfecta
#   1 = asociación perfecta
#   Regla empírica: V < 0.1 débil, 0.1-0.3 moderada, > 0.3 fuerte

n_total <- sum(tabla)
k       <- min(nrow(tabla), ncol(tabla))
V_cramer <- sqrt(test_chi$statistic / (n_total * (k - 1)))
names(V_cramer) <- "V de Cramér"
V_cramer

# --- 6.g) Ejemplo con datos del Banco Mundial (WDI) -------------------------
# Para practicar con otra fuente: ¿hay asociación entre la REGIÓN del mundo
# y el NIVEL DE INGRESO (income) del país?
# Ambas son categóricas -> test chi-cuadrado.

df_chi_wdi <- wdi_paises %>%
  filter(year == 2023,
         !is.na(region), !is.na(income),
         income != "Not classified") %>%
  mutate(region = factor(region),
         income = factor(income))

tabla_wdi <- table(df_chi_wdi$region, df_chi_wdi$income)
tabla_wdi

test_chi_wdi <- chisq.test(tabla_wdi, simulate.p.value = TRUE, B = 5000)
test_chi_wdi
# Nota: usamos simulate.p.value = TRUE porque algunas celdas pueden tener
# frecuencia esperada < 5. La simulación (Monte Carlo con B réplicas) da un
# p-valor válido sin depender de la aproximación asintótica.

# Residuos: ¿qué combinaciones región-ingreso son inusuales?
test_chi_wdi$residuals %>% round(2)

# V de Cramér para el ejemplo WDI
n_wdi <- sum(tabla_wdi)
k_wdi <- min(nrow(tabla_wdi), ncol(tabla_wdi))
V_wdi <- sqrt(chisq.test(tabla_wdi)$statistic / (n_wdi * (k_wdi - 1)))
names(V_wdi) <- "V de Cramér"
V_wdi
