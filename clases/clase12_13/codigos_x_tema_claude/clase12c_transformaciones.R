# =============================================================================
# Ciencia de Datos para Economía y Negocios - FCE-UBA
# Clase 12c: Transformaciones de variables
# -----------------------------------------------------------------------------
# Contenidos:
#   1. Transformación logarítmica: cambia la FORMA
#   2. Estandarización (z-score): cambia la ESCALA
#   3. Normalización min-max: cambia el RANGO
#   4. Comparación visual de las cuatro versiones
#   5. Cuándo usar cada una y qué cuidados tener
# -----------------------------------------------------------------------------
# Datos: indicadores del Banco Mundial (paquete {WDI}) para el año 2022.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Preparación del entorno
# -----------------------------------------------------------------------------

library(tidyverse)
library(WDI)
library(scales)

options(scipen = 999)
set.seed(42)


# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================

indicadores <- c(
  pib_pc       = "NY.GDP.PCAP.CD",
  poblacion    = "SP.POP.TOTL",
  esperanza    = "SP.DYN.LE00.IN",
  inflacion    = "FP.CPI.TOTL.ZG"
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
         pib_pc, poblacion, esperanza, inflacion)


# =============================================================================
# 2. INTRODUCCIÓN: ¿POR QUÉ TRANSFORMAR?
# =============================================================================
#
# Hasta acá decidimos QUÉ HACER con los problemas (NAs, outliers). Ahora
# preparamos las variables para que sean más manejables analíticamente.
# Tres transformaciones clásicas:
#   - Logaritmo:        cambia la FORMA (asimetría -> simetría).
#   - Estandarización:  cambia la ESCALA (media 0, desvío 1).
#   - Min-Max:          cambia el RANGO (resultado entre 0 y 1).
#
# Lo crucial: log cambia la forma de la distribución. Z-score y min-max
# solo cambian la escala, no la forma. Esa diferencia es la que define
# para qué problemas sirve cada una.
# =============================================================================


# =============================================================================
# 3. TRANSFORMACIÓN LOGARÍTMICA
# =============================================================================
#
# Útil cuando la variable es positiva y tiene cola derecha larga.
# Muy común en economía: PIB, ingresos, ventas, precios.
# Ventaja extra: en regresión, los coeficientes se interpretan como
# elasticidades o cambios porcentuales.
# Cuidado:
#   - log(0) = -Inf -> si hay ceros, usar log(x + 1).
#   - log(negativo) no existe.
# =============================================================================

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

ggplot(paises,aes(x=log(pib_pc),y=esperanza)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# Conclusión visual: la versión en log es mucho más simétrica. Esa es la
# razón por la que en economía se trabaja casi siempre con log de ingresos
# o de PIB.

# Comparación numérica de la asimetría:
paises_log |>
  summarise(
    media_orig    = mean(pib_pc),
    mediana_orig  = median(pib_pc),
    media_log     = mean(log_pib),
    mediana_log   = median(log_pib)
  )
# En la versión original media >> mediana (asimetría a derecha).
# En la versión log media ~ mediana (distribución casi simétrica).


# =============================================================================
# 4. ESTANDARIZACIÓN (Z-SCORE)
# =============================================================================
#
# z_i = (x_i - media) / desvío. Resultado: media 0, desvío 1.
# Cuándo usarla:
#   - Comparar variables con distintas unidades (PIB en USD vs esperanza
#     de vida en años).
#   - Modelos donde la escala importa: clustering, PCA, regularización.
#   - Identificar valores extremos.
# Importante: NO cambia la forma de la distribución, solo la escala.
# =============================================================================

paises_std <- paises |>
  filter(!is.na(pib_pc), !is.na(esperanza)) |>
  mutate(
    pib_z       = (pib_pc    - mean(pib_pc))    / sd(pib_pc),
    esperanza_z = (esperanza - mean(esperanza)) / sd(esperanza)
  )

paises_std %>% 
  group_by(region) %>% 
  summarize(media_pib_pc = mean(pib_pc,na.rm=T),
            media_esperanza = mean(esperanza,na.rm=T),
            media_pib_pc_std = mean(pib_z,na.rm=T),
            media_esperanza_std = mean(esperanza_z,na.rm=T))

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


# =============================================================================
# 5. NORMALIZACIÓN MIN-MAX
# =============================================================================
#
# x' = (x - min) / (max - min). Resultado: rango [0, 1].
# Cuándo usarla:
#   - Cuando se necesita que todas las variables estén en el mismo rango.
#   - Algoritmos sensibles a escala (KNN, redes neuronales).
# Cuidado: es MUY sensible a outliers (un valor extremo comprime todo el
# resto contra el cero).
# =============================================================================

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


# =============================================================================
# 6. COMPARACIÓN VISUAL DE LAS CUATRO VERSIONES
# =============================================================================

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
# 7. CUÁNDO USAR CADA TRANSFORMACIÓN
# =============================================================================
#
# Tabla rápida de referencia:
#
#   Transformación   | Cambia      | Útil para                          | Cuidados
#   ---------------- | ----------- | ---------------------------------- | -----------------------
#   log(x)           | la FORMA    | variables económicas asimétricas;  | x > 0 estricto;
#                    |             | regresiones con elasticidades      | si hay 0, log(x+1)
#   z-score          | la ESCALA   | comparar variables de unidades     | no arregla la asimetría
#                    |             | distintas; PCA, clustering         |
#   min-max [0,1]    | el RANGO    | redes neuronales, KNN              | sensible a outliers
#
# Una guía práctica para variables económicas:
#   1. Si la variable es muy asimétrica (PIB, ingresos, ventas, precios)
#      -> primero log, después (si hace falta) z-score o min-max.
#   2. Si la variable ya es razonablemente simétrica (esperanza de vida,
#      tasas, porcentajes) -> z-score si querés comparar, sin transformar
#      si no.
#   3. Si tenés outliers genuinos en una variable que querés normalizar a
#      [0,1] -> winsorizá antes (ver clase12a), si no el min-max queda
#      dominado por los extremos.
# =============================================================================


# =============================================================================
# 8. EJEMPLO DE USO: REGRESIÓN CON Y SIN LOG
# =============================================================================
#
# Una de las razones principales por las que en economía se transforma con
# logaritmo es la INTERPRETACIÓN. Veámoslo.
# =============================================================================

datos_reg <- paises |>
  filter(!is.na(pib_pc), pib_pc > 0, !is.na(esperanza))

# Modelo en niveles:
modelo_lin <- lm(esperanza ~ pib_pc, data = datos_reg)
summary(modelo_lin)$coefficients

# Modelo log-lineal:
modelo_log <- lm(esperanza ~ log(pib_pc), data = datos_reg)
summary(modelo_log)$coefficients

# Interpretación:
#   - En el modelo lineal, el coeficiente dice cuántos años de esperanza
#     aumentan por cada USD adicional de PIB pc. Es un número chiquísimo
#     (tipo 0.0001) y no es muy útil.
#   - En el modelo log-lineal, el coeficiente dice cuántos años aumentan
#     por cada DUPLICACIÓN aproximada del PIB pc (multiplicar por log(2)
#     aproximado). Es un número interpretable.

# R² comparado:
c(R2_lineal = summary(modelo_lin)$r.squared,
  R2_log    = summary(modelo_log)$r.squared)
# Casi siempre el modelo log ajusta mejor cuando la variable explicativa
# tiene cola larga. Es un argumento empírico, no solo de interpretación.


# =============================================================================
# 9. CIERRE Y PREGUNTAS PARA PENSAR
# =============================================================================
#
# -----------------------------------------------------------------------------
# Preguntas para pensar / hacer en casa:
# -----------------------------------------------------------------------------
#
#   1. Apliquen las cuatro versiones (original, log, z-score, min-max) a la
#      variable poblacion. ¿Qué transformación funciona mejor para visualizar
#      la distribución? ¿Tiene sentido aplicar log a poblacion?
#
#   2. Comparen la regresión esperanza ~ pib_pc con la regresión
#      esperanza ~ log(pib_pc). ¿Cuál ajusta mejor (R²)? ¿Cómo se interpreta
#      el coeficiente en cada caso?
#
#   3. Estandaricen pib_pc y esperanza por separado y luego corran una
#      regresión simple entre ambas variables estandarizadas. ¿Qué representa
#      el coeficiente de pendiente en este caso?
#
#   4. Apliquen min-max a inflacion. ¿Qué problema se ve? ¿Cómo lo resolverían?
#      Probar primero winsorizando (sección 7.2 del script de outliers).
#
# -----------------------------------------------------------------------------
# Ideas clave para llevarse:
# -----------------------------------------------------------------------------
#
#   - El LOG cambia la forma de la distribución; el z-score y el min-max
#     solo cambian la escala.
#   - En economía, transformar con log no es solo una cuestión visual:
#     habilita interpretaciones en términos de elasticidades y suele mejorar
#     el ajuste de regresiones.
#   - Min-max es muy sensible a outliers; usarlo después de winsorizar
#     o de aplicar log es una práctica habitual.
#   - Toda transformación se DOCUMENTA en el informe. El lector necesita
#     saber si "el coeficiente de PIB" se refiere al PIB o al log del PIB.
#
# =============================================================================
