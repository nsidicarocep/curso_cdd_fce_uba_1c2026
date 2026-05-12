# =====================================================================
# Clase 14: Regresión lineal — Script práctico
# Dataset: World Development Indicators (Banco Mundial, paquete WDI)
# Caso: la curva de Preston — esperanza de vida y PIB per cápita
# =====================================================================

# ---------------------------------------------------------------------
# 1. SETUP
# ---------------------------------------------------------------------

# install.packages(c("tidyverse", "WDI", "broom",
#                    "lmtest", "sandwich", "car", "estimatr"))

library(tidyverse)
library(WDI)
library(broom)      # tidy(), glance()
library(lmtest)     # bptest(), coeftest()
library(sandwich)   # vcovHC() — errores robustos
library(car)        # vif()

options(scipen = 999)
theme_set(theme_minimal(base_size = 12))


# ---------------------------------------------------------------------
# 2. DESCARGA Y PREPARACIÓN DE DATOS
# ---------------------------------------------------------------------

# Indicadores del Banco Mundial:
#   NY.GDP.PCAP.PP.KD  - PIB per cápita PPA (USD constantes 2017)
#   SP.DYN.LE00.IN     - Esperanza de vida al nacer (años)
#   SH.XPD.CHEX.PC.CD  - Gasto en salud per cápita (USD corrientes)
#   SP.URB.TOTL.IN.ZS  - Población urbana (% del total)
#   SP.DYN.TFRT.IN     - Tasa de fertilidad (hijos por mujer)

indicadores <- c(
  pib_pc      = "NY.GDP.PCAP.PP.KD",
  esp_vida    = "SP.DYN.LE00.IN",
  gasto_salud = "SH.XPD.CHEX.PC.CD",
  urbano      = "SP.URB.TOTL.IN.ZS",
  fertilidad  = "SP.DYN.TFRT.IN"
)

# Usamos 2019 como último año "normal" pre-COVID con cobertura completa
datos_raw <- WDI(country   = "all",
                 indicator = indicadores,
                 start     = 2019,
                 end       = 2019,
                 extra     = TRUE)

# WDI con extra = TRUE incluye 'region' e 'income' (grupos de ingreso).
# Los agregados regionales tienen income == "Aggregates": los filtramos.
datos <- datos_raw |>
  filter(income != "Aggregates", !is.na(region)) |>
  select(pais = country, iso = iso3c, region, income,
         pib_pc, esp_vida, gasto_salud, urbano, fertilidad) |>
  drop_na()

# Convertimos las categóricas a factor con niveles ordenados
datos <- datos |>
  mutate(
    income = factor(income,
                    levels = c("Low income", "Lower middle income",
                               "Upper middle income", "High income")),
    region = factor(region)
  )

glimpse(datos)
nrow(datos)  # Cantidad de países en la muestra final


# ---------------------------------------------------------------------
# 3. EXPLORACIÓN VISUAL
# ---------------------------------------------------------------------

# Distribución de la variable dependiente
datos |>
  ggplot(aes(esp_vida)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white") +
  labs(x = "Esperanza de vida (años)", y = "Países",
       title = "Distribución de la esperanza de vida (2019)")

# Scatter: la curva de Preston
datos |>
  ggplot(aes(pib_pc, esp_vida)) +
  geom_point(aes(color = income), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "tomato",
              linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  labs(x = "PIB per cápita PPA (USD)", y = "Esperanza de vida",
       color = NULL,
       title = "La curva de Preston",
       subtitle = "Línea punteada: lineal | Línea verde: loess (no paramétrica)") +
  theme(legend.position = "bottom")

# La relación es CLARAMENTE no lineal en niveles.
# Veamos con log(pib_pc):
datos |>
  ggplot(aes(log(pib_pc), esp_vida)) +
  geom_point(aes(color = income), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") +
  labs(x = "log(PIB per cápita)", y = "Esperanza de vida",
       color = NULL,
       title = "Con log(PIB), la relación se linealiza") +
  theme(legend.position = "bottom")


# ---------------------------------------------------------------------
# 4. MODELO LINEAL SIMPLE
# ---------------------------------------------------------------------

# Versión naive: en niveles
mod_simple_nivel <- lm(esp_vida ~ pib_pc, data = datos)
summary(mod_simple_nivel)

# Versión con log(pib_pc): semielasticidad
mod_simple_log <- lm(esp_vida ~ log(pib_pc), data = datos)
summary(mod_simple_log)

# Interpretación del modelo log:
#   beta = coeficiente de log(pib_pc)
#   Un aumento del 1% en el PIB per cápita se asocia con un aumento
#   de aproximadamente (beta / 100) años de esperanza de vida.

# Comparación rápida con broom::glance
bind_rows(
  glance(mod_simple_nivel) |> mutate(modelo = "Nivel"),
  glance(mod_simple_log)   |> mutate(modelo = "Log(PIB)")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)

# IMPORTANTE: ambos modelos tienen la MISMA variable dependiente (esp_vida),
# por lo que sí es válido comparar AIC y BIC.

# LO QUE NO PUEDOH HACER JAMAS ES COMPARAR MODELOS CON Y EN DIFERENTE FORMATO


# ---------------------------------------------------------------------
# 5. REGRESIÓN MÚLTIPLE CON CONTROLES CONTINUOS
# ---------------------------------------------------------------------

mod_mult <- lm(esp_vida ~ log(pib_pc) + log(gasto_salud) +
                 urbano + fertilidad,
               data = datos)
summary(mod_mult)

# Lectura ceteris paribus:
#   - log(pib_pc):      efecto del PIB, controlando salud, urbanización y fertilidad
#   - log(gasto_salud): efecto del gasto en salud, controlando PIB, ...
#   - urbano:           cada punto porcentual extra de urbanización
#   - fertilidad:       efecto de cada hijo adicional por mujer (suele ser negativo)

# Diagnóstico rápido de multicolinealidad (VIF)
vif(mod_mult)
# Regla práctica: VIF > 5 indica colinealidad preocupante; > 10 es grave.



# ---------------------------------------------------------------------
# 6. VARIABLES CATEGÓRICAS (DUMMIES)
# ---------------------------------------------------------------------

# Niveles de income (ya ordenados manualmente arriba)
levels(datos$income)
# Por default, el primero ("Low income") es la referencia.

# Modelo con grupo de ingreso como categórica
mod_income <- lm(esp_vida ~ log(pib_pc) + income, data = datos)
summary(mod_income)

# Cada coeficiente de income mide la diferencia promedio en años
# de esperanza de vida respecto a la categoría base (Low income),
# controlando por el log(PIB per cápita).

# ¿Y si quisiéramos High income como referencia?
datos_alt <- datos |>
  mutate(income = relevel(income, ref = "High income"))

mod_income_alt <- lm(esp_vida ~ log(pib_pc) + income, data = datos_alt)
summary(mod_income_alt)
# Ahora los coeficientes son negativos: cuánto MENOS vive cada grupo
# respecto a los países de alto ingreso.

# El R² y los valores ajustados son IDÉNTICOS: cambiar la referencia
# es solo una reparametrización.

# Test F para la significancia conjunta de income
anova(lm(esp_vida ~ log(pib_pc), data = datos), mod_income)
# H0: todos los coeficientes de income son cero.
# Si p < 0.05, income aporta información más allá del log(PIB).


# ---------------------------------------------------------------------
# 7. MODELO CON REGIÓN Y MÚLTIPLES CATEGÓRICAS
# ---------------------------------------------------------------------

# Pasamos Sub-Saharan Africa a referencia (la región con menor esp_vida en promedio)
datos <- datos |>
  mutate(region = relevel(region, ref = "Sub-Saharan Africa"))

mod_completo <- lm(esp_vida ~ log(pib_pc) + log(gasto_salud) +
                     urbano + fertilidad + income + region,
                   data = datos)
summary(mod_completo)

# Cómo se lee:
# - Los coeficientes de region son diferencias promedio en años respecto a
#   Sub-Saharan Africa, controlando por el resto.
# - Si un coeficiente de region NO es significativo, no podemos rechazar que
#   esa región sea equivalente a Sub-Saharan Africa en esperanza de vida,
#   una vez que controlamos por PIB, salud, urbanización, etc.


# ---------------------------------------------------------------------
# 8. COMPARACIÓN DE MODELOS: R² ajustado, AIC y BIC
# ---------------------------------------------------------------------

modelos <- list(
  "1. Simple nivel"        = mod_simple_nivel,
  "2. Simple log"          = mod_simple_log,
  "3. Múltiple continuas"  = mod_mult,
  "4. + income"            = mod_income,
  "5. + region (completo)" = mod_completo
)

comparacion <- map_dfr(modelos, glance, .id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC, df)

print(comparacion, n = Inf)

# Observaciones esperadas:
# - El R² crece SIEMPRE al sumar variables (modelos 1 -> 5).
# - El R² ajustado puede subir o bajar: sube solo si la variable agregada
#   aporta más de lo que cuesta perder un grado de libertad.
# - AIC y BIC penalizan la complejidad: menor es mejor.
# - BIC penaliza más fuerte: a veces "elige" un modelo más chico que AIC.


# ---------------------------------------------------------------------
# 9. DIAGNÓSTICO: ¿HAY HETEROSCEDASTICIDAD?
# ---------------------------------------------------------------------

# Gráfico residuos vs valores ajustados
tibble(
  ajustado = fitted(mod_completo),
  residuo  = resid(mod_completo)
) |>
  ggplot(aes(ajustado, residuo)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "tomato") +
  geom_smooth(se = FALSE, color = "darkred", linewidth = 0.6) +
  labs(x = "Valores ajustados", y = "Residuos",
       title = "Residuos vs ajustados",
       subtitle = "¿La dispersión es pareja a lo largo del eje X?")

# Cuatro gráficos diagnósticos base de R
par(mfrow = c(2, 2))
plot(mod_completo)
par(mfrow = c(1, 1))

# Test formal de Breusch-Pagan
# H0: homoscedasticidad
bptest(mod_completo)
# Si p < 0.05: rechazamos H0, hay evidencia de heteroscedasticidad.


# ---------------------------------------------------------------------
# 10. ERRORES ROBUSTOS DE WHITE
# ---------------------------------------------------------------------

# Errores estándar clásicos (asumen homoscedasticidad)
coeftest(mod_completo)

# Errores robustos HC1 (default de Stata, recomendado para n grande)
coeftest(mod_completo, vcov = vcovHC(mod_completo, type = "HC1"))

# Errores robustos HC3 (conservadores, recomendado para muestras chicas)
coeftest(mod_completo, vcov = vcovHC(mod_completo, type = "HC3"))

# Comparación lado a lado: SE clásicos vs robustos
se_clasico <- coeftest(mod_completo)
se_robusto <- coeftest(mod_completo, vcov = vcovHC(mod_completo, type = "HC1"))

comparacion_se <- tibble(
  Coeficiente = rownames(se_clasico),
  Estimate    = round(se_clasico[, "Estimate"], 3),
  SE_clasico  = round(se_clasico[, "Std. Error"], 3),
  SE_robusto  = round(se_robusto[, "Std. Error"], 3),
  Ratio       = round(se_robusto[, "Std. Error"] /
                        se_clasico[, "Std. Error"], 2)
)
print(comparacion_se, n = Inf)

# IMPORTANTE: los coeficientes (Estimate) son IDÉNTICOS — los SE robustos
# no cambian a MCO, solo corrigen los errores estándar.
# Donde el ratio > 1, los SE clásicos subestiman la incertidumbre.


# ---------------------------------------------------------------------
# 11. ALTERNATIVA: estimatr::lm_robust()
# ---------------------------------------------------------------------

# install.packages("estimatr")
library(estimatr)

mod_robust <- lm_robust(esp_vida ~ log(pib_pc) + log(gasto_salud) +
                          urbano + fertilidad + income + region,
                        data    = datos,
                        se_type = "HC1")
summary(mod_robust)

# ---------------------------------------------------------------------
# 13. RESULTADOS PARA EL INFORME (broom::tidy)
# ---------------------------------------------------------------------

# Tabla "limpia" del modelo completo, con SE robustos
resultados <- tidy(mod_completo,
                   conf.int = TRUE,
                   se_type  = "HC1") |>
  mutate(across(where(is.numeric), \(x) round(x, 3)))

print(resultados, n = Inf)
