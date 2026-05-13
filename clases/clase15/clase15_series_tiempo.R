# ============================================================================
# Clase 15 — Series Temporales: código práctico con datos del INDEC
# Ciencia de Datos para Economía y Negocios | FCE-UBA
# Nicolás Sidicaro
# ============================================================================

# 0. Paquetes ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)       # rollmean, rollapply, na.locf
library(stinepack) # interpolación de Stineman

theme_set(theme_minimal(base_size = 12))


# 1. Carga de datos ----------------------------------------------------------
# Usamos la API de datos.gob.ar para descargar series de INDEC.
# Documentación: https://datos.gob.ar/series/api/

# --- 1.1 EMAE (Estimador Mensual de Actividad Económica, base 2004 = 100) ---
emae <- read_csv(
  paste0(
    'https://infra.datos.gob.ar/catalog/sspm/dataset/143/distribution/143.3/download/emae-valores-anuales-indice-base-2004-mensual.csv'
    ),
  show_col_types = FALSE
) |>
  rename(fecha = indice_tiempo, emae = 2) |>
  mutate(fecha = ymd(fecha))

# --- 1.2 IPC Nacional (nivel general, base dic-2016 = 100) ---
ipc <- read_csv(
  paste0("https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.3/download/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-mensual.csv"),
  show_col_types = FALSE
) |>
  rename(fecha = indice_tiempo, ipc = 2) |>
  mutate(fecha = ymd(fecha))

# --- 1.3 Censos nacionales de población (fuente: INDEC) ---
censos <- tibble(
  anio = c(1869, 1895, 1914, 1947, 1960, 1970, 1980, 1991, 2001, 2010, 2022),
  poblacion = c(
    1877490, 4044911, 7903662, 15893827, 20013793,
    23364431, 27949480, 32615528, 36260130, 40117096, 46044703
  )
)

# --- 1.4 Vistazo rápido a los datos ---
glimpse(emae)
glimpse(ipc)
print(censos)


# ============================================================================
# SECCIÓN 1 — EXPLORACIÓN Y COMPONENTES
# ============================================================================

# 2. ¿Qué aspecto tiene una serie temporal? ----------------------------------

ggplot(emae, aes(x = fecha, y = emae)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  labs(
    title = "EMAE: Estimador Mensual de Actividad Económica",
    subtitle = "Base 2004 = 100 | Fuente: INDEC vía datos.gob.ar",
    x = NULL, y = "Índice"
  )

# ¿Qué componentes identificamos visualmente?
# - Tendencia: crecimiento de largo plazo (con caída 2018-2020)
# - Estacionalidad: patrón que se repite cada 12 meses
# - Ciclo: las recesiones de 2009, 2014, 2018, 2020
# - Ruido: fluctuaciones irregulares mes a mes


# 3. El objeto ts() -----------------------------------------------------------
# R tiene una clase nativa para series temporales: ts()
# Requiere especificar la frecuencia (12 = mensual, 4 = trimestral)

emae_ts <- ts(
  emae$emae,
  start = c(year(min(emae$fecha)), month(min(emae$fecha))),
  frequency = 12
)

# Verificar
print(window(emae_ts, start = c(2004, 1), end = c(2004, 12)))
frequency(emae_ts)  # 12
cycle(emae_ts) |> head(24)  # mes de cada observación


# ============================================================================
# SECCIÓN 2 — TRANSFORMACIONES ESTÁNDAR
# ============================================================================

# 4. Indexación y cambio de base -----------------------------------------------

# El EMAE tiene base 2004 = 100. ¿Y si queremos base enero 2012?
base_ene2012 <- emae |> filter(fecha == "2012-01-01") |> pull(emae)

emae <- emae |>
  mutate(emae_base2012 = emae / base_ene2012 * 100)

# Comparar
emae |>
  select(fecha, `Base 2004` = emae, `Base ene-2012` = emae_base2012) |>
  pivot_longer(-fecha, names_to = "base", values_to = "valor") |>
  ggplot(aes(x = fecha, y = valor, color = base)) +
  geom_line(linewidth = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  labs(
    title = "EMAE con distintas bases",
    subtitle = "El cambio de base no altera la dinámica, solo el nivel de referencia",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme(legend.position = "bottom")


# 5. Deflactación --------------------------------------------------------------
# Llevemos los salarios a terminos reales 

salarios <- read_csv('https://cdn.produccion.gob.ar/cdn-cep/datos-por-provincia/por-provincia/salarios/w_mean_priv.csv')

# Recortamos al período donde tenemos ambas series
periodo_comun <- inner_join(salarios, ipc, by = "fecha")

periodo_comun <- periodo_comun |> 
  filter(zona_prov == 'BUENOS AIRES') %>% 
  mutate(
    nominal = w_mean,
    # Deflactamos dividiendo por IPC
    real = nominal / ipc * 100
  )

periodo_comun |>
  select(fecha, Nominal = nominal, Real = real, Salario = nominal) |>
  pivot_longer(-fecha, names_to = "serie", values_to = "valor") |>
  ggplot(aes(x = fecha, y = valor, color = serie)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Deflactación: serie nominal vs. real",
    subtitle = "Real = Nominal / IPC × 100 | Fuente: INDEC",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme(legend.position = "bottom")


# 6. Variaciones y log-diferencias ---------------------------------------------

emae <- emae |>
  arrange(fecha) |>
  mutate(
    # Variación mensual (respecto al mes anterior)
    var_mensual = (emae / lag(emae) - 1) * 100,
    
    # Variación interanual (respecto al mismo mes del año anterior)
    var_interanual = (emae / lag(emae, 12) - 1) * 100,
    
    # Log-diferencia mensual (aproximación a la variación %)
    logdif_mensual = (log(emae) - log(lag(emae))) * 100,
    
    # Log-diferencia interanual
    logdif_interanual = (log(emae) - log(lag(emae, 12))) * 100
  )

# La variación interanual neutraliza la estacionalidad
emae |>
  filter(!is.na(var_interanual)) |>
  ggplot(aes(x = fecha, y = var_interanual)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_line(color = "steelblue", linewidth = 0.4) +
  geom_col(aes(fill = var_interanual > 0), alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  labs(
    title = "EMAE: variación interanual (%)",
    subtitle = "Compara cada mes con el mismo mes del año anterior",
    x = NULL, y = "%"
  )

# Comparar variación % simple vs. log-diferencia
emae |>
  filter(!is.na(var_mensual)) |>
  ggplot(aes(x = var_mensual, y = logdif_mensual)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "tomato") +
  labs(
    title = "Variación % simple vs. log-diferencia",
    subtitle = "Son casi idénticas para cambios pequeños; divergen para cambios grandes",
    x = "Variación % simple", y = "Log-diferencia × 100"
  )


# ============================================================================
# SECCIÓN 3 — PROMEDIOS MÓVILES
# ============================================================================

# 7. Promedios móviles sobre el EMAE -------------------------------------------

emae <- emae |>
  mutate(
    # Promedio móvil centrado de 3 meses
    ma3  = rollmean(emae, k = 3, fill = NA, align = "center"),
    # Promedio móvil centrado de 12 meses (elimina estacionalidad)
    ma12 = rollmean(emae, k = 12, fill = NA, align = "center")
  )

emae |>
  select(fecha, Original = emae, `MA-3` = ma3, `MA-12` = ma12) |>
  pivot_longer(-fecha, names_to = "serie", values_to = "valor") |>
  mutate(serie = factor(serie, levels = c("Original", "MA-3", "MA-12"))) |>
  ggplot(aes(x = fecha, y = valor, color = serie, linewidth = serie)) +
  geom_line() +
  scale_color_manual(values = c("grey70", "steelblue", "tomato")) +
  scale_linewidth_manual(values = c(0.3, 0.6, 1.0)) +
  labs(
    title = "EMAE: promedios móviles de distinto orden",
    subtitle = "MA-12 estima la tendencia-ciclo eliminando la estacionalidad",
    x = NULL, y = "Índice", color = NULL, linewidth = NULL
  ) +
  theme(legend.position = "bottom")


# 8. Promedio móvil exponencial (EMA) ------------------------------------------

# alpha alto → más peso al dato reciente, menos suavizado
# alpha bajo → más suavizado

emae <- emae |>
  mutate(
    ema_rapido = TTR::EMA(emae, n = 3),   # equivale a alpha ≈ 0.5
    ema_lento  = TTR::EMA(emae, n = 12)   # equivale a alpha ≈ 0.15
  )

# Nota: EMA() viene de TTR Si no está disponible, se puede hacer manualmente:
# ema_manual <- function(x, alpha) {
#   ema <- numeric(length(x))
#   ema[1] <- x[1]
#   for (i in 2:length(x)) ema[i] <- alpha * x[i] + (1 - alpha) * ema[i-1]
#   ema
# }

emae |>
  select(fecha, Original = emae, `EMA rápido (n=3)` = ema_rapido,
         `EMA lento (n=12)` = ema_lento) |>
  pivot_longer(-fecha, names_to = "serie", values_to = "valor") |>
  ggplot(aes(x = fecha, y = valor, color = serie)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("steelblue", "darkorange", "grey70")) +
  labs(
    title = "EMAE: promedios móviles exponenciales",
    subtitle = "El EMA lento se comporta como estimador de tendencia",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme(legend.position = "bottom")


# ============================================================================
# SECCIÓN 4 — DESCOMPOSICIÓN
# ============================================================================

# 9. Descomposición clásica ----------------------------------------------------

desc_aditiva <- decompose(emae_ts, type = "additive")
desc_mult    <- decompose(emae_ts, type = "multiplicative")

# Graficar la descomposición aditiva
plot(desc_aditiva,
     col = "steelblue",
     xlab = "")
title("Descomposición clásica aditiva del EMAE", line = -1, outer = TRUE)

# También podemos extraer los componentes como tibble para ggplot
desc_df <- tibble(
  fecha = emae$fecha,
  observada    = as.numeric(emae_ts),
  tendencia    = as.numeric(desc_aditiva$trend),
  estacional   = as.numeric(desc_aditiva$seasonal),
  residuo      = as.numeric(desc_aditiva$random)
) |>
  pivot_longer(-fecha, names_to = "componente", values_to = "valor") |>
  mutate(componente = factor(componente,
                             levels = c("observada", "tendencia", "estacional", "residuo"),
                             labels = c("Serie observada", "Tendencia", "Estacionalidad", "Residuo")
  ))

ggplot(desc_df, aes(x = fecha, y = valor)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  facet_wrap(~componente, scales = "free_y", ncol = 1) +
  labs(
    title = "EMAE: descomposición clásica aditiva",
    subtitle = "decompose(emae_ts, type = 'additive')",
    x = NULL, y = NULL
  )


# 10. Descomposición STL -------------------------------------------------------

desc_stl <- stl(emae_ts, s.window = "periodic")
# s.window = "periodic" → estacionalidad constante (como la clásica)
# s.window = 13         → estacionalidad que varía lentamente

# Con estacionalidad variable
desc_stl_flex <- stl(emae_ts, s.window = 13, t.window = 25, robust = TRUE)

plot(desc_stl_flex,
     col = "steelblue",
     main = "STL con estacionalidad variable y robustez")


# 11. Comparar clásica vs. STL ------------------------------------------------

# Extraer estacionalidades de ambos métodos
comp_estacionalidad <- tibble(
  fecha       = emae$fecha,
  clasica     = as.numeric(desc_aditiva$seasonal),
  stl_fija    = as.numeric(desc_stl$time.series[, "seasonal"]),
  stl_variable = as.numeric(desc_stl_flex$time.series[, "seasonal"])
) |>
  pivot_longer(-fecha, names_to = "metodo", values_to = "estacional") |>
  mutate(metodo = factor(metodo,
                         levels = c("clasica", "stl_fija", "stl_variable"),
                         labels = c("Clásica", "STL (periódica)", "STL (variable)")
  ))

ggplot(comp_estacionalidad, aes(x = fecha, y = estacional, color = metodo)) +
  geom_line(linewidth = 0.4) +
  labs(
    title = "Componente estacional: clásica vs. STL",
    subtitle = "STL con s.window bajo permite que la estacionalidad cambie en el tiempo",
    x = NULL, y = "Factor estacional", color = NULL
  ) +
  theme(legend.position = "bottom")


# ============================================================================
# SECCIÓN 5 — DESESTACIONALIZACIÓN
# ============================================================================

# 12. Factores estacionales manuales ------------------------------------------

# Paso a paso: el método "artesanal"
emae_desest <- emae |>
  mutate(
    mes = month(fecha),
    # Tendencia estimada con MA-12 centrado (ya calculada arriba como ma12)
    ratio = emae / ma12
  )

# Factor estacional promedio por mes
factores <- emae_desest |>
  filter(!is.na(ratio)) |>
  group_by(mes) |>
  summarise(factor_estacional = mean(ratio), .groups = "drop")

print(factores)

# Los factores deben promediar ~1 (multiplicativo) o ~0 (aditivo).
# Si no, se normalizan:
factores <- factores |>
  mutate(factor_norm = factor_estacional / mean(factor_estacional))

# Aplicar
emae_desest <- emae_desest |>
  left_join(factores |> select(mes, factor_norm), by = "mes") |>
  mutate(emae_sa_manual = emae / factor_norm)

ggplot(emae_desest, aes(x = fecha)) +
  geom_line(aes(y = emae, color = "Original"), linewidth = 0.3) +
  geom_line(aes(y = emae_sa_manual, color = "Desestac. manual"), linewidth = 0.5) +
  scale_color_manual(values = c("Original" = "grey60", "Desestac. manual" = "steelblue")) +
  labs(
    title = "EMAE: desestacionalización por factores estacionales",
    subtitle = "Método multiplicativo con factores constantes",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme(legend.position = "bottom")


# 13. Desestacionalización con STL --------------------------------------------

emae_sa_stl <- emae_ts - desc_stl_flex$time.series[, "seasonal"]

emae_desest <- emae_desest |>
  mutate(
    emae_sa_stl    = as.numeric(emae_sa_stl),
    emae_tendencia = as.numeric(desc_stl_flex$time.series[, "trend"])
  )

ggplot(emae_desest, aes(x = fecha)) +
  geom_line(aes(y = emae, color = "Original"), linewidth = 0.3, alpha = 0.6) +
  geom_line(aes(y = emae_sa_stl, color = "Desestacionalizada (STL)"), linewidth = 0.5) +
  geom_line(aes(y = emae_tendencia, color = "Tendencia (STL)"), linewidth = 0.8) +
  scale_color_manual(values = c(
    "Original" = "grey60",
    "Desestacionalizada (STL)" = "steelblue",
    "Tendencia (STL)" = "tomato"
  )) +
  labs(
    title = "EMAE: original vs. desestacionalizada vs. tendencia",
    subtitle = "STL con estacionalidad variable (s.window = 13) y robustez",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme(legend.position = "bottom")


# 14. ¿Cómo se ve la estacionalidad mes a mes? --------------------------------

# Gráfico de estacionalidad: cada año como una línea
emae |>
  mutate(anio = year(fecha), mes = month(fecha, label = TRUE)) |>
  filter(anio >= 2010, anio <= 2023) |>
  ggplot(aes(x = mes, y = emae, group = anio, color = factor(anio))) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Patrón estacional del EMAE",
    subtitle = "Cada línea es un año. El valle de enero-febrero es sistemático.",
    x = NULL, y = "Índice", color = "Año"
  ) +
  theme(legend.position = "right")


# ============================================================================
# SECCIÓN 6 — EMPALME DE SERIES
# ============================================================================

# 15. Construir dos series con quiebre ----------------------------------------
# Ejemplo motivador: simulamos lo que ocurre cuando el INDEC cambia la base
# de un indicador. Usamos la propia serie de EMAE partida en dos tramos
# con un quiebre artificial.

# Serie "vieja" (base A): corre hasta dic-2015, con un nivel más bajo
# Serie "nueva" (base B): arranca en ene-2012, nivel original del EMAE
# Overlap: 2012-2015 (4 años de superposición)

factor_quiebre <- 0.75  # la serie vieja está en una escala 25% menor

empalme <- emae |>
  select(fecha, emae) |>
  mutate(
    serie_vieja = if_else(fecha <= "2015-12-01", emae * factor_quiebre, NA_real_),
    serie_nueva = if_else(fecha >= "2012-01-01", emae, NA_real_)
  )

# Visualizar las dos series con el overlap sombreado
ggplot(empalme, aes(x = fecha)) +
  annotate("rect",
           xmin = ymd("2012-01-01"), xmax = ymd("2015-12-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "steelblue"
  ) +
  geom_line(aes(y = serie_vieja, color = "Serie vieja (base A)"),
            linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = serie_nueva, color = "Serie nueva (base B)"),
            linewidth = 0.5, na.rm = TRUE) +
  annotate("text", x = ymd("2014-01-01"), y = Inf, label = "Overlap",
           vjust = 2, color = "steelblue", fontface = "bold") +
  scale_color_manual(values = c("steelblue", "tomato")) +
  labs(
    title = "Dos series con quiebre de nivel",
    subtitle = "Zona sombreada: período de superposición (2012–2015)",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme(legend.position = "bottom")


# 16. Método por cociente (retropolación multiplicativa) -----------------------
# Supuesto: el quiebre es proporcional al nivel.

# Calculamos el ratio promedio en el período de overlap
overlap <- empalme |>
  filter(!is.na(serie_vieja) & !is.na(serie_nueva))

ratio_empalme <- mean(overlap$serie_nueva / overlap$serie_vieja)
cat("Ratio promedio en el overlap:", round(ratio_empalme, 4), "\n")

# Retropolamos: multiplicamos la serie vieja por el ratio
empalme <- empalme |>
  mutate(
    emp_cociente = case_when(
      !is.na(serie_nueva) ~ serie_nueva,
      TRUE ~ serie_vieja * ratio_empalme
    )
  )


# 17. Método por diferencia (empalme aditivo) ----------------------------------
# Supuesto: el quiebre es una constante aditiva.

brecha_empalme <- mean(overlap$serie_nueva - overlap$serie_vieja)
cat("Brecha promedio en el overlap:", round(brecha_empalme, 2), "\n")

empalme <- empalme |>
  mutate(
    emp_diferencia = case_when(
      !is.na(serie_nueva) ~ serie_nueva,
      TRUE ~ serie_vieja + brecha_empalme
    )
  )


# 18. Encadenamiento por tasas de variación ------------------------------------
# Preserva la dinámica de la serie vieja, no sus niveles.

# Punto de empalme: ene-2012 (primer mes de la serie nueva)
punto <- empalme |> filter(fecha == "2012-01-01")

# Reconstruir hacia atrás usando variaciones de la serie vieja
emae_vieja <- empalme |>
  filter(!is.na(serie_vieja)) |>
  arrange(fecha) |>
  mutate(var_vieja = serie_vieja / lag(serie_vieja))

# Nivel de partida desde la serie nueva
nivel_empalme <- punto$serie_nueva

# Reconstruir
datos_pre <- emae_vieja |>
  filter(fecha <= "2012-01-01") |>
  arrange(desc(fecha))

emp_tasas <- numeric(nrow(datos_pre))
emp_tasas[1] <- nivel_empalme  # ene-2012

for (i in 2:nrow(datos_pre)) {
  # Ir hacia atrás: dividir por la variación
  emp_tasas[i] <- emp_tasas[i - 1] / datos_pre$var_vieja[i - 1]
}

datos_pre$emp_tasas <- emp_tasas

# Combinar con la serie nueva
empalme <- empalme |>
  left_join(
    datos_pre |> select(fecha, emp_tasas),
    by = "fecha"
  ) |>
  mutate(
    emp_tasas = if_else(is.na(emp_tasas) & !is.na(serie_nueva),
                        serie_nueva, emp_tasas)
  )


# 19. Comparar los tres métodos ------------------------------------------------

empalme |>
  select(fecha, Original = emae,
         Cociente = emp_cociente,
         Diferencia = emp_diferencia,
         `Tasas de var.` = emp_tasas) |>
  filter(!is.na(Original), fecha <= "2018-01-01") |>
  pivot_longer(-fecha, names_to = "metodo", values_to = "valor") |>
  ggplot(aes(x = fecha, y = valor, color = metodo, linetype = metodo)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("grey40", "steelblue", "darkorange", "forestgreen")) +
  scale_linetype_manual(values = c("dashed", "solid", "solid", "solid")) +
  labs(
    title = "Comparación de métodos de empalme",
    subtitle = "Los tres reconstruyen la serie vieja; difieren en el supuesto sobre el quiebre",
    x = NULL, y = "Índice", color = NULL, linetype = NULL
  ) +
  theme(legend.position = "bottom")


# ============================================================================
# SECCIÓN 7 — INTERPOLACIÓN
# ============================================================================

# 20. Datos censales: un dato cada ~10 años ------------------------------------

ggplot(censos, aes(x = anio, y = poblacion / 1e6)) +
  geom_point(color = "steelblue", size = 3) +
  geom_line(color = "steelblue", linetype = "dashed", linewidth = 0.3) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Población de Argentina según censos nacionales",
    subtitle = "¿Cómo estimar la población en los años intercensales?",
    x = NULL, y = "Millones de habitantes"
  )


# 21. Grilla anual para interpolar ---------------------------------------------

anios <- tibble(anio = seq(min(censos$anio), max(censos$anio)))
interp <- anios |> left_join(censos, by = "anio")


# 22. LOCF (Last Observation Carried Forward) ----------------------------------

interp <- interp |>
  mutate(locf = na.locf(poblacion, na.rm = FALSE))


# 23. Interpolación lineal ----------------------------------------------------

interp <- interp |>
  mutate(lineal = approx(
    x = censos$anio, y = censos$poblacion,
    xout = anio, method = "linear"
  )$y)


# 24. Interpolación exponencial ------------------------------------------------
# Equivale a interpolar linealmente sobre log(población) y exponenciar.

interp <- interp |>
  mutate(exponencial = exp(approx(
    x = censos$anio, y = log(censos$poblacion),
    xout = anio, method = "linear"
  )$y))


# 25. Spline cúbico -----------------------------------------------------------

interp <- interp |>
  mutate(spline = spline(
    x = censos$anio, y = censos$poblacion,
    xout = anio, method = "natural"
  )$y)


# 27. Comparación visual de todos los métodos ----------------------------------

interp_long <- interp |>
  pivot_longer(
    cols = c(locf, lineal, exponencial, spline),
    names_to = "metodo",
    values_to = "pob_interp"
  ) |>
  mutate(metodo = factor(metodo,
                         levels = c("locf", "lineal", "exponencial", "spline"),
                         labels = c("LOCF", "Lineal", "Exponencial", "Spline cúbico")
  ))

ggplot(interp_long, aes(x = anio, y = pob_interp / 1e6)) +
  geom_line(aes(color = metodo), linewidth = 0.5) +
  geom_point(
    data = censos, aes(x = anio, y = poblacion / 1e6),
    color = "black", size = 2, inherit.aes = FALSE
  ) +
  facet_wrap(~metodo, ncol = 3) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Interpolación de población intercensal: cinco métodos",
    subtitle = "Puntos negros = datos censales observados",
    x = NULL, y = "Millones de hab.", color = NULL
  ) +
  theme(legend.position = "none")


# 28. Zoom: ¿dónde difieren más? ----------------------------------------------
# El período 1914–1947 (33 años sin censo) es el más exigente.

interp_long |>
  filter(anio >= 1910, anio <= 1955) |>
  ggplot(aes(x = anio, y = pob_interp / 1e6, color = metodo)) +
  geom_line(linewidth = 0.6) +
  geom_point(
    data = censos |> filter(anio >= 1910, anio <= 1955),
    aes(x = anio, y = poblacion / 1e6),
    color = "black", size = 3, inherit.aes = FALSE
  ) +
  labs(
    title = "Zoom 1910–1955: el gap más largo (33 años sin censo)",
    subtitle = "El LOCF genera un escalón; el spline puede oscilar; lineal y exponencial son monótonos",
    x = NULL, y = "Millones de hab.", color = NULL
  ) +
  theme(legend.position = "bottom")


# 29. Interpolación en una serie mensual: gaps artificiales --------------------
# ¿Qué pasa si le sacamos algunos meses al EMAE y tratamos de recuperarlos?

set.seed(123)
emae_gap <- emae |>
  filter(fecha >= "2015-01-01", fecha <= "2019-12-01") |>
  select(fecha, emae) |>
  mutate(
    # Crear un gap de 6 meses
    es_gap = fecha >= "2017-04-01" & fecha <= "2017-09-01",
    emae_obs = if_else(es_gap, NA_real_, emae)
  )

# Aplicar métodos
t_num <- as.numeric(emae_gap$fecha)
obs_idx <- which(!is.na(emae_gap$emae_obs))

emae_gap <- emae_gap |>
  mutate(
    locf    = na.locf(emae_obs, na.rm = FALSE),
    lineal  = approx(t_num[obs_idx], emae_obs[obs_idx], xout = t_num)$y,
    spline  = spline(t_num[obs_idx], emae_obs[obs_idx], xout = t_num, method = "natural")$y
  )

emae_gap |>
  pivot_longer(c(emae, locf, lineal, spline),
               names_to = "serie", values_to = "valor") |>
  mutate(serie = factor(serie,
                        levels = c("emae", "locf", "lineal", "spline"),
                        labels = c("Verdadera (oculta)", "LOCF", "Lineal", "Spline")
  )) |>
  ggplot(aes(x = fecha, y = valor, color = serie, linetype = serie)) +
  annotate("rect",
           xmin = ymd("2017-04-01"), xmax = ymd("2017-09-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "tomato"
  ) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = c("grey50", "darkorange", "steelblue", "forestgreen")) +
  scale_linetype_manual(values = c("dashed", "solid", "solid", "solid")) +
  labs(
    title = "Interpolación de un gap de 6 meses en el EMAE",
    subtitle = "Zona sombreada: datos faltantes. Línea punteada: serie verdadera.",
    x = NULL, y = "Índice", color = NULL, linetype = NULL
  ) +
  theme(legend.position = "bottom")
