# Ciencia de Datos para Economía y Negocios

**Facultad de Ciencias Económicas — Universidad de Buenos Aires**

Docente: Nicolás Sidicaro
1er cuatrimestre 2026

---

## 📋 Presentación del curso

Este curso introduce a estudiantes de la Licenciatura en Economía en las herramientas y conceptos fundamentales de la ciencia de datos, con un enfoque orientado a la aplicación en problemas económicos reales. No busca ser exhaustivo en formalismos matemáticos, sino brindar una base sólida de programación, análisis estadístico y visualización que permita a los estudiantes trabajar con datos de manera autónoma y rigurosa.

El lenguaje de programación utilizado es **R**, elegido por su fortaleza en estadística, econometría y visualización de datos (especialmente `ggplot2`), y por su amplia adopción en investigación económica.

Las clases se organizan en dos instancias semanales:

- **Martes**: clases teóricas (presencial)
- **Viernes**: clases prácticas de programación y código (virtual)

---

## 🔗 Links relevantes

| Recurso | Link |
|---|---|
| Cronograma del curso | [Link al cronograma](https://docs.google.com/spreadsheets/d/1iKuWU7Yc_EEUsqabAkBizfgksrgC1FhG1uCD0RJZB-Q/edit) |
| Consignas del Trabajo Práctico | [Link al TP]([URL_TP](https://github.com/nsidicarocep/curso_cdd_fce_uba_1c2026/tree/main/trabajo_practico)) |
| Bibliografía completa | [Link a bibliografía](https://docs.google.com/spreadsheets/d/1iKuWU7Yc_EEUsqabAkBizfgksrgC1FhG1uCD0RJZB-Q/edit?gid=283594947#gid=283594947) |
| Clase virtual de los viernes | [Link a la clase](https://teams.microsoft.com/l/meetup-join/19%3ameeting_ZDA1Yjc2ZTMtNTk2NC00MDI0LWFiMWUtNTMyZWZhYTUxMjcy%40thread.v2/0?context=%7b%22Tid%22%3a%224c818f79-ab84-4552-9b7c-2fe715b0d0d5%22%2c%22Oid%22%3a%2277f0ea96-2e96-4a71-85cd-f693c7e6c2cd%22%7d) |
| Clases grabadas viejas | [Link a las clases](https://docs.google.com/spreadsheets/d/1qJqhNKqZBd8fF-mceK9HD0qxr4E0L953vhowUnWI-IM/edit?gid=1693566194#gid=1693566194) |
| Recursos adicionales | [Link a los recursos](https://docs.google.com/spreadsheets/d/1iKuWU7Yc_EEUsqabAkBizfgksrgC1FhG1uCD0RJZB-Q/edit?gid=1608576266#gid=1608576266) |

### Materiales por clase

Cada clase tiene su propia carpeta dentro de `/clases/` con los siguientes archivos:

| Archivo | Descripción |
|---|---|
| `Clase_XX_Slides.pptx` | Diapositivas de la clase teórica |
| `Clase_XX_Practica.pdf` | Consigna de la práctica |
| `Clase_XX_Codigo_Completo.R` | Script completo de referencia |
| `Clase_XX_Codigo_EnVivo.R` | Script que se completa durante la clase en vivo |

---

## 📁 Estructura de carpetas

```
├── README.md
├── clases/
│   ├── clase_00_presentacion/
│   │   └── Clase_00_Presentacion.pptx
│   ├── clase_01_logica_pseudocodigo/
│   │   ├── Clase_01_Slides.pptx
│   │   └── Clase_01_Ejercicios.pdf
│   ├── clase_02_intro_R/
│   │   ├── Clase_02_Slides.pptx
│   │   ├── Clase_02_Practica.pdf
│   │   ├── Clase_02_Codigo_Completo.R
│   │   └── Clase_02_Codigo_EnVivo.R
│   ├── clase_03_.../
│   │   └── ...
│   └── ...
├── datos/
│   └── (datasets utilizados en clase)
├── trabajo_practico/
│   ├── Consignas_TP.md
│   └── Bases_de_Datos_Disponibles.md
└── bibliografia y links de interes/
    └── Bibliografia.md
```

---

## 📅 Fechas importantes

| Fecha | Evento | Link |
|---|---|---|
| 13/03 | Inicio de clases — Presentación del curso | |
| 17/04 | **Instancia 1** — Validación de base de datos e hipótesis | [Formulario 1](https://docs.google.com/forms/d/1rk-iEklDWILKR5eAS9CmdZiuW_alilUKEj8TaMesv0g/edit)| 
| 26/05 | **Instancia 2** — Validación de métodos estadísticos | |
| 12/06 | **Instancia 3** — Validación de visualizaciones | |
| 23/06 | **Entrega final del TP** | |
| 26/06 | **Segunda opción Entrega final del TP** | |
| 30/06 y 03/07 | **Recuperatorios y TP opcional de Web Scraping** | |
| 24/03, 03/04, 01/05 | Feriados | |

Las tres instancias intermedias son de carácter formativo: permiten recibir devolución y corregir el rumbo antes de la entrega final.
Son obligatorias las entregas y forman parte de la nota final con la siguiente ponderación: 10%, 20% y 10%. Tendrán las tres una nota del 1 al 10.

---

## 📚 Unidades temáticas

### Unidad 1 — Programación en R

Introducción al pensamiento computacional y a la programación. Se trabaja primero con lógica y pseudocódigo (sin computadora) para luego pasar a R: tipos de datos, estructuras, funciones, condicionales, loops, y manipulación de datos con `tidyverse` (`dplyr`, `tidyr`, `readr`). Incluye también joins, manejo de strings, fechas, funciones propias y organización de proyectos.

### Unidad 2 — Estadística para Ciencia de Datos

Estadística descriptiva (medidas de tendencia central, dispersión, distribuciones) e inferencial (muestreo, intervalos de confianza, tests de hipótesis). Se cubren los fundamentos de regresión lineal simple y múltiple, y nociones de series de tiempo. El enfoque es conceptual y aplicado — los estudiantes cuentan con materias específicas de econometría para el tratamiento formal. Incluye también tratamiento de datos faltantes, outliers y transformaciones.

### Unidad 3 — Herramientas cuantitativas para economistas

Índices y medidas de uso frecuente en economía: Herfindahl-Hirschman (HHI) y ratios de concentración (CR4/CR8), coeficiente de Gini y curva de Lorenz, Ventajas Comparativas Reveladas (RCA de Balassa), índice de Grubel-Lloyd, e índices de precios (Laspeyres, Paasche, Fisher). Índices complejos para formulación de datos combinados. También se trabajan operaciones habituales como deflactar variables nominales, calcular tasas de variación e indexar series.

### Unidad 4 — Visualización de datos

Principios de diseño gráfico y comunicación visual. Uso de `ggplot2` para la construcción de gráficos estáticos y la gramática de gráficos. Se trabaja sobre customización avanzada (temas, paletas, anotaciones) y narrativa con datos (data storytelling). Incluye un taller práctico de visualización con datos reales.

### Unidad 5 — Web Scraping (módulo opcional)

Introducción a la extracción de datos de la web: estructura de una página HTML, uso de `rvest` para scraping estático, y nociones de APIs. Este módulo se dicta después de la entrega del TP, sin presión evaluativa, como contenido extra.

---

## 🛠️ Software

- [R](https://cran.r-project.org/) + [RStudio](https://posit.co/download/rstudio-desktop/) (entorno principal)
- [Google Colab](https://colab.research.google.com/) (alternativa en la nube, no requiere instalación)

---

## 📖 Bibliografía principal

- Wickham, H., Çetinkaya-Rundel, M., & Grolemund, G. (2023). *R for Data Science* (2nd ed.). O'Reilly. Disponible en [r4ds.hadley.nz](https://r4ds.hadley.nz/) — **Gratuito**
- Diez, D. M., Çetinkaya-Rundel, M., & Barr, C. D. (2019). *OpenIntro Statistics* (4th ed.). OpenIntro. Disponible en [openintro.org](https://www.openintro.org/book/os/) — **Gratuito**

La bibliografía completa se encuentra en el [documento de bibliografía](https://docs.google.com/spreadsheets/d/1iKuWU7Yc_EEUsqabAkBizfgksrgC1FhG1uCD0RJZB-Q/edit?gid=283594947#gid=283594947).

---

## 📊 Evaluación

La evaluación consiste en un **Trabajo Práctico grupal** que se desarrolla a lo largo del cuatrimestre, con tres instancias intermedias de validación y una entrega final:

| Instancia | Qué se evalúa |
|---|---|
| Instancia 1 | Base de datos seleccionada e hipótesis de trabajo |
| Instancia 2 | Métodos estadísticos aplicados |
| Instancia 3 | Visualizaciones y comunicación de resultados |
| Entrega final | Trabajo Práctico integrador completo |

Los estudiantes eligen una base de datos de un [listado de 15 fuentes disponibles](https://docs.google.com/spreadsheets/d/1iKuWU7Yc_EEUsqabAkBizfgksrgC1FhG1uCD0RJZB-Q/edit?gid=2048431813#gid=2048431813), que incluye datos de INDEC, OEDE, BACI-CEPII, Banco Mundial, EPH, CEDLAS/SEDLAC, entre otros.

---

## 🤖 Uso de herramientas de IA

El curso no prohíbe el uso de herramientas de IA (ChatGPT, Claude, Copilot, etc.), sino que lo enmarca con criterios claros. Se provee una [guía de uso](URL_GUIA_IA) que distingue entre niveles de utilización apropiados según la actividad del curso. El criterio general: la IA es una herramienta, no un sustituto del aprendizaje.

---

## 📬 Contacto

Nicolás Sidicaro — [EMAIL](mailto:nsidicaro.fce@gmail.com)
