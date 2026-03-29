# Trabajo Práctico Integrador

## Ciencia de Datos para Economía y Negocios — FCE-UBA

La evaluación de la materia consiste en un **Trabajo Práctico grupal** que se desarrolla a lo largo del cuatrimestre. El trabajo tiene tres instancias intermedias de validación y una entrega final.

Los estudiantes eligen una base de datos de un [listado de 20 fuentes disponibles](https://docs.google.com/spreadsheets/d/1iKuWU7Yc_EEUsqabAkBizfgksrgC1FhG1uCD0RJZB-Q/edit?gid=2048431813#gid=2048431813), que incluye datos de INDEC, OEDE, BACI-CEPII, Banco Mundial, EPH, CEDLAS/SEDLAC, entre otros. A partir de esa base, cada grupo debe plantear una temática de análisis, formular una hipótesis y desarrollarla progresivamente a lo largo de las entregas.

---

## Fechas importantes

| Fecha | Evento |
|---|---|
| 17/04 | **Instancia 1** — Validación de base de datos e hipótesis |
| 26/05 | **Instancia 2** — Validación de métodos estadísticos |
| 12/06 | **Instancia 3** — Validación de visualizaciones |
| 23/06 | **Entrega final del TP** |
| 26/06 | Segunda opción de entrega final del TP |
| 30/06 y 03/07 | Recuperatorios y TP opcional de Web Scraping |

---

## Instancias intermedias

Las tres instancias intermedias son de carácter formativo: permiten recibir devolución del docente y corregir el rumbo antes de la entrega final. Son obligatorias y forman parte de la nota final.

### Instancia 1 — Base de datos e hipótesis (17/04)

En esta primera entrega se espera que cada grupo presente:

1. **Integrantes del grupo.**
2. **Base de datos seleccionada** del listado de fuentes disponibles.
3. **Temática a desarrollar.** Por ejemplo, si eligen el PBG Provincial podrían plantearse explorar la divergencia económica entre provincias o realizar un análisis comparativo entre dos provincias con características similares.
4. **Hipótesis de trabajo.** Una conjetura sobre lo que esperan encontrar al finalizar el análisis. Esta hipótesis debe servir como guía para las etapas sucesivas del trabajo.
5. **Variables principales** que planean utilizar, indicando el tipo de cada una (numérica, categórica, temporal, etc.).
6. **Bases de datos complementarias**, si consideran que pueden enriquecer el análisis.
7. **Benchmarks de comparación** que resulten útiles para contextualizar los resultados. Por ejemplo, la evolución del PIB nacional si trabajan con datos provinciales.
8. **Descripción del proceso de limpieza** necesario para llevar la base de datos en su formato crudo (tal como se descarga) al formato requerido para el análisis ¿es necesario llevarlo a tabla larga? ¿hay que eliminar variables?.

### Instancia 2 — Métodos estadísticos (26/05)

En esta segunda entrega los grupos deben mostrar avance en la exploración de los datos y en la definición de su estrategia analítica. Se espera que presenten:

1. **Exploración inicial de las variables.** ¿Qué forma tienen las distribuciones? ¿Cuáles son los rangos, valores típicos y dispersión de las variables principales?
2. **Diagnóstico de calidad de datos.** ¿Existen valores nulos o faltantes? ¿Se detectan outliers? ¿Cómo planean tratarlos y por qué?
3. **Métodos estadísticos o herramientas cuantitativas** que necesitan para avanzar en la respuesta de su hipótesis y su objetivo de trabajo. Justificar brevemente la elección.

Los estudiantes deben haber incorporado en esta instancia las sugerencias realizadas por el docente en la devolución de la Instancia 1.
La nota dependerá de la profundidad y justificación de cada uno de los puntos mencionados.

### Instancia 3 — Visualizaciones (12/06)

En esta tercera entrega cada grupo debe presentar **al menos dos visualizaciones de datos** que incorporen los elementos de storytelling vistos en el curso. Para cada visualización se debe indicar:

- **Qué rol cumple dentro del trabajo.** ¿Describe la relación entre dos o más variables? ¿Presenta el resultado de una herramienta estadística? ¿Apoya las conclusiones? ¿Sirve para mostrar el tratamiento de valores nulos u outliers?
- **En qué parte de la presentación final se ubicará** y cómo colabora en la construcción del argumento.

Las visualizaciones deben ser funcionales al análisis, no meramente decorativas.

---

## Entrega final (23/06 o 26/06)

La entrega final consta de dos componentes:

### Presentación

Un archivo en formato **PowerPoint (.pptx) o PDF** con la presentación del trabajo completo.

El docente podrá solicitar que los grupos expongan sus trabajos de forma oral. Esto puede requerirse para mejorar la nota necesaria para aprobar o para verificar que los estudiantes realizaron el trabajo comprendiendo los conceptos y no fue resultado exclusivo de herramientas de inteligencia artificial.

### Repositorio

Un repositorio en **GitHub** con los códigos, datos y resultados del trabajo. El repositorio debe respetar la siguiente estructura de carpetas:

```
proyecto/
├── raw/            # Datos originales tal como fueron descargados, sin modificar
├── auxiliar/        # Bases de datos complementarias o archivos de apoyo
├── input/           # Datos procesados y listos para el análisis
├── output/
│   ├── tablas/      # Tablas de resultados generadas por los scripts
│   └── graficos/    # Visualizaciones generadas por los scripts
├── script/          # Scripts de R, cada uno con un objetivo específico
├── utils/           # Funciones propias (un script por función)
└── README.md        # Descripción del proyecto y guía del repositorio
```

**Sobre los scripts:** cada script debe tener un objetivo específico y claro, tal como se trabajó en las primeras clases del curso. Se deben utilizar todas las carpetas de la estructura (tomar datos de raw, guardar los preprocesados en input y guardar los finales en output).

**Sobre las funciones propias:** en caso de crear funciones para automatizar tareas repetitivas, cada función debe alojarse en un script separado dentro de la carpeta `utils/`.

**Sobre el README:** debe estar escrito en formato Markdown e incluir el objetivo del trabajo, la justificación de la base de datos elegida, una descripción de lo realizado, la estructura de carpetas y cualquier instrucción necesaria para reproducir el análisis.

---

## Composición de la nota

| Componente | Peso |
|---|---|
| Instancia 1 | 10% |
| Instancia 2 | 20% |
| Instancia 3 | 10% |
| **Total entregas intermedias** | **40%** |
| Entrega final | 60% |

Dentro de la entrega final, un 10% de su ponderación depende de la incorporación de los comentarios y sugerencias que el docente realizó sobre las entregas intermedias. Se valorará el proceso de mejora entre las instancias y el resultado final.

---

## Reglas generales

- **Todas las entregas son obligatorias** y deben estar aprobadas para aprobar la materia.
- En caso de obtener una calificación no aprobada en una entrega intermedia, se podrá recuperar **una sola** de las tres instancias en las fechas de recuperatorio.
- Todas las entregas recibirán **devolución del docente** en los días siguientes a su envío.
- La entrega final tendrá su devolución en formato de **grilla de evaluación**, detallando cada punto de interés y si se evidenció un proceso de mejora entre las entregas intermedias y el resultado final.
