# =============================================================================
# Ciencia de Datos para Economía y Negocios - FCE-UBA
# Clase 12d: Expresiones regulares con {stringr}
#            aplicadas a nomencladores de actividad económica
# -----------------------------------------------------------------------------
# Contenidos:
#   1. Por qué regex importa para datos económicos argentinos
#   2. Dataset de ejemplo: códigos CLANAE-2018
#   3. Funciones básicas de stringr
#   4. Limpieza de descripciones (espacios, mayúsculas, abreviaturas)
#   5. Códigos jerárquicos: extracción de niveles (sección, división, etc.)
#   6. Padding y normalización de códigos numéricos
#   7. Patrones avanzados: clases de caracteres, cuantificadores, anclajes
#   8. Captura de grupos y backreferences
#   9. Cruce con otros nomencladores (HS para comercio exterior)
#  10. Ejercicios
# -----------------------------------------------------------------------------
# Por qué este tema:
#   En economía aplicada en Argentina trabajamos rutinariamente con códigos
#   de actividad económica: CLANAE (INDEC), CIIU/ISIC (Naciones Unidas),
#   CAES (Encuestas), HS (comercio exterior), CUCI/SITC (BACI-CEPII).
#   Estos códigos llegan en formatos imperfectos: con/sin ceros a la
#   izquierda, descripciones en MAYÚSCULAS con espacios dobles,
#   abreviaturas inconsistentes, codificaciones rotas. Las regex con
#   {stringr} son la herramienta estándar para limpiarlos.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Preparación del entorno
# -----------------------------------------------------------------------------

library(tidyverse)
# stringr ya viene incluido en tidyverse, pero lo declaramos para que se
# entienda que es el paquete protagonista de esta clase.
library(stringr)

options(scipen = 999)


# =============================================================================
# 1. ¿POR QUÉ REGEX?
# =============================================================================
#
# Las "expresiones regulares" (regex, regexp) son un mini-lenguaje para
# describir PATRONES de texto. En lugar de buscar literalmente "Argentina",
# podemos buscar "cualquier palabra que empiece con A y termine en a", o
# "cualquier secuencia de 4 dígitos seguida de un punto y 2 dígitos más".
#
# Para datos económicos esto es decisivo. Tres ejemplos de la vida real:
#
#   1. AFIP exporta el código de actividad como entero. Las actividades
#      del agro empiezan con 0 (ej: 011110 = cultivo de arroz), pero al
#      ser entero, R lo lee como 11110. Hay que rellenar con un cero
#      adelante para hacer joins. -> str_pad()
#
#   2. Los datos de OEDE traen las descripciones en MAYÚSCULAS, con
#      "FABRIC. DE  PARTES  Y PIEZAS" (mayúsculas, abreviatura, espacios
#      dobles). Para mostrarlas en un gráfico necesitamos
#      "Fabricación de partes y piezas". -> str_to_title(), str_replace_all(),
#      str_squish().
#
#   3. Queremos quedarnos solo con las actividades de la sección C
#      (industria manufacturera), que en CLANAE son los códigos que
#      empiezan por 10 a 33. -> str_detect() con un patrón de rango.
#
# Vamos a ver las tres cosas (y bastante más) sobre un dataset de ejemplo
# que reproduce los problemas típicos de los nomencladores reales.
# =============================================================================


# =============================================================================
# 2. DATASET DE EJEMPLO: CÓDIGOS CLANAE-2018
# =============================================================================
#
# CLANAE = Clasificador Nacional de Actividades Económicas (INDEC, AFIP).
# Es la versión argentina del CIIU rev. 4 / ISIC rev. 4 de Naciones Unidas.
# Estructura jerárquica:
#   - Sección  : 1 letra      (A = agro, C = manufactura, F = construcción...)
#   - División : 2 dígitos    (10 a 33 = secciones de manufactura)
#   - Grupo    : 3 dígitos
#   - Clase    : 4 dígitos
#   - Subclase : 6 dígitos    (máximo desagregado, lo que usa AFIP)
#
# Construimos un dataset que replica problemas habituales:
#   - códigos numéricos sin cero a la izquierda
#   - descripciones en MAYÚSCULAS, con abreviaturas y espacios dobles
#   - filas con "n.c.p." (no clasificadas previamente) que aparecen siempre
#   - códigos que no respetan el formato de 6 dígitos
# =============================================================================

clanae <- tribble(
  ~codigo_num,   ~descripcion,
   11110,        "CULTIVO DE ARROZ",
   11120,        "CULTIVO DE  TRIGO",
   11200,        "CULTIVO DE LEGUMBRES FRESCAS",
   105020,       "ELABORACION DE QUESOS",
   105030,       "ELAB. DE HELADOS",
   192000,       "FABRIC. DE PRODUCTOS DERIVADOS DEL PETROLEO",
   282990,       "FABRICACION DE OTRA MAQUINARIA  DE USO GENERAL N.C.P.",
   293010,       "FABRICACION DE PARTES, PIEZAS  Y ACCESORIOS PARA VEHICULOS AUTOMOTORES",
   451110,       "VENTA DE AUTOS Y CAMIONETAS NUEVOS",
   620100,       "SERVICIOS DE PROGRAMACION INFORMATICA",
   620900,       "SERVICIOS DE INFORMATICA N.C.P.",
   711009,       "SERVICIOS DE ARQUITECTURA E INGENIERIA N.C.P.",
   841100,       "ADMINISTRACION PUBLICA EN GENERAL"
)

clanae

# Notar: codigo_num es INTEGER, no character. Las primeras tres filas
# (cultivo de arroz, trigo, legumbres) deberían ser 011110, 011120, 011200,
# pero al leerse como entero perdieron el cero a la izquierda.


# =============================================================================
# 3. FUNCIONES BÁSICAS DE {stringr}
# =============================================================================
#
# Las funciones de stringr empiezan TODAS con `str_`. Esa convención es
# muy útil: con autocompletar (Tab) ven todas las opciones disponibles.
# Las que usamos al 90 %:
#
#   str_length()         largo de cada string
#   str_to_lower()       todo a minúsculas
#   str_to_upper()       todo a mayúsculas
#   str_to_title()       Capitaliza Cada Palabra
#   str_trim()           saca espacios al inicio y al final
#   str_squish()         saca espacios al inicio, final y reduce dobles
#   str_detect()         devuelve TRUE/FALSE por fila si hay match
#   str_extract()        extrae el primer match (o NA)
#   str_extract_all()    extrae todos los matches (devuelve lista)
#   str_replace()        reemplaza el primer match
#   str_replace_all()    reemplaza todos los matches
#   str_sub()            extrae sub-cadena por posición (no es regex)
#   str_pad()            rellena con caracteres a una longitud fija
#   str_split()          parte un string por un delimitador
#   str_count()          cuenta cuántas veces aparece un patrón
#   str_starts(), str_ends()    booleanos para inicio/final
#
# Ejemplos rápidos sin regex (solo strings literales):
# =============================================================================

x <- c("FABRIC. DE QUESOS", "  Cultivo de Trigo  ", "Servicios n.c.p.")

str_length(x)            # 17, 19, 16
str_to_lower(x)
str_to_upper(x)
str_to_title(x)          # capitaliza cada palabra
str_trim(x)              # saca espacios al borde
str_squish(x)            # saca al borde Y reduce dobles internos


# =============================================================================
# 4. LIMPIEZA DE DESCRIPCIONES
# =============================================================================
#
# La descripción que llega de OEDE/AFIP suele tener:
#   1. Todo en MAYÚSCULAS.
#   2. Espacios dobles (a veces triples) por culpa de exports a CSV.
#   3. Abreviaturas inconsistentes: "FABRIC.", "ELAB.", "SERV.".
#   4. La sigla "N.C.P." (no clasificadas previamente) que es ruido visual.
#   5. Comas o puntuación irregular.
# =============================================================================


# -----------------------------------------------------------------------------
# 4.1 Mayúsculas/minúsculas y espacios
# -----------------------------------------------------------------------------

clanae_limpio <- clanae |>
  mutate(
    descripcion_limpia = descripcion |>
      str_to_lower() |>          # todo a minúsculas
      str_squish() |>            # saca espacios dobles, deja uno
      str_to_sentence()          # "Cultivo de arroz" (capitaliza la primera)
  )

clanae_limpio |> select(descripcion, descripcion_limpia)

# str_to_sentence vs str_to_title:
#   - title:    "Cultivo De Arroz"  (capitaliza cada palabra)
#   - sentence: "Cultivo de arroz"  (capitaliza solo la primera, como un
#                                    título de oración en castellano)
# Para descripciones de actividad casi siempre queremos sentence.


# -----------------------------------------------------------------------------
# 4.2 Reemplazar abreviaturas: el primer regex
# -----------------------------------------------------------------------------
#
# Queremos pasar "fabric. de" -> "fabricación de", "elab. de" -> "elaboración
# de", "serv. de" -> "servicios de". str_replace_all() nos deja hacerlo
# todo en una pasada con un VECTOR DE REEMPLAZOS.
#
# Notación: el primer argumento es el patrón, el segundo es lo que va en
# su lugar. Si pasamos un vector con nombres, cada nombre es un patrón
# y su valor es el reemplazo.
# -----------------------------------------------------------------------------

reemplazos <- c(
  "\\bfabric\\."  = "fabricación",
  "\\belab\\."    = "elaboración",
  "\\bserv\\."    = "servicios",
  "\\badmin\\."   = "administración",
  " n\\.c\\.p\\." = ""              # eliminamos la sigla n.c.p.
)

# Atención a los puntos: en regex, "." significa "cualquier caracter".
# Para que signifique punto literal hay que escaparlo con doble backslash:
# "\\.". El doble backslash es porque en R "\\" representa un solo "\".

# El "\\b" es un ANCLA de palabra: matchea la frontera entre una letra y
# algo que no es letra. Sirve para NO matchear "fabric." dentro de
# "metalfabric." (no es nuestro caso, pero es buena costumbre).

clanae_limpio <- clanae_limpio |>
  mutate(
    descripcion_limpia = descripcion_limpia |>
      str_replace_all(reemplazos) |>
      str_squish() |>             # por si quedaron espacios dobles
      str_to_sentence()
  )

clanae_limpio |> select(descripcion, descripcion_limpia)

# Esta misma idea (vector de reemplazos con nombres) sirve para limpiar
# nombres de provincias, monedas, países, etc. En lugar de encadenar 10
# str_replace, definimos UN diccionario y lo aplicamos.


# =============================================================================
# 5. PADDING Y NORMALIZACIÓN DE CÓDIGOS NUMÉRICOS
# =============================================================================
#
# El problema clásico: los códigos vienen como integer y perdieron el cero
# a la izquierda. Para hacer joins con un nomenclador completo (que tiene
# los códigos como texto de 6 dígitos), hay que reconstruir el formato.
# =============================================================================


# -----------------------------------------------------------------------------
# 5.1 str_pad(): rellenar con ceros
# -----------------------------------------------------------------------------
# Argumentos:
#   width = 6          longitud final deseada
#   pad   = "0"        caracter de relleno
#   side  = "left"     desde qué lado rellenar
# -----------------------------------------------------------------------------

clanae_limpio <- clanae_limpio |>
  mutate(
    codigo = str_pad(as.character(codigo_num),
                     width = 6, pad = "0", side = "left")
  )

clanae_limpio |> select(codigo_num, codigo, descripcion_limpia)

# Verificación: todos deben tener 6 caracteres ahora.
clanae_limpio |>
  mutate(longitud = str_length(codigo)) |>
  count(longitud)


# -----------------------------------------------------------------------------
# 5.2 Validar el formato del código
# -----------------------------------------------------------------------------
#
# Antes de seguir, conviene chequear que efectivamente todos los códigos
# son cadenas de 6 dígitos. Lo hacemos con str_detect() y un patrón regex.
#
#   ^      : ancla "inicio del string".
#   \\d    : un dígito (0-9).
#   {6}    : exactamente 6 veces.
#   $      : ancla "fin del string".
#
# Es decir: ^\\d{6}$ = "del principio al fin, exactamente seis dígitos".
# Sin los anclajes, "12345678" también matchearía (porque contiene seis
# dígitos seguidos), y eso no es lo que queremos.
# -----------------------------------------------------------------------------

clanae_limpio |>
  mutate(es_valido = str_detect(codigo, "^\\d{6}$")) |>
  count(es_valido)

# Si algún código viene con letras, espacios o longitud distinta, esto
# lo marca al toque. En datos reales de AFIP/OEDE pasa con frecuencia
# (códigos legacy, errores de export).


# =============================================================================
# 6. CÓDIGOS JERÁRQUICOS: EXTRACCIÓN DE NIVELES
# =============================================================================
#
# Esta es la operación que MÁS se hace cuando se trabaja con CLANAE/CIIU.
# A partir del código de 6 dígitos queremos derivar todos los niveles
# superiores para poder agregar a distintas resoluciones.
# =============================================================================


# -----------------------------------------------------------------------------
# 6.1 str_sub() para niveles por posición
# -----------------------------------------------------------------------------
# str_sub(x, start, end) extrae caracteres por posición (1-indexado).
# Es la herramienta natural para códigos jerárquicos posicionales como
# CLANAE, CIIU o HS, donde los primeros n dígitos definen el nivel n.
# -----------------------------------------------------------------------------

clanae_niveles <- clanae_limpio |>
  mutate(
    division   = str_sub(codigo, 1, 2),    # 2 dígitos
    grupo      = str_sub(codigo, 1, 3),    # 3 dígitos
    clase      = str_sub(codigo, 1, 4),    # 4 dígitos
    subclase   = codigo                    # 6 dígitos (el código completo)
  ) |>
  select(codigo, division, grupo, clase, subclase, descripcion_limpia)

clanae_niveles


# -----------------------------------------------------------------------------
# 6.2 La sección (letra) a partir de la división
# -----------------------------------------------------------------------------
#
# La sección de CLANAE es una letra que NO está en el código numérico:
# hay que asignarla a partir del rango de la división. Es decir:
#   01–03 -> A (agro, ganadería, pesca)
#   05–09 -> B (minería)
#   10–33 -> C (industria manufacturera)
#   35    -> D (electricidad y gas)
#   36–39 -> E (agua y saneamiento)
#   41–43 -> F (construcción)
#   ...
#
# Esto se resuelve con case_when() sobre el valor numérico de la división.
# No es regex, pero es la otra mitad del trabajo: regex para EXTRAER,
# case_when para CLASIFICAR.
# -----------------------------------------------------------------------------

clanae_niveles <- clanae_niveles |>
  mutate(
    div_num = as.integer(division),
    seccion = case_when(
      div_num %in% 1:3    ~ "A - Agro, ganadería, silvicultura y pesca",
      div_num %in% 5:9    ~ "B - Explotación de minas y canteras",
      div_num %in% 10:33  ~ "C - Industria manufacturera",
      div_num == 35       ~ "D - Suministro de electricidad y gas",
      div_num %in% 36:39  ~ "E - Suministro de agua y saneamiento",
      div_num %in% 41:43  ~ "F - Construcción",
      div_num %in% 45:47  ~ "G - Comercio al por mayor y al por menor",
      div_num %in% 49:53  ~ "H - Transporte y almacenamiento",
      div_num %in% 55:56  ~ "I - Alojamiento y servicios de comida",
      div_num %in% 58:63  ~ "J - Información y comunicaciones",
      div_num %in% 64:66  ~ "K - Actividades financieras",
      div_num %in% 68     ~ "L - Actividades inmobiliarias",
      div_num %in% 69:75  ~ "M - Servicios profesionales y técnicos",
      div_num %in% 77:82  ~ "N - Servicios administrativos",
      div_num %in% 84     ~ "O - Administración pública",
      div_num %in% 85     ~ "P - Enseñanza",
      div_num %in% 86:88  ~ "Q - Salud humana y servicios sociales",
      TRUE                ~ "Otra / no asignada"
    )
  )

clanae_niveles |> select(codigo, division, seccion, descripcion_limpia)


# -----------------------------------------------------------------------------
# 6.3 Filtrar por sección con regex
# -----------------------------------------------------------------------------
#
# Una vez que tenemos el código padeado, podemos filtrar por nivel sin
# necesidad de pasar por la división. Por ejemplo, todas las actividades
# de manufactura son las que tienen división entre 10 y 33.
#
# Patrón: el código empieza con "1", "2" o "30"-"33".
# Una forma elegante con regex:
#   "^(1\\d|2\\d|3[0-3])"
#
# Lectura del patrón:
#   ^         inicio del string
#   ( ... )   alternativas, separadas por |
#   1\\d      "1" seguido de un dígito (10 a 19)
#   2\\d      "2" seguido de un dígito (20 a 29)
#   3[0-3]    "3" seguido de un dígito entre 0 y 3 (30 a 33)
# -----------------------------------------------------------------------------

clanae_niveles |>
  filter(str_detect(codigo, "^(1\\d|2\\d|3[0-3])")) |>
  select(codigo, descripcion_limpia)

# Útil sabido: para extraer SOLO la parte que matcheó usamos str_extract().
clanae_niveles |>
  mutate(prefijo_division = str_extract(codigo, "^\\d{2}")) |>
  count(prefijo_division)


# =============================================================================
# 7. PATRONES AVANZADOS: CLASES, CUANTIFICADORES Y ANCLAJES
# =============================================================================
#
# Vamos a sistematizar los componentes del lenguaje regex. Es el momento
# de tenerlos juntos como referencia.
#
# CLASES DE CARACTERES:
#   .         cualquier caracter (excepto salto de línea)
#   \\d       dígito (0-9)             ; \\D = NO dígito
#   \\s       espacio en blanco        ; \\S = NO espacio
#   \\w       letra/dígito/guión bajo  ; \\W = el complemento
#   [abc]     a, b o c
#   [^abc]    cualquier caracter EXCEPTO a, b, c
#   [a-z]     rango: cualquier letra minúscula
#   [0-9]     rango: cualquier dígito (equivalente a \\d)
#
# CUANTIFICADORES (cuántas veces se repite el elemento anterior):
#   ?         0 o 1
#   *         0 o más
#   +         1 o más
#   {n}       exactamente n
#   {n,}      n o más
#   {n,m}     entre n y m
#
# ANCLAJES (no consumen caracteres, marcan posición):
#   ^         inicio del string
#   $         fin del string
#   \\b       frontera de palabra
#
# OPERADORES:
#   |         alternancia (uno u otro)
#   ()        agrupar (también captura)
#   (?:...)   agrupar SIN capturar
#
# MODIFICADORES (en stringr, vía la función regex()):
#   regex(p, ignore_case = TRUE)   insensible a mayúsculas
#   regex(p, dotall = TRUE)        el . matchea también \n
# =============================================================================


# -----------------------------------------------------------------------------
# 7.1 Detectar actividades agropecuarias
# -----------------------------------------------------------------------------
# Las divisiones 01 a 03 son agro/pesca. Como nuestros códigos están
# padeados a 6 dígitos, todas empiezan con "01", "02" o "03".

agro <- clanae_limpio |>
  filter(str_detect(codigo, "^0[1-3]"))

agro |> select(codigo, descripcion_limpia)


# -----------------------------------------------------------------------------
# 7.2 Encontrar descripciones que mencionen vehículos
# -----------------------------------------------------------------------------
# Patrón: la palabra "vehículo" o "vehiculo" (con o sin tilde), en cualquier
# parte del texto, ignorando mayúsculas. Lo escribimos con [iíu] para
# tolerar variantes de tildes (clásico problema de encoding en datos AFIP).

clanae |>
  filter(str_detect(descripcion,
                    regex("veh[ií]culo", ignore_case = TRUE))) |>
  select(descripcion)

# Truco: regex(p, ignore_case = TRUE) es mejor que escribir [Vv][Ee]...
# para hacer una búsqueda insensible a mayúsculas.


# -----------------------------------------------------------------------------
# 7.3 Detectar actividades "n.c.p." (no clasificadas previamente)
# -----------------------------------------------------------------------------
# La sigla puede aparecer como "n.c.p.", "N.C.P.", "ncp" o "n c p". Vamos
# a tolerar todas con un patrón flexible.
#
# Patrón: "n", opcionalmente un punto o espacio, "c", lo mismo, "p".
# Punto o espacio = clase [\\. ], opcional con ?.

clanae |>
  mutate(es_ncp = str_detect(descripcion,
                             regex("n[\\. ]?c[\\. ]?p\\.?",
                                   ignore_case = TRUE))) |>
  select(descripcion, es_ncp)

# Las "n.c.p." son una bandera importante: indican un cajón de sastre
# dentro del nomenclador y suelen ser candidatas a revisarse manualmente.


# -----------------------------------------------------------------------------
# 7.4 Códigos especiales: extraer "PEN" o "PVR" de un campo libre
# -----------------------------------------------------------------------------
# A veces los datasets vienen con un código alfanumérico irregular en una
# misma columna (típico de bases SRT, ART, o exports rotos de SIPA donde
# se mezcla código de actividad con sufijos legacy).
# Inventemos un caso así para mostrar el patrón:

ejemplo <- c("282990-PEN", "451100", "192000_PVR_2017", "XXX-no_aplica")

# Extraer el código numérico de 6 dígitos (si lo hay):
str_extract(ejemplo, "\\d{6}")

# Detectar los que tienen sufijo alfabético en mayúsculas:
str_detect(ejemplo, "[A-Z]{3,}")

# Extraer todos los grupos de letras mayúsculas de 3 o más caracteres:
str_extract_all(ejemplo, "[A-Z]{3,}")
# Devuelve una LISTA porque cada elemento puede tener cero, uno o varios
# matches. Si querés un vector plano, mejor usar str_extract() (que devuelve
# solo el primero) o unnest_longer() después.


# =============================================================================
# 8. CAPTURA DE GRUPOS Y BACKREFERENCES
# =============================================================================
#
# Los paréntesis en regex hacen dos cosas a la vez:
#   - AGRUPAN para aplicar cuantificadores o alternancias.
#   - CAPTURAN: el match queda guardado y se puede reusar.
#
# Backreferences: en el reemplazo, \\1 refiere al primer grupo capturado,
# \\2 al segundo, etc. Es muy útil para reordenar partes de un string.
# =============================================================================


# -----------------------------------------------------------------------------
# 8.1 Extraer los componentes jerárquicos con un solo patrón
# -----------------------------------------------------------------------------
# str_match() devuelve una matriz: la primera columna es el match completo,
# las siguientes son los grupos capturados.
#
# Patrón: dos dígitos (división), luego dos dígitos (resto del grupo+clase),
# luego dos dígitos (los últimos dos, que diferencian la subclase).

componentes <- str_match(clanae_limpio$codigo,
                         "^(\\d{2})(\\d{2})(\\d{2})$")
head(componentes)

# Convertimos a tibble para verlo claro:
clanae_limpio |>
  mutate(
    division_re = str_match(codigo, "^(\\d{2})(\\d{2})(\\d{2})$")[, 2],
    medio_re    = str_match(codigo, "^(\\d{2})(\\d{2})(\\d{2})$")[, 3],
    final_re    = str_match(codigo, "^(\\d{2})(\\d{2})(\\d{2})$")[, 4]
  ) |>
  select(codigo, division_re, medio_re, final_re) |>
  head()

# Para este caso particular str_sub es más limpio (es regex de más). Pero
# str_match() es la herramienta cuando los componentes NO son posicionales
# fijos sino que dependen del patrón (separadores, sufijos opcionales, etc.).


# -----------------------------------------------------------------------------
# 8.2 Reformatear códigos con backreferences
# -----------------------------------------------------------------------------
# Supongamos que querés mostrar el código con puntos como separador entre
# niveles: "192000" -> "19.20.00".
# El patrón captura los tres pares de dígitos y el reemplazo los reinserta
# con puntos:
#   patrón     : ^(\\d{2})(\\d{2})(\\d{2})$
#   reemplazo  : \\1.\\2.\\3

clanae_limpio |>
  mutate(codigo_dotted = str_replace(codigo,
                                     "^(\\d{2})(\\d{2})(\\d{2})$",
                                     "\\1.\\2.\\3")) |>
  select(codigo, codigo_dotted) |>
  head()


# -----------------------------------------------------------------------------
# 8.3 Caso real: extraer el código del sistema armonizado (HS) con sufijos
# -----------------------------------------------------------------------------
# En BACI-CEPII y en datos de aduanas argentinas, las posiciones arancelarias
# vienen a veces como "1001.99.10" (HS6 con dos sufijos nacionales) y a
# veces como "100199" o "1001990010". Queremos quedarnos con los primeros
# 6 dígitos (HS6) sin importar el formato.

aduana <- c(
  "1001.99.10",       # con puntos
  "100199",           # solo HS6
  "1001990010",       # HS10 sin puntos
  "1001-99-10",       # con guiones
  "AR1001.99.10"      # con prefijo país
)

# Estrategia: sacar todo lo que NO sea dígito y quedarnos con los primeros 6.
hs6 <- aduana |>
  str_remove_all("\\D") |>      # \\D = "todo lo que NO es dígito"
  str_sub(1, 6)

hs6

# Otra forma, más estricta, usando captura:
str_match(aduana, "(\\d{4})\\.?(\\d{2})")[, c(2, 3)]
# Esto captura los primeros 4 dígitos y los siguientes 2 (con o sin punto
# entre medio). Útil cuando hay sufijos que NO queremos tocar.


# =============================================================================
# 9. CRUCE CON OTROS NOMENCLADORES
# =============================================================================
#
# CIIU rev. 4 e ISIC rev. 4 son idénticos a CLANAE-2018 hasta el nivel
# de clase (4 dígitos). A partir de ahí, CLANAE agrega la "subclase
# argentina" con dos dígitos extra (las posiciones 5-6).
#
# Esto significa que, si tenemos un dataset con códigos CIIU de 4 dígitos
# (típico de comparaciones internacionales con UNIDO, ILOSTAT o WIOD) y
# queremos cruzarlo con datos argentinos (SIPA, OEDE, AFIP), basta con
# truncar el CLANAE de 6 dígitos a sus primeros 4.
# =============================================================================


# -----------------------------------------------------------------------------
# 9.1 Construir la tabla de equivalencia CLANAE -> CIIU
# -----------------------------------------------------------------------------

equivalencia <- clanae_limpio |>
  transmute(
    clanae_6      = codigo,
    ciiu_4        = str_sub(codigo, 1, 4),    # = clase
    descripcion   = descripcion_limpia
  )

equivalencia |> head()


# -----------------------------------------------------------------------------
# 9.2 Caso típico: agregar empleo CLANAE a la resolución CIIU para comparar
# -----------------------------------------------------------------------------
# Inventemos datos de empleo por código CLANAE-6:

empleo <- tibble(
  clanae_6 = c("011110", "011120", "011200", "105020", "105030",
               "192000", "282990", "293010", "451110", "620100"),
  empleo   = c(8500, 12000, 4300, 9800, 2100, 18500, 7200, 25400, 14600, 33800)
)

# Agregamos al nivel CIIU-4 con un mutate + group_by:
empleo_ciiu <- empleo |>
  mutate(ciiu_4 = str_sub(clanae_6, 1, 4)) |>
  group_by(ciiu_4) |>
  summarise(empleo_total = sum(empleo), .groups = "drop")

empleo_ciiu

# Este patrón (truncar el código y agregar) es la operación FUNDAMENTAL
# cuando se trabaja con clasificadores jerárquicos. Sirve igual para
# CLANAE -> CIIU, HS10 -> HS6, NACE -> NACE-2D, etc.


# =============================================================================
# 10. EJERCICIOS
# =============================================================================
#
# -----------------------------------------------------------------------------
# Ejercicio 1: Limpieza de descripciones
# -----------------------------------------------------------------------------
# Construir un vector de reemplazos que limpie también:
#   - "PROD." -> "productos"
#   - "EXPL." -> "explotación"
#   - "VTA."  -> "venta"
#   - "MAQ."  -> "maquinaria"
# Aplicarlo al dataset clanae y verificar el resultado.
#
# -----------------------------------------------------------------------------
# Ejercicio 2: Validación de códigos
# -----------------------------------------------------------------------------
# Crear un vector con códigos "sucios" mezclando válidos e inválidos:
#
#   codigos_test <- c("011110", "11110", "ABC123", "0111100", "01-11-10",
#                     "011110 ", " 011110", "011a10")
#
# Escribir una función que reciba el vector y devuelva un tibble con:
#   - el código original
#   - una columna lógica "es_valido" (TRUE si y solo si tiene exactamente
#     6 dígitos, sin nada más)
#   - una columna "tipo_problema" que diga qué tiene de mal cuando no es
#     válido (longitud incorrecta, contiene letras, contiene espacios,
#     contiene separadores, etc.).
#
# Pista: usen str_detect() con varios patrones distintos.
#
# -----------------------------------------------------------------------------
# Ejercicio 3: Agregación a CIIU desde códigos sin padear
# -----------------------------------------------------------------------------
# El equipo de SRT te pasa una tabla de empleo donde el código de actividad
# vino como integer (sin cero a la izquierda). Construirla así:
#
#   empleo_srt <- tibble(
#     codigo = c(11110, 11120, 105020, 192000, 282990),
#     empleo = c(5000,  7800,  3200,   12100, 8400)
#   )
#
# Tarea: padear el código a 6 dígitos, derivar la división (2 dígitos)
# y la sección CLANAE (letra A-Z), y hacer un summarise() del empleo total
# por sección.
#
# -----------------------------------------------------------------------------
# Ejercicio 4: HS y posiciones arancelarias
# -----------------------------------------------------------------------------
# Dado el vector:
#
#   posiciones <- c("8703.23.10.100A",
#                   "AR8703.23.10",
#                   "8703231",
#                   "8703.23",
#                   "MERCOSUR-8703.23.10")
#
# Escribir una expresión (con stringr) que devuelva el HS6 (primeros 6
# dígitos) cuando el código contenga al menos esa cantidad de dígitos, y
# NA cuando no. Usar una sola pipeline.
#
# Pista: combinar str_remove_all() para sacar lo no-numérico, str_length()
# para validar, e ifelse() (o case_when) para devolver NA en los inválidos.
#
# -----------------------------------------------------------------------------
# Ejercicio 5 (avanzado): Detectar palabras clave en descripciones
# -----------------------------------------------------------------------------
# Construir un patrón regex que detecte si una descripción menciona
# CUALQUIERA de los siguientes términos (con o sin tilde, en cualquier
# capitalización):
#   - hidrocarburos
#   - petróleo / petroleo
#   - gas natural
#   - refinación / refinacion
#
# Aplicar el patrón al campo descripcion del dataset clanae_limpio y
# generar una variable lógica "es_hidrocarburos". Esta misma técnica es
# la que se usa para construir clasificaciones ad-hoc (proxy de cadenas
# productivas, agrupaciones temáticas) cuando el nomenclador oficial no
# alcanza.
#
# =============================================================================
# Ideas clave para llevarse:
# -----------------------------------------------------------------------------
#
#   - {stringr} es la interfaz amigable a regex en R; las funciones empiezan
#     todas con `str_` y son consistentes en su orden de argumentos.
#   - En regex, los anclajes ^ y $ son CRUCIALES para no matchear de más.
#   - Para códigos jerárquicos (CLANAE, CIIU, HS), str_sub() y str_pad()
#     resuelven el 80 % de los casos; str_match() y los grupos de captura
#     resuelven el resto.
#   - Construir un vector con NOMBRES (patrón = reemplazo) y aplicarlo con
#     str_replace_all() es la forma estándar de mantener un "diccionario
#     de limpieza" reusable entre proyectos.
#   - SIEMPRE escapar los puntos con \\. cuando se quiera matchear punto
#     literal (en versiones, en separadores de codigos, en n.c.p., etc.).
#   - Para insensibilidad a mayúsculas y a tildes, regex(p, ignore_case=TRUE)
#     y clases como [aá] son más legibles que repetir variantes.
#
# =============================================================================
