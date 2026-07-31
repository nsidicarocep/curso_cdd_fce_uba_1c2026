# Scraper de datos accciones de FinViz 

# Cargar librerias de scraper 
library(rvest)
library(tidyverse)

# Definir 
accion <- c('YPF','TSLA','T','NVDA','WETO','REPL')


# Loopear sobre las acciones 
df <- tibble()
for(i in 1:length(accion)){
  # Leer HTML 
  pagina <- read_html(paste0('https://finviz.com/stock?t=',accion[i],'&p=d'))
  
  # Levantar la informacion de la tabla de estadistica clave 
  estad <- pagina %>% 
    html_elements(css = '[class="table-dark-row"]') %>% 
    html_text2()
  
  # Para limpiar los datos usamos un prompt en Claude usando Sonnet 5
  # Prompt: Dame un código de R que me permita con expresiones regulares (usando el paquete stringr) limpiar la información de estos datos
  
  # Limpiar los datos 
  tmp <- tibble(raw = estad) %>%
    mutate(
      # Extrae label y valor usando grupos de captura
      match  = str_match(raw, "^(.*?)\\n\\t\\n(.*?)\\n\\t$"),
      label  = str_trim(match[, 2]),
      valor  = str_squish(match[, 3])  # colapsa espacios múltiples y trimea
    ) %>%
    select(label, valor)
  
  # Casos vacíos (ej. "Trades") quedan como "" -> los paso a NA
  tmp <- tmp %>%
    mutate(valor = na_if(valor, ""))
  
  # Agregar el nombre de la accion
  tmp <- tmp %>% 
    mutate(empresa = accion[i])
  
  # Completar df 
  df <- bind_rows(df,tmp)
  print(i)
  Sys.sleep(1)
}

