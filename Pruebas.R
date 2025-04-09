library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
datos <- read_xlsx("Archivos/adm.xlsx") |> 
  mutate(id = 1:n())


reglas <-  tribble(
  ~id,  ~desc,                                      ~cond,          
  "r1", "pais fuera de la lista",                   "!(countrycode %in% c(4, 11, 14, 23, 31, 48, 54, 65, 72, 97))",
  "r2", "ID en formato DD/MM/YYYY-AA",              "stringr::str_detect(patientid,pattern = '^[:digit:]{2}/[:digit:]{2}/[:digit:]{4}-[:upper:]{2}$', negate = T) | is.na(dmy(substr(patientid,1,10),quiet = T))",
  "r3", "Fecha de entrevista en formato DD/MM/YYYY","stringr::str_detect(patientid,pattern = '^[:digit:]{2}/[:digit:]{2}/[:digit:]{4}$', negate = T) | is.na(dmy(interview,quiet = T))",
  "r4", "Fechas congruentes(edad > 18)",            "as.numeric(dmy(interview,quiet = T) - dmy(substr(patientid,1,10),quiet = T))/365 < 18",
  "r5", "Grupo étnico entre las opciones",          "!(ethnicgroup %in% c(1,2,3,4))",
  "r6", "Elegible SCR dentro de opciones",          "!(scr %in% c(1,2))",
  "r7", "Elegible USSCR dentro de opciones",        "!(usscr %in% c(1,2))",
  "r8", "Consentimiento dentro de opciones",        "!(consent %in% c(1,2))",
  "r9", "Número del sujeto en el formato adecuado", "stringr::str_detect(subjectnumber,pattern = '^[:digit:]{9}$', negate = T)",
  "r10", "Enrolamiento correcto",                   "scr != 2 | usscr != 2 | consent != 2",
  "r11", "Número del sujeto correcto",              "substr(subjectnumber, 1, 3) != paste0(0,countrycode) & substr(subjectnumber, 1, 3) != paste0(0, 0, countrycode)",
  "r12", "Grupo étnico respondido",                 "is.na(ethnicgroup)",
  "r13", "Número del sujeto completado",            "is.na(subjectnumber)",
  "r14", "Código de país completado",               "is.na(countrycode)"
  )

# ==============================================================================
#                                   VALIDACIÓN
# ==============================================================================

# Funcion validador()
# argumentos:
# - datos: conjunto de validacion
# - id : nombre de la columna en (datos) con el identificador
# - cond : nombre de la columna en (datos) con la condicion
# salida: vector nombrado
validador = function(datos, id, cond){
  reglas = datos[[cond]]
  names(reglas) = datos[[id]]
  reglas
}

# Funcion validar()
# argumentos:
# - datos : conjunto de datos a validar
# - id : nombre de la columna en (datos) con el identificador
# - validador: salida de validador()
# salida: tibble con el resultado de la validación
validar = function(datos, id, validador){
  sapply(
    validador,
    function(x) eval(parse(text = x), datos)
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(registro = datos[[id]], .before = 0)
}


validacion <-  validar(datos, "id", validador(reglas, "id", "cond"))




# GRÁFICO por la gran cantidad de datos
# Para poder hacerlo (si es que lo vamos a presentar) tenemos que dividir entre 
# las reglas donde "FALSE" es positivo y donde "FALSE" es negativo (es decir, 
# por un lado las reglas que están bien que nos den FALSE y las que no); de ese 
# modo no se van a presentar confusiones.

# Reglas FALSE POSITIVO: r1, r4, r5, r6, r7, r8, r10, r11, r12, r13, r14
# Reglas FALSE NEGATIVO: r2, r3, r9 (están dudosas pq no entiendo cómo funciona lo que pusiste)

# -REGLAS CASO 1-
# 1ro: se pasa a formato largo
validacion_largo_1 <-  tidyr::pivot_longer(validacion[1:100,c(1,2, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15)], -registro, names_to = "regla", values_to = "error")

# 2do: se grafica
library(ggplot2)
ggplot(validacion_largo_1)+
  aes(x = registro, y = regla, fill = error)+
  geom_tile(width = 0.7, height = .9, color = "black")+
  scale_fill_manual(values = c("#377EB8","#E41A1C","#999999"))+
  theme(caption = "Gráfico 1: Falso = bueno") +
  theme_bw()


# -REGLAS CASO 2-
# 1ro: se pasa a formato largo
validacion_largo_2 <-  tidyr::pivot_longer(validacion[1:100,c(1,3, 4, 10)], -registro, names_to = "regla", values_to = "error")

# 2do: se grafica
library(ggplot2)
ggplot(validacion_largo_2)+
  aes(x = registro, y = regla, fill = error)+
  geom_tile(width = 0.7, height = .9, color = "black")+
  scale_fill_manual(values = c("#E41A1C", "#377EB8","#999999"))+
  theme(caption = "Gráfico 2: Falso = malo") +
  theme_bw()


# RESULTADOS

# 1) Participantes limpios
# Al igual que el gráfico, hay que contar por un lado aquellos individuos donde
# el TRUE significa error y lo que no (en este último caso sumamos los TRUE y 
# sumamos a individuos limpios)

# -CASO 1-
validacion_largo_1 |>
  filter(error) |>
  distinct(registro) |>
  count()
# Hay 22 individuos con inconsistencias -> hay, en este caso, 978 limpios


# -CASO 2-
validacion_largo_2 |>
  filter(error) |>
  distinct(registro) |>
  count()
# Hay 100 individuos con todo "TRUE" -> hay 100 individuos limpios

# ¿Cómo saber cuáles coinciden con los 978 de arriba? NICO, HELP.
