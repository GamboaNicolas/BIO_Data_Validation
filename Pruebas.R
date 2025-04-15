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
  "r3", "Fecha de entrevista en formato DD/MM/YYYY","stringr::str_detect(interview,pattern = '^[:digit:]{2}/[:digit:]{2}/[:digit:]{4}$', negate = T) | is.na(dmy(interview,quiet = T))",
  "r4", "Fechas congruentes(edad > 18)",            "as.numeric(dmy(interview,quiet = T) - dmy(substr(patientid,1,10),quiet = T))/365 < 18",
  "r5", "Grupo étnico entre las opciones",          "!(ethnicgroup %in% c(1,2,3,4))",
  "r6", "Elegible SCR dentro de opciones",          "!(scr %in% c(1,2))",
  "r7", "Elegible USSCR dentro de opciones",        "!(usscr %in% c(1,2))",
  "r8", "Consentimiento dentro de opciones",        "!(consent %in% c(1,2))",
  "r9", "Número del sujeto en el formato adecuado", "stringr::str_detect(subjectnumber,pattern = '^[:digit:]{9}$', negate = T)",
  # "r10", "Enrolamiento correcto",                   "scr != 2 | usscr != 2 | consent != 2",
  "r10", "Enrolamiento correcto",                   "((scr == 1 | usscr == 1 | consent == 1) & !is.na(subjectnumber)) | ((scr == 2 & usscr == 2 & consent == 2) & is.na(subjectnumber))" ,
  # "r11", "Número del sujeto correcto",              "substr(subjectnumber, 1, 3) != paste0(0,countrycode) & substr(subjectnumber, 1, 3) != paste0(0, 0, countrycode)",
  "r11", "Número del sujeto correcto",              "as.numeric(substr(subjectnumber, 1, 3)) != countrycode",
  "r12", "Grupo étnico respondido",                 "is.na(ethnicgroup)",
  # "r13", "Número del sujeto completado",            "is.na(subjectnumber)",
  "r13", "Código de país completado",               "is.na(countrycode)",
  "r14", "scr completado",                          "is.na(scr)",
  "r15", "usscr completado",                        "is.na(usscr)",
  "r16", "consent completado",                      "is.na(consent)",
  "r17", "ID pasiente completado",                  "is.na(patientid)",
  "r18", "Fecha de entrevista completada",          "is.na(interview)"
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
validador <- function(datos, id, cond){
  reglas <- datos[[cond]]
  names(reglas) <- datos[[id]]
  reglas
}

# Funcion validar()
# argumentos:
# - datos : conjunto de datos a validar
# - id : nombre de la columna en (datos) con el identificador
# - validador: salida de validador()
# salida: tibble con el resultado de la validación
validar <- function(datos, id, validador){
  sapply(
    validador,
    function(x) eval(parse(text = x), datos)
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(registro = datos[[id]], .before = 0)
}


validacion <-  validar(datos, "id", validador(reglas, "id", "cond"))




# GRÁFICO (a modo ilustrativo)
# 1ro: se pasa a formato largo
validacion_largo <-  tidyr::pivot_longer(validacion, -registro, names_to = "regla", values_to = "error") |> 
  mutate(regla = factor(regla,levels = reglas$id))

# 2do: se grafica
library(ggplot2)
ggplot(validacion_largo)+
  aes(x = registro, y = regla, fill = error)+
  geom_tile(width = 0.7, height = .9, color = "black")+
  scale_fill_manual(values = c("skyblue2","firebrick3","gray"))+
  theme_bw()


# RESULTADOS

# 1) Participantes limpios

validacion_largo |>
  filter(error) |>
  distinct(registro) |>
  count()
# Hay 239 individuos con inconsistencias -> hay, en este caso, 761 limpios

# 2) Partipantes "sucios"
# Con el código anterior vemos que se tienen 239 individuos con inconsistencias; 
# vemos quiénes son esos individuos:

kableExtra::kable(validacion_largo |>
                    filter(error) |>
                    count(registro)) 

# 3) Inconsistencias más frecuentes

kableExtra::kable(validacion_largo |>
                    filter(error) |>
                    count(regla) |> 
                    arrange(-n) |> 
                    `colnames<-`(c("Regla", "N° de individuos"))) 

# 4) Campos con más inconsistencias


validacion_largo <- validacion_largo |>
  mutate(campo = rep(c(rep("rango", times = 9), 
                   rep("consistencia", times = 2),
                   rep("existencia", times = 7)), times = 18000/18))


kableExtra::kable(validacion_largo |>
                    filter(error) |>
                    count(campo))


