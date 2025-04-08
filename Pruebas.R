3library(dplyr)
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
  "r10", "Enrolamiento correcto",                   "scr == 2 && usscr == 2 && consent == 2",
  "r11", "Número del sujeto correcto",              "substr(subjectnumber, 1, 2) == countrycode | substr(subjectnumber, 1, 2) == countrycode",
  "r12", "Grupo étnico respondido",                 "ethnicgroup != NA",
  "r13", "Número del sujeto completado",            "subjectnumber != NA"
  )


