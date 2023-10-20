library(tidyverse)
library(stringr)
library(stringi)

load("Emergencias_R.Rdata")
load("Acciones.Rdata")

#Exploramos un poco la data
#Los años con mayor cantidad de datos (94.3%) son 2019-2023 (2,353 emergencias) 
#43 emergencias no tienen fecha de emergencia
#Filtramos los que nose conoce el administrados y los que cuentan con unidad fiscalizable conocida
#Filtramos solo hidrocarburos ya que es la mayor casuistica

EmergenciasHID<- Emergencias_R %>%
  filter(!is.na(FE_FECHA_EMERG), 
         FE_FECHA_EMERG>=as.Date("2020-01-01"),
         #FE_FECHA_EMERG>=as.Date("2019-12-31"),
         !str_detect(TX_ADMINISTRADO, "NO DETERMINADO"), 
         !is.na(ID_UF_CSIG),
         str_detect(TX_SUB_SECTOR, "Hidrocarburos")) %>%
  select(-c(TX_DEPARTAMENTO, TX_PROVINCIA, TX_DISTRITO)) %>%
  distinct()  


EmergenciasHID %>%
  group_by(ID_UF_CSIG,ANHO = year(FE_FECHA_EMERG)) %>% summarise(codigos = n_distinct(TX_CODIGO, na.rm = TRUE)) %>%
  spread(ANHO,codigos) %>% janitor::adorn_totals(c("row", "col"))




EmergenciasUF<- as.vector(unique(EmergenciasHID$ID_UF_CSIG))

#Revisamos las supervisiones realizadas

Acciones %>% 
  filter( str_detect(TXCOORDINACION,"HIDROCARBUROS") ,
          !str_detect(TXESTADO,"ANULADA|PLANIFICACIÓN|PROGRAMADA|NO EJECUTADA|EN REVISION"),
          FEFIN>= as.Date("2018-01-01"),
          #FEFIN<= as.Date("2018-12-30"),
          str_detect(TXACCION,"IN SITU"),
          IDUF_SIG %in% EmergenciasUF) %>% view()

