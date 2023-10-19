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
         FE_FECHA_EMERG>=as.Date("2019-01-01"),
         #FE_FECHA_EMERG>=as.Date("2019-12-31"),
         !str_detect(TX_ADMINISTRADO, "NO DETERMINADO"), 
         !is.na(ID_UF_CSIG),
         str_detect(TX_SUB_SECTOR, "Hidrocarburos")) %>%
  select(-c(TX_DEPARTAMENTO, TX_PROVINCIA, TX_DISTRITO)) %>%
  distinct() 
# %>% view()
#   group_by(TX_SUB_SECTOR, ANHO = year(FE_FECHA_EMERG)) %>% summarise(codigos = n_distinct(TX_CODIGO, na.rm = TRUE), UF = n_distinct(ID_UF_CSIG)) %>% 
#   spread(ANHO,codigos) %>% janitor::adorn_totals(c("row", "col"))


EmergenciasUF<- as.vector(unique(EmergenciasHID$ID_UF_CSIG))

#Revisamos las supervisiones realizadas

Acciones %>% 
  filter( str_detect(TXCOORDINACION,"HIDROCARBUROS") ,
          !str_detect(TXESTADO,"ANULADA|PLANIFICACIÓN|PROGRAMADA|NO EJECUTADA|EN REVISION"),
          FEFIN>= as.Date("2018-01-01"),
          IDUF_SIG %in% EmergenciasUF) %>% view()





#Filtramos las emergencias asociadas con unidades fiscalizables
#Códigos de emergencia por UF
#Como el 60% de emrgencias se da en hidrocarburos tomaremos como referencia ese subsector

Emer_conUF_HID<- Emergencias_R %>% filter(!is.na(ID_UF_CSIG),str_detect(TX_SUB_SECTOR,"Hidrocarburos")) %>%
  filter(!str_detect(TX_ADMINISTRADO, "NO DETERMINADO"), !str_detect(TX_VERIFICACION,"No amerita"),
         !str_detect(TX_TIPO_ATENCION,"Seguimiento"), !str_detect(TX_VERIFICACION,"No inmediata"),
         str_detect(TX_FLUIDO,"Petróleo")) %>%
  select(-c(TX_DEPARTAMENTO,TX_PROVINCIA ,TX_DISTRITO)) %>% distinct(., .keep_all = TRUE)

Emer_conUF_HID %>% group_by(TX_ADMINISTRADO,ID_UF_CSIG) %>% summarise(emerg = n_distinct(TX_CODIGO, na.rm = TRUE)) %>% arrange(desc(emerg) )

Emer_conUF_HID %>% distinct(TX_CODIGO, TX_TIPO,FG_TIPO_VOLCA,TX_CAUSA) %>% filter(!str_detect(FG_TIPO_VOLCA,"SI") ) 

table(Emer_conUF_HID$ID_UF_CSIG
      )
