library(tidyverse)
library(stringr)
library(stringi)

load("Emergencias_R.Rdata")
load("Acciones.Rdata")
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
