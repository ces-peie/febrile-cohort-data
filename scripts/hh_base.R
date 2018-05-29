
#--------------------------------
# HOUSEHOLD BASELINE DATA-----
#--------------------------------




#--------------------------------
# Set environment----
#--------------------------------

#Load packages
library(package = "tidyverse")



#--------------------------------
# Read data----
#--------------------------------


#read data

#baseline houses
hh_baseline <- read_csv(file="data/02_baseline_households.csv")

#household enrollment data
hh_enrollment <- read_csv(file = "data/01_enrollment_visits.csv")



#----------------------------------
# Rename variables ----
#----------------------------------


#variable name without capital letters
hh_base <- hh_baseline %>%
  set_names(tolower(names(.)))


#----------------------------------
#Check household consent form-----
#----------------------------------


# Number of households with consent form at the enrollment time
hh_enrollment %>%
  group_by(dioConsentimiento) %>%
 filter(dioConsentimiento=="si")%>%
  summarize(n=n())#267 consent forms----

#Number of households with baseline survey (enrolled)
hh_base %>%
  distinct(idcasa) %>%
  summarize(n=n()) #314 households-----

# identify households with missing consent form for baseline survey
# Household ID is missing from the enrollment data



#--------------------------------------
#Check if the variables name match ODK name ----
#--------------------------------------

#check variables names
names(hh_base)
str(hh_base)
#categorical variables are presented as dummy variables-----

#OSCAR CODE FOR DUMMY VARIABLES----
# Conservas un ID y las variables que quieres
hh_base %>%
  as_tibble() %>%
  select(idcasa, matches("pared_"), matches("animal_")) %>%
  # Para ver que sale
  # Pivot para tener todas en una misma columna
  gather(variable, value, -idcasa) %>%
  arrange(idcasa, variable) %>%
  # Separar la variable de la categoria
  separate(col = variable, into = c("variable", "response"), sep = "_") %>%
  # Para cada idCasa pegar las marcadas y contar las marcadas
  group_by(idcasa, variable) %>%
  summarize(
    n_selected = sum(value, na.rm = TRUE),
    list_selected = paste(
      response[value == 1 & !is.na(value)],
      collapse = " "
    )
  )


#----------------------------------
# Check missing values from the data----
#----------------------------------



hh_base %>%
  #select house ID and members from household baseline
  select(
    idcasa = idcasa, 
    hhpersonas = personas
  )%>%
  #left_join to merge variables from the two datasets
  left_join(
    memb_base%>%
      select(idcasa, personas)
  )%>% 
  #314 households marged
  group_by(idcasa, personas)%>%
  summarize(n = n())%>%
  filter(personas != n) %>%
  knitr::kable()




#----------------------------------
#cross-validation household ID----
#----------------------------------


#check missing household ID
hh_base %>% 
  summarize(sum(is.na(idcasa)))


#check unique household ID
hh_base %>% 
  group_by(idcasa) %>%
  filter(n()>1)%>%
  summarize(n=n()) # No ID is repeated----



#----------------------------------
#Validation household location----
#----------------------------------


# Check missing values for all variables ---
hh_base %>%
  gather(variable, value, -idcasa, factor_key = TRUE) %>%
  group_by(variable) %>%
  summarize(
    missing = sum(is.na(value))
  ) %>%
  knitr::kable()


#check missing household altitude
hh_base %>% 
  filter(is.na(gpslatitude)) %>%
  pull(idcasa)


#check missing household longitude
hh_base %>% 
  filter(is.na(gpslongitude)) %>%
  pull(idcasa)

#"F-01-28-138-6" missing location altitude and longitude----

# Check coordinates on interactive map
hh_base %>%
  select(idcasa, matches("gps")) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    lng = ~gpslongitude,
    lat = ~gpslatitude,
    popup = ~idcasa
  )

# Check wrong coordinate
hh_base %>%
  filter(idcasa == "F-01-29-088-3") %>%
  t()




#----------------------------------
#Validation household esjefehogar ----
#----------------------------------

#check households with missing esjefehogar
hh_base %>% 
  filter(is.na(esjefehogar)) %>%
  pull(idcasa)




#----------------------------------
#Validation household personas ----
#----------------------------------

#check households with missing personas
hh_base %>% 
  filter(is.na(personas)) %>%
  pull(idcasa)


#----------------------------------
#Validation household casapropia ----
#----------------------------------

#check household with missing casapropia
hh_base %>% 
  filter(is.na(casapropia)) %>%
  pull(idcasa)


#----------------------------------
#Validation household casamejoras ----
#----------------------------------

#check households with missing casamejoras
hh_base %>% 
  filter(is.na(casamejoras)) %>%
  pull(idcasa)




#----------------------------------
#Validation household cuartos ----
#----------------------------------


#check households with missing cuartos
hh_base %>% 
  filter(is.na(cuartos)) %>%
  pull(idcasa)



#----------------------------------
#Validation household cuartos ----
#----------------------------------

#check households witn missing perosnasviviencuartos
hh_base %>% 
  filter(is.na(personasvivencuartos)) %>%
  pull(idcasa)
#"F-01-38-001-3" "F-01-38-002-5" "F-01-38-003-2" missing data on personas viven cuartos-----
#pueden existir casas en donde no viven personas?


#----------------------------------
#Validation household dondecocina----
#----------------------------------

#check households with missing dondecocina
hh_base %>% 
  filter(is.na(dondecocina)) %>%
  pull(idcasa)




#----------------------------------
#Validation household cocinacomo----
#----------------------------------

#check household with missing cononacomo
hh_base %>% 
  filter(is.na(cocinacomo)) %>%
  pull(idcasa)
#"F-02-01-009-3" "F-02-09-052-4" "F-02-10-057-6" missing values cocinacomo----



#----------------------------------
#Validation household sanitario----
#----------------------------------

#check households with missing sanitario_inodorolavale
hh_base %>% 
  filter(is.na(sanitario_inodorolavable)) %>%
  pull(idcasa)

#check households with missing sanitario_inodoroconecado
hh_base %>% 
  filter(is.na(sanitario_inodoroconectadoafos)) %>%
  pull(idcasa)

#check households with missing sanitario_letrina
hh_base %>% 
  filter(is.na(sanitario_letrina)) %>%
  pull(idcasa)

#check households with missing sanitario_letrinacondrenaje
hh_base %>% 
  filter(is.na(sanitario_letrinacondrenaje)) %>%
  pull(idcasa)

#check households with missing sanitario_notienesanitario
hh_base %>% 
  filter(is.na(sanitario_notienesanitario)) %>%
  pull(idcasa)

#check households with missing sanitario_otro
hh_base %>% 
  filter(is.na(sanitario_otro)) %>%
  pull(idcasa)


#----------------------------------
#Validation articuloshogar----
#----------------------------------

#check households with missing articulos_luz
hh_base %>% 
  filter(is.na(articulos_luz)) %>%
  pull(idcasa)

#check households with missing articulos_panelessolares
hh_base %>% 
  filter(is.na(articulos_panelessolares)) %>%
  pull(idcasa)

#check households with missing articulos_radiograbadora
hh_base %>% 
  filter(is.na(articulos_radiograbadora)) %>%
  pull(idcasa)

#check households with missing articulos_telefonodelinea
hh_base %>% 
  filter(is.na(articulos_telefonodelinea)) %>%
  pull(idcasa)

#check households with missing articulos_telefonocelular
hh_base %>% 
  filter(is.na(articulos_telefonocelular)) %>%
  pull(idcasa)

#check households with missing articulos_televisor
hh_base %>% 
  filter(is.na(articulos_televisor)) %>%
  pull(idcasa)

#check households with missing articulos_refrigeradora
hh_base %>% 
  filter(is.na(articulos_refrigeradora)) %>%
  pull(idcasa)

#check households with missing articulos_lavadoraderopa
hh_base %>% 
  filter(is.na(articulos_lavadoraderopa)) %>%
  pull(idcasa)

#check households with missing articulos_secadoraderopa
hh_base %>% 
  filter(is.na(articulos_secadoraderopa)) %>%
  pull(idcasa)

#check households with missing articulo_hornomicroondas
hh_base %>% 
  filter(is.na(articulos_hornomicroondas)) %>%
  pull(idcasa)

#check households with missing articulo_computadora
hh_base %>% 
  filter(is.na(articulos_computadora)) %>%
  pull(idcasa)



#----------------------------------
#Validation transporte----
#----------------------------------

#check households with missing trans_bicicleta
hh_base %>% 
  filter(is.na(trans_bicicleta)) %>%
  pull(idcasa)

#check households with missing trans_moto_pasola
hh_base %>% 
  filter(is.na(trans_moto_pasola)) %>%
  pull(idcasa)

#check households with missing trans_carro
hh_base %>% 
  filter(is.na(trans_carro)) %>%
  pull(idcasa)

#check households with missing trans_pickup
hh_base %>% 
  filter(is.na(trans_pickup))%>%
  pull(idcasa)

#check households with missing trans_camion  
hh_base %>% 
  filter(is.na(trans_camion))%>%
  pull(idcasa)

##check households with missing trans_caballoomula
hh_base %>% 
  filter(is.na(trans_caballoomula))%>%
  pull(idcasa)

#check households with missing trans_ninguno
hh_base %>% 
  filter(is.na(trans_ninguno))%>%
  pull(idcasa)


#----------------------------------
#Validation combustible----
#----------------------------------

#check household with missing combustible_gaspropano
hh_base %>% 
  filter(is.na(combustible_gasprogano))%>%
  pull(idcasa)

#check household with missing combustible_electricidad
hh_base %>% 
  filter(is.na(combustible_electricidad))%>%
  pull(idcasa)

#check household with missing combustible_lenia
hh_base %>% 
  filter(is.na(combustible_lenia))%>%
  pull(idcasa)

#check household with missing combustible_carbondelia
hh_base %>% 
  filter(is.na(combustible_carbondelia))%>%
  pull(idcasa)

#check household with missing combustible_residuosagricolas
hh_base %>% 
  filter(is.na(combustible_residuosagricolas))%>%
  pull(idcasa)

#check household with missing combustible_ninguno
hh_base %>% 
  filter(is.na(combustible_ninguno))%>%
  pull(idcasa)




#----------------------------------
#Validation piso----
#----------------------------------

#check household missing piso_tierra
hh_base %>% 
  filter(is.na(piso_tierra_arena))%>%
  pull(idcasa)

#check household missing piso_rustico
hh_base %>% 
  filter(is.na(piso_rustico))%>%
  pull(idcasa)

#check household missing piso_ladrillo
hh_base %>% 
  filter(is.na(piso_ladrillo_decemento))%>%
  pull(idcasa)

#check household missing piso_torda_cemento
hh_base %>% 
  filter(is.na(piso_torta_cemento))%>%
  pull(idcasa)

#check household missing piso_granito
hh_base %>% 
  filter(is.na(piso_granito))%>%
  pull(idcasa)

#check household missing piso_ceramica
hh_base %>% 
  filter(is.na(piso_ceramica))%>%
  pull(idcasa)

#check household missing piso_idcasa
hh_base %>% 
  filter(is.na(piso_madrea_lustrada))%>%
  pull(idcasa)

#check household missing piso_otro
hh_base %>% 
  filter(is.na(piso_otro))%>%
  pull(idcasa)


#----------------------------------
#Validation pared----
#----------------------------------

#check household missing pared_bajareque
hh_base %>% 
  filter(is.na(pared_bajareque))%>%
  pull(idcasa)

#check household missing pared_adobe
hh_base %>% 
  filter(is.na(pared_adobe))%>%
  pull(idcasa)

#check household missing pared_block
hh_base %>% 
  filter(is.na(pared_block))%>%
  pull(idcasa)

#check household missing pared_madera
hh_base %>% 
  filter(is.na(pared_madera))%>%
  pull(idcasa)

#check household missing pared_lamina
hh_base %>% 
  filter(is.na(pared_lamina))%>%
  pull(idcasa)

#check household missing pared_paolopique
hh_base %>% 
  filter(is.na(pared_palopique))%>%
  pull(idcasa)

#check household missing pared_concreto
hh_base %>% 
  filter(is.na(pared_concreto))%>%
  pull(idcasa)

#check household missing pared_ladrillo
hh_base %>% 
  filter(is.na(pared_ladrillo))%>%
  pull(idcasa)


#----------------------------------
#Validation techo----
#----------------------------------

#check household missing techo_paja
hh_base %>% 
  filter(is.na(techo_paja))%>%
  pull(idcasa)

#check household missing techo_palma
hh_base %>% 
  filter(is.na(techo_palma))%>%
  pull(idcasa)


#check household missing techo_teja
hh_base %>% 
  filter(is.na(techo_teja))%>%
  pull(idcasa)

#check household missing techo_lamina
hh_base %>% 
  filter(is.na(techo_lamina))%>%
  pull(idcasa)

#check household missing techo_concreto
hh_base %>% 
  filter(is.na(techo_concreto))%>%
  pull(idcasa)



#----------------------------------
#Validation vivienda_cedazo----
#----------------------------------

#check household missing vivienda_cedazo
hh_base %>% 
  filter(is.na(vivienda_cedazo))%>%
  pull(idcasa)


#----------------------------------
#Validation pabellon----
#----------------------------------

#check household missing values pabellon
hh_base %>% 
  filter(is.na(pabellon))%>%
  pull(idcasa)

#check  household missing values pabellon_insecticida
hh_base %>% 
  filter(is.na(pabellon_insecticida))%>%
  pull(idcasa)
#missing answers pabellon_insecticida
#[1] "F-01-01-073-2" "F-01-01-131-3" "F-01-01-133-4" "F-01-03-007-4" "F-01-06-004-1"
#[6] "F-01-08-050-3" "F-01-11-106-4" "F-01-11-152-1" "F-01-12-008-1" "F-01-12-012-2"
#[11] "F-01-14-143-1" "F-01-16-080-1" "F-01-18-119-4" "F-01-22-021-2" "F-01-22-032-3"
#[16] "F-01-22-053-1" "F-01-23-083-4" "F-01-30-123-5" "F-01-31-155-4" "F-01-33-059-2"
#[21] "F-01-33-060-5" "F-02-01-062-1" "F-02-03-106-4" "F-02-03-122-3" "F-02-04-022-1"
#[26] "F-02-04-092-4" "F-02-06-021-2" "F-02-09-154-2" "F-02-10-072-3" 

#cross-validation missing values pabellon and pabellon_insecticida
hh_base %>%
  filter(pabellon==3)%>%
  filter(!is.na(pabellon_insecticida))
#missing values in pabellon_insectida are the observations without pabellon----

#----------------------------------
#Validation fumi----
#----------------------------------

#check household missing values fumi
hh_base %>% 
  filter(is.na(fumi))%>%
  pull(idcasa)

#check household missing values fumi
hh_base %>% 
  filter(is.na(fumi_unidad))%>%
  pull(idcasa)

#check household missing values fecfumiga
hh_base %>% 
  filter(is.na(fecfumiga))%>%
  pull(idcasa)
#38  households with missing values fecfumiga

#cross-validation missing values pabellon and pabellon_insecticida
hh_base %>%
  filter(fumi==0)%>%
  filter(!is.na(fecfumiga))
#missing values in fecfumiga are the observations from hoseuholds with 0 fumiga observations----



#----------------------------------
#Validation pila_limpieza----
#----------------------------------

#check household with missing pila_limpiezafreq
hh_base %>% 
  filter(is.na(pila_limpiezafreq))%>%
  pull(idcasa)



#----------------------------------
#Validation larvi---
#----------------------------------

#check household with missing larvi
hh_base %>% 
  filter(is.na(larvi))%>%
  pull(idcasa)

table(hh_base$larvi) #larvi value 99, is a missing value?

#check household with missing larvi_cuando
hh_base %>% 
  filter(is.na(larvi_cuando))%>%
  pull(idcasa)
#125 househols with missing larvi_cuando values


#cross-validation missing values larvi and larvi_cuando
hh_base %>%
  filter(larvi==0)%>%
  filter(!is.na(larvi_cuando))
#121 missing values in larvi_cuando are the observations from households with 0 larvi observations----
# + 4 observations larvi 99


#check household with missing larvi_unidad
hh_base %>% 
  filter(is.na(larvi_unidad))%>%
  pull(idcasa)
#125 househols with missing larvi_unidad values


#cross-validation missing values larvi and larvi_unidad
hh_base %>%
  filter(larvi==0)%>%
  filter(!is.na(larvi_unidad))
#121 missing values in larvi_unidad are the observations from households with 0 larvi observations----
# + 4 observations larvi 99


#----------------------------------
#Validation basura_criadero----
#----------------------------------

#check household with missing basura_criadero
hh_base %>% 
  filter(is.na(basura_criadero))%>%
  pull(idcasa)


#----------------------------------
#Validation deposito_agua----
#----------------------------------

#check household with missing deposito_agua
hh_base %>% 
  filter(is.na(deposito_agua))%>%
  pull(idcasa)

table(hh_base$deposito_agua) #89 Observations withouth "no" deposito_agua

#check household with missing deposito_agua_tapadera
hh_base %>% 
  filter(is.na(deposito_agua_tapadera))%>%
  pull(idcasa)
#89 missing values deposito_tapadera

#cross-validation missing values deposito_agua and deposito_agua_tapadera
hh_base %>%
  filter(deposito_agua==0)%>%
  filter(!is.na(deposito_agua_tapadera))
#missing values in deposito_tapadera are the observations from households with 0 deposito_agua observations----


#----------------------------------
#Validation tipococinar----
#----------------------------------

#check household with missing tipococinar_camioncisterna
hh_base %>% 
  filter(is.na(tipococinar_camionocisterna))%>%
  pull(idcasa)

#check household with missing tipococinar_agua_lluvia
hh_base %>% 
  filter(is.na(tipococinar_agua_lluvia))%>%
  pull(idcasa)

#check household with missing tipococinar_nacimanantial
hh_base %>% 
  filter(is.na(tipococinar_nacimanantial))%>%
  pull(idcasa)


#check household with missing tipococinar_aguapotable
hh_base %>% 
  filter(is.na(tipococinar_aguapotable))%>%
  pull(idcasa)

#check household with missing tipococinar_chorropublico
hh_base %>% 
  filter(is.na(tipococinar_chorropublico))%>%
  pull(idcasa)

#check household with missing tipococinar_aguaembotellada
hh_base %>% 
  filter(is.na(tipococinar_aguaembotelladaobo))%>%
  pull(idcasa)


#check household with missing tipococinar_lavaderopublico
hh_base %>% 
  filter(is.na(tipococinar_lavaderopublico))%>%
  pull(idcasa)

#check household with missing tipococinar_pozopublico
hh_base %>% 
  filter(is.na(tipococinar_pozopublico))%>%
  pull(idcasa)

#check household with missing tipococinar_posopropio
hh_base %>% 
  filter(is.na(tipococinar_posopropio))%>%
  pull(idcasa)


#----------------------------------
#Validation tipovarios----
#----------------------------------

#check household with missing tipovarios_camioncisterna
hh_base %>% 
  filter(is.na(tipovarios_camionocisterna))%>%
  pull(idcasa)


#check household with missing tipovarios_agua_lluvia
hh_base %>% 
  filter(is.na(tipovarios_agua_lluvia))%>%
  pull(idcasa)

#check household with missing tipovarios_aguarioolago
hh_base %>% 
  filter(is.na(tipovarios_aguarioolago))%>%
  pull(idcasa)

#check household with missing tipovarios_nacimanantial
hh_base %>% 
  filter(is.na(tipovarios_nacimanantial))%>%
  pull(idcasa)

#check household with missing tipovarios_aguapotable
hh_base %>% 
  filter(is.na(tipovarios_aguapotable))%>%
  pull(idcasa)

#check household with missing tipovarios_chorropublico
hh_base %>% 
  filter(is.na(tipovarios_chorropublico))%>%
  pull(idcasa)


#check household with missing tipovarios_lavaderopublico
hh_base %>% 
  filter(is.na(tipovarios_lavaderopublico))%>%
  pull(idcasa)

#check household with missing tipovarios_pozopublico
hh_base %>% 
  filter(is.na(tipovarios_pozopublico))%>%
  pull(idcasa)



#----------------------------------
#Validation basura_recoleccion----
#----------------------------------

#check household with missing basura_recoleccion
hh_base %>% 
  filter(is.na(basura_recoleccion))%>%
  pull(idcasa)

table(hh_base$basura_recoleccion) #139 households without basura_recoleccion

#check household with missing frecbasura
hh_base %>% 
  filter(is.na(frecbasura))%>%
  pull(idcasa)
#137 missing households with frecbasura 

#cross-validation missing values basura_recoleccion and frecbasura
hh_base %>%
  filter(basura_recoleccion==0)%>%
  filter(!is.na(frecbasura))
#missing values in frecbasura are the observations  with 0 basura_recoleccion observations----


#----------------------------------
#Validation casa_drenaje----
#----------------------------------

#check household with missing casa_drenaje
hh_base %>% 
  filter(is.na(casa_drenaje))%>%
  pull(idcasa)


#check household with missing drenaje_pluvial
hh_base %>% 
  filter(is.na(drenaje_pluvial))%>%
  pull(idcasa)


#check household with missing drenaje_grises
hh_base %>% 
  filter(is.na(drenaje_grises))%>%
  pull(idcasa)



#check household with missing drenaje_negras
hh_base %>% 
  filter(is.na(drenaje_negras))%>%
  pull(idcasa)


#check household with missing drenaje_nosabe
hh_base %>% 
  filter(is.na(drenaje_no_sabe))%>%
  pull(idcasa)



#----------------------------------
#Validation toman_leche----
#----------------------------------


#check household with missing toman_leche
hh_base %>% 
  filter(is.na(toman_leche))%>%
  pull(idcasa)

table(hh_base$toman_leche)#181 no toman_leche

#check household with missing toman_leche_fuentevaca
hh_base %>% 
  filter(is.na(toman_leche_fuentevaca))%>%
  pull(idcasa)

#check household with missing toman_leche_fuentecabra
hh_base %>% 
  filter(is.na(toman_leche_fuentecabra))%>%
  pull(idcasa)

#check household with missing leche_hierven
hh_base %>% 
  filter(is.na(leche_hierven))%>%
  pull(idcasa)
#185 households missing leche_hierven


#cross-validation missing values toman_leche and leche_hierven
hh_base %>%
  filter(toman_leche==0)%>%
  filter(!is.na(leche_hierven))
#missing values in leche_hierven are the observations  with 0 toman_leche observations----




#----------------------------------
#Validation queso_fresco----
#----------------------------------


#check household with missing queso_fresco
hh_base %>% 
  filter(is.na(queso_fresco))%>%
  pull(idcasa)




#----------------------------------
#Validation lata----
#----------------------------------


#check household with missing lata
hh_base %>% 
  filter(is.na(lata))%>%
  pull(idcasa)

table(hh_base$lata) # 94 no lata


#check household with missing latafreq
hh_base %>% 
  filter(is.na(latafreq))%>%
  pull(idcasa)


#cross-validation missing values lata and latafreq
hh_base %>%
  filter(lata==0)%>%
  filter(!is.na(latafreq))
#missing values in latafreq are the observationswith 0 lata observations----



#----------------------------------
#Validation cultivo----
#----------------------------------


#check household with missing cultivo
hh_base %>% 
  filter(is.na(cultivo))%>%
  pull(idcasa)



#----------------------------------
#Validation animal----
#----------------------------------

#check household with missing animalposee
hh_base %>% 
  filter(is.na(animalposee))%>%
  pull(idcasa)

#check household with missing animal_perro
hh_base %>% 
  filter(is.na(animal_perro))%>%
  pull(idcasa)

#check household with missing animal_gato
hh_base %>% 
  filter(is.na(animal_gato))%>%
  pull(idcasa)

#check household with missing animal_cerdo
hh_base %>% 
  filter(is.na(animal_cerdo))%>%
  pull(idcasa)

#check household with missing animal_caballo
hh_base %>% 
  filter(is.na(animal_caballo))%>%
  pull(idcasa)

#check household with missing animal_vaca
hh_base %>% 
  filter(is.na(animal_vaca))%>%
  pull(idcasa)

#check household with missing animal_pato
hh_base %>% 
  filter(is.na(animal_pato))%>%
  pull(idcasa)

#check household with missing animal_pollo
hh_base %>% 
  filter(is.na(animal_pollo))%>%
  pull(idcasa)

#check household with missing animal_otrasaves
hh_base %>% 
  filter(is.na(animal_otrasaves))%>%
  pull(idcasa)

#check household with missing animal_otro
hh_base %>% 
  filter(is.na(animal_otro))%>%
  pull(idcasa)

table(hh_base$animal_otro)#13 animal otro yes


#check household with missing animalotro
hh_base %>% 
  filter(is.na(animalotro))%>%
  pull(idcasa)

table(hh_base$animalotro)#animalotro type of animal
#QUE HAGO CON LA VARIABLE ANIMAL OTRO?----


#----------------------------------
#Validation ratones----
#----------------------------------

#check household with missing ratonesencasa
hh_base %>% 
  filter(is.na(ratonesencasa))%>%
  pull(idcasa)

#check household with missing ratones_cuando
hh_base %>% 
  filter(is.na(ratones_cuando))%>%
  pull(idcasa)
#61 missing values ratones_cuando


#cross-validation missing values ratonesecasa and ratones_cuando
hh_base %>% 
 filter(ratonesencasa==0)%>%
  filter(!is.na(ratones_cuando))
#missing values in ratones_cuando are the observations with 0 ratonesencasa observations----



#check household with missing ratones_unidad
hh_base %>% 
  filter(is.na(ratones_unidad))%>%
  pull(idcasa)
#61 missing values ratones_cuando


#cross-validation missing values ratonesecasa and ratones_unidad
hh_base %>% 
  filter(ratonesencasa==0)%>%
  filter(!is.na(ratones_unidad))
#missing values in ratones_unidad are the observations with 0 ratonesencasa observations----


#----------------------------------
#Validation pulgas----
#----------------------------------

#check household with missing pulgas
hh_base %>% 
  filter(is.na(pulgas))%>%
  pull(idcasa)

#check household with missing pulgas_cuando
hh_base %>% 
  filter(is.na(pulgas_cuando))%>%
  pull(idcasa)
#242 missing househld with pulgas_cuando


#cross-validation missing values pulgas and pulgas_cuando
hh_base %>% 
  filter(pulgas==0)%>%
  filter(!is.na(pulgas_cuando))
#missing values in pulgas_cuando are the observations with 0 pulgas observations----

#check household with missing pulgas_unidad
hh_base %>% 
  filter(is.na(pulgas_unidad))%>%
  pull(idcasa)
#242 missing househld with pulgas_unidad


#cross-validation missing values pulgas and pulgas_unidad
hh_base %>% 
  filter(pulgas==0)%>%
  filter(!is.na(pulgas_unidad))
#missing values in pulgas_unidad are the observations with 0 pulgas observations----




#----------------------------------
#Validation garrapatas----
#----------------------------------

#check household with missing garrapatas
hh_base %>% 
  filter(is.na(garrapatas))%>%
  pull(idcasa)

#check household with missing garrapatas_cuando
hh_base %>% 
  filter(is.na(garrapatas_cuando))%>%
  pull(idcasa)
#272 missing househld with garrapatas_cuando


#cross-validation missing values garraptas and garrapatas_cuando
hh_base %>% 
  filter(garrapatas==0)%>%
  filter(!is.na(garrapatas_cuando))
#missing values in garrapatas_cuando are the observations with 0 garrapatas observations----

#check household with missing garrapatas_unidad
hh_base %>% 
  filter(is.na(garrapatas_unidad))%>%
  pull(idcasa)
#272 missing househld with pulgas_unidad


#cross-validation missing values garrapatas and garrapatas_unidad
hh_base %>% 
  filter(garrapatas==0)%>%
  filter(!is.na(garrapatas_unidad))
#missing values in garraptas_unidad are the observations with 0 garrapatas observations----





#----------------------------------
# Write household baseline data----
#----------------------------------

write_csv(hh_base, path = "output/clean-tables/hh_base.csv")

# End of script----

