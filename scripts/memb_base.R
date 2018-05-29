
#--------------------------------
# INDIVIDUALS BASELINE DATA-----
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

#baseline individuals
memb_baseline <- read_csv(file = "data/02_baseline_people.csv")

#baseline households
hh_base <- read_csv(file = "output/clean-tables/hh_base.csv")




#----------------------------------*
# Rename variables ----
#----------------------------------*


#variable name without capital letters
memb_base <- memb_baseline %>%
  set_names(tolower(names(.)))

names(memb_base)


#----------------------------------*
# check members are in the households ----
#----------------------------------*

#check if all individuals have a house ID
hh_base %>%
  filter(is.na(idcasa))%>%
  pull(idcasa)



#Check number members match house baseline
check_memb_number <- hh_base %>%
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
  filter(personas != n)%>%
  print()

#4 households from the baseline have one individual less than from individuals baseline----


#----------------------------------*
# check missing values ----
#----------------------------------*


# Check missing values for all variables ---
memb_base %>%
  gather(variable, value, -idcasa, factor_key = TRUE) %>%
  group_by(variable) %>%
  summarize(
    missing = sum(is.na(value))
  ) %>%
  filter(missing != 0)



#----------------------------------*
# check estapersona ----
#----------------------------------*



#check missing estapersona
memb_base %>% 
  filter(is.na(estapersona))%>%
  pull(idcasa)


#----------------------------------**
# check fecnac ----
#----------------------------------**


#check missing fecnac
memb_base %>% 
  filter(is.na(fecnac))%>%
  pull(idcasa)



#----------------------------------*
# check calculoedad ----
#----------------------------------*


#check missing calculoedad
memb_base %>% 
  filter(is.na(calculoedad))%>%
  pull(idcasa)




#----------------------------------*
# check fecnac_exacta ----
#----------------------------------*


#check missing fecnac_exacta
memb_base %>% 
  filter(is.na(fecnac_exacta))%>%
  pull(idcasa)


#----------------------------------*
# check edad ----
#----------------------------------*


#check missing edad
memb_base %>% 
  filter(is.na(edad))%>%
  pull(idcasa)



#crossvalidation edad and calculoedad
memb_base %>% 
  filter(calculoedad != edad)%>%
  group_by(idcasa, identificador)%>%
  summarize(edad, calculoedad)
#calculoedad and edad do not match for 14 individuals-----


#----------------------------------*
# check sexo ----
#----------------------------------*


#check missing sexo
memb_base %>% 
  filter(is.na(sexo))%>%
  pull(idcasa)





#----------------------------------*
# check parent----
#----------------------------------*


#check missing parent
memb_base %>% 
  filter(is.na(parent))%>%
  pull(idcasa)


#----------------------------------*
# check parentotro----
#----------------------------------*


#check missing parentotro
memb_base %>% 
  filter(is.na(parentotro))%>%
  pull(idcasa)
#1000 missing parenotro


#cross-validation missing values parentotro and parent
memb_base %>% 
  filter(parent<9)%>%
  filter(!is.na(parentotro))
#missing values in parent otro are from parent<9 observations----




#----------------------------------*
# check alfa----
#----------------------------------*


#check missing alfa
memb_base %>% 
  filter(is.na(alfa))%>%
  pull(idcasa)


#----------------------------------*
# check ninio_asistenino----
#----------------------------------*

#should not be a filter or category for child that should be before this question-----

memb_base %>% 
  #filter for children from 5 to 14 years old
  filter(edad %in% c(5, 14))%>%
  filter(is.na(ninio_asistenino))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#missing data ninio_asistenino from 37 individuals


#cross validation missing values ninio_asistenino
memb_base %>% 
  #filter for children from 5 to 14 years old
  filter(edad %in% c(5, 14))%>%
  filter(alfa==0)%>%
  filter(!is.na(ninio_asistenino))
#missing values in ninio_asisteninio are the observations with 0 alfa----




#----------------------------------*
# check ninio_graoescolarnino----
#----------------------------------*


#check missing ninio_gradoescolarnino
memb_base %>% 
  #filter for children from 5 to 14 years old
  filter(edad %in% c(5, 14))%>%
  filter(is.na(ninio_gradoescolarninio))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#missing data ninio_gradoescolarnino from 37 individuals


#cross validation missing values nino_gradoescolarnino
memb_base %>% 
  #filter for children from 5 to 14 edad
  filter(edad %in% c(5, 14))%>%
  filter(alfa==0)%>%
  filter(!is.na(ninio_gradoescolarninio))
#missing values in ninio_gradoescolarnino are the observations with 0 alfa----




#----------------------------------*
# check mayor_aniogano----
#----------------------------------*

#check missing mayor_aniogano
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_aniogano))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#missing data mayor_aniogano from 254 individuals


#cross validation missing values mayor_aniogano
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(alfa==0)%>%
  filter(!is.na(mayor_aniogano))
#missing values in mayor_aniogano are the observations with 0 alfa----



#----------------------------------*
# validation mayor_rol----
#----------------------------------*


#check missing mayor_roltra_agricul
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_agricul))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_carnice
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_carnice))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_mataderorastro
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_mataderorastro))%>%
  group_by(idcasa, identificador)%>%
  summarize()



#check missing mayor_roltra_construccion
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_construccion))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_comercio
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_comercio))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_profesional
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_profesional))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_amadecasa
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_amadecasa))%>%
  group_by(idcasa, identificador)%>%
  summarize()



#check missing mayor_roltra_estudiante
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_estudiante))%>%
  group_by(idcasa, identificador)%>%
  summarize()



#check missing mayor_roltra_empleadadomestica
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_empleadadomest))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_carpintero
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_carpintero))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_mecanico
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_mecanino))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_enfermera
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_enfermera))%>%
  group_by(idcasa, identificador)%>%
  summarize()

#check missing mayor_roltra_notrabaja
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_notrabaja))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing mayor_roltra_otro
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltra_otro))%>%
  group_by(idcasa, identificador)%>%
  summarize()



#check missing mayor_roltraotro
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_roltraotro))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#729 missing values mayor_roltraotro



#cross-validation missing mayor_roltraotro
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
    filter(mayor_roltraotro==1)%>%
  filter(!is.na(mayor_roltraotro))
#missing values mayor_roltraotro are the 1 values from roltra_notrana----



#----------------------------------*
# validation mayor_trabaja----
#----------------------------------*


#check missing mayor_trabajaganado
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_trabajaganado))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#888 missing values mayor_trabajaganado


#cross-validation missing mayor_roltraotro and mayor_trabajaganado
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(mayor_roltraotro==1)%>%
  filter(!is.na(mayor_trabajaganado))
#missing values mayor_trabajaganado are the 1 values from roltra_notrana----


#check missing mayor_trabajagranja
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_trabajagranja))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#888 missing values mayor_trabajagranja


#cross-validation missing mayor_roltraotro and mayor_trabajagranja
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(mayor_roltraotro==1)%>%
  filter(!is.na(mayor_trabajagranja))
#missing values mayor_trabajagranja are the 1 values from roltra_notrana----



#check missing mayor_trabajaplantacionazucar
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_trabajaplantacionazucar))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#888 missing values mayor_trabajaplantacionazucar


#cross-validation missing mayor_roltraotro and mayor_trabajaplantacionazucar
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(mayor_roltraotro==1)%>%
  filter(!is.na(mayor_trabajaplantacionazucar))
#missing values mayor_trabajaplantacionazucar are the 1 values from roltra_notrana----



#check missing mayor_trabajaplantacioncafe
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_trabajaplantacioncafe))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#888 missing values mayor_trabajaplantacioncafe


#cross-validation missing mayor_roltraotro and mayor_trabajaplantacioncafe
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(mayor_roltraotro==1)%>%
  filter(!is.na(mayor_trabajaplantacioncafe))
#missing values mayor_trabajaplantacioncafe are the 1 values from roltra_notrana----


#check missing mayor_trabajaplantacionbanano
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(mayor_trabajagranja))%>%
  group_by(idcasa, identificador)%>%
  summarize()
#888 missing values mayor_trabajagranja


#cross-validation missing mayor_roltraotro and mayor_trabajaplantacionbabano
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(mayor_roltraotro==1)%>%
  filter(!is.na(mayor_trabajaplantacionbanano))
#missing values mayor_trabajaplantacionbanano are the 1 values from roltra_notrana----





#----------------------------------*
# validation enfercronica----
#----------------------------------*


#check missing enfercronica_asma
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_asma))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_diabetes
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_diabetes))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_cancer
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_cancer))%>%
  group_by(idcasa, identificador)%>%
  summarize()



#check missing enfercronica_insufcardiaca
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_insufcardiaca))%>%
  group_by(idcasa, identificador)%>%
  summarize()



#check missing enfercronica_cirrosis
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_cirrocis))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_insufrenal
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_insufrenal))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_epoc
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_epoc))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_hipertension
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_hipertension))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_epilepsia
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_epilepsia))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_tuberculosis
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_tuberculosis))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_ninguno
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_ninguno))%>%
  group_by(idcasa, identificador)%>%
  summarize()


#check missing enfercronica_otro
memb_base %>% 
  #filter for adults >14 edad
  filter(edad>14)%>%
  filter(is.na(enfercronica_otro))%>%
  group_by(idcasa, identificador)%>%
  summarize()






#----------------------------------*
# Write household baseline data----
#----------------------------------*

write_csv(memb_base, path = "output/clean-tables/memb_base.csv")

# End of script----
