library(tidyverse)
library(dashboardthemes)
library(shinyWidgets)
library(plotly)
library(sf)
library(tmap)
library(haven)
library(viridis)
library(readxl)
library(RColorBrewer)
library(ggcorrplot)

source("custom_theme.R")
# Mapas a nivel Entidad
mapa_ent <- st_read("www/sin_islas.shp")
# Mapas a nivel Munucipio
mapa_mun <- st_read("www/00mun.shp", 
                    options = "ENCODING=WINDOWS-1252")

# Bases de datos para glosarios
glosario_pob<-read.csv("www/glosario_pob.csv")
glosario_nat<-read.csv("www/glosario_nat.csv")
glosario_mor<-read.csv("www/glosario_mor.csv")
glosario_mat<-read.csv("www/glosario_mat.csv")
glosario_div<-read.csv("www/glosario_div.csv")
glosario_intern<-read.csv("www/glosario_internacional.csv")
glosario_intern_eco<-read.csv("www/glosario_internacional_econ.csv")

# Bases de datos para gráficas internacionales
internacionales<-read.csv("www/country_internacional.csv")
internacionales$year <- as.Date(paste0(internacionales$year, '-01-01'))
internacionales_eco<-read.csv("www/country_internacional_econ.csv")
internacionales_eco$year <- as.Date(paste0(internacionales_eco$year, '-01-01'))

# Bases de datos para gráficas Joel Mortalidad
mort_sexo_anio <- read_excel("www/Mortalidad_sexo_anio.xls")
mort_causa_anio <- read_excel("www/mort_causa_anio.xlsx")

# Bases para mapas Joel Mortalidad
mort_estado_sexo <- read_excel("www/mort_estado_sexo.xlsx")
mort_estado_causa <- read_excel("www/mort_estado_causa.xlsx")
mort_muni <- read_excel("www/mort_muni.xls")
opc_mun <- c("Enfermedades del corazón",
             "Enfermedades del higado", "Tumor maligno", "Diabetes" 
             , "Accidente", "Homicidio")


# Bases de datos para gráficas Will Embarazos por Características de Madre
embarazos1 <- read_xls("www/prueba1.xls")
embarazos1$anio <- as.Date(paste0(embarazos1$anio, '-01-01'))
embarazos2 <- read_xls("www/prueba2.xls")
embarazos2$anio <- as.Date(paste0(embarazos2$anio, '-01-01'))
x1 <- c("Situación laboral 2009-2019",names(embarazos1)[2:8])
x2 <- c("Estado civil",names(embarazos1)[9:15])
x3 <- c("Escolaridad",names(embarazos1)[16:24])
x4 <- c("Condición de empleo",names(embarazos1)[25:27])
n <- max(length(x1), length(x2),length(x3),length(x4))
length(x1) <- n
length(x2) <- n
length(x3) <- n
length(x4) <- n
nombres_variables1 <- data.frame(rbind(x1,x2,x3,x4))
nombres_buenos1 <- read.csv("www/nom_variables1.csv",header = FALSE)
rm(x1,x2,x3,x4,n)
# Bases de datos para gráficas Will Embarazos por regiones, entidades, país
embarazos_area <- read_xls("www/emb_ado_areas.xls")
embarazos_area$anio <- as.Date(paste0(embarazos_area$anio, '-01-01'))

# Base de datos para mapas Will Embarazos adolecentes por estados, municipios y región

emb_muni <- read_xls("www/emb_ado_muni_todos.xls")
emb_mapa_muni <- merge(emb_muni, mapa_mun, by="CVEGEO", all.x=TRUE)

emb_est <- read_xls("www/emb_ado_estados_todos.xls")
emb_mapa_estados <- merge(emb_est, mapa_ent, by="CVE_EDO", all.x=TRUE)

emb_reg <- read_xls("www/emb_ado_regiones_todos.xls")
emb_mapa_reg <- merge(emb_reg, mapa_ent, by="CVE_EDO", all.x=TRUE)

rm(emb_muni,emb_est,emb_reg)

# Bases de datos Diego para gráficas de nupcialidad 
divorcios_anio <- read_excel("www/divorcios_diego1.xls")
names_divorcio <- colnames(divorcios_anio)[-1]
matrimonios_anio <- read_excel("www/matrimonio_diego1.xls")
names_matrimonio <- colnames(matrimonios_anio)[-1]
divorcios_causa_anio <- read_excel("www/pastel_diego_divorcios.xls")
divorcios_causa_anio_sinmutuo <- divorcios_causa_anio%>%filter(causa!="Mutuo concentimiento")
matrimonios_tipo_anio <- read_excel("www/pastel_diego_matrimonios.xls")

# Base de datos Diego Mapas Nupcialidad

# Base de datos de matrimonio a nivel municipio
matrimonios_mun<-read_dta("www/matrimonios_year_map_municipio_listo.dta")
matrimonios_mun <- matrimonios_mun%>%rename(año=year)%>%mutate(CVEGEO=paste0(CVE_ENT,CVE_MUN))%>%select(-c(CVE_ENT,CVE_MUN))
matrimonios_mun$year <- as.Date(paste0(matrimonios_mun$año, '-01-01'))
matrimonios_mun_names <- names(matrimonios_mun)[2:15] #Seleccionamos las columnas con las variables para el usuario
# Base de datos de matrimonio a nivel entidad
matrimonios_ent<-read_dta("www/matrimonios_year_map_entidad_listo.dta")
matrimonios_ent <- matrimonios_ent%>%rename(año=year,CVE_EDO=CVE_ENT)
matrimonios_ent$year <- as.Date(paste0(matrimonios_ent$año, '-01-01'))
matrimonios_ent_names <- names(matrimonios_ent)[3:16] #Seleccionamos las columbas con las variables para el usuario

# Base de datos de divorcios a nivel municipio
divorcios_mun<-read_dta("www/divorcios_year_map_municipio_listo.dta")
divorcios_mun <- divorcios_mun%>%rename(año=year)%>%mutate(CVEGEO=paste0(CVE_ENT,CVE_MUN))%>%select(-c(CVE_ENT,CVE_MUN))
divorcios_mun$year <- as.Date(paste0(divorcios_mun$año, '-01-01'))
divorcios_mun_names <- names(divorcios_mun)[2:9] #Seleccionamos las columnas con las variables para el usuario
# Base de datos de divorcios a nivel entidad
divorcios_ent<-read_dta("www/divorcios_year_map_entidad_listo.dta")
divorcios_ent <- divorcios_ent%>%rename(año=year,CVE_EDO=CVE_ENT)
divorcios_ent$year <- as.Date(paste0(divorcios_ent$año, '-01-01'))
divorcios_ent_names <- names(divorcios_ent)[3:10] #Seleccionamos las columbas con las variables para el usuario

# Bases de datos Mapas Chetty, Clustering y Corr
chetty <- read_excel("www/mapa_chetty.xlsx")
chetty <- chetty%>%rename(CVE_EDO=CVE_ENT)
variables_esperanza<-colnames(chetty)[3:5]

correlations <- read_excel("www/correlations.xlsx")
comparacion <- read_excel("www/scatter_datos_limpio.xlsx")
comparacion <- comparacion%>%rename(CVE_EDO=CVE_ENT)
comparacion_nuevo <- read_excel("www/scatter_datos_limpio.xlsx")
comparacion_nuevo <- comparacion_nuevo%>%rename(CVE_EDO=CVE_ENT)
variables_poblacion<-colnames(comparacion_nuevo)[3:37]
variables_economicas <-colnames(comparacion_nuevo)[38:length(colnames(comparacion_nuevo))]
nombres <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
             "Coahuila", "Colima","Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
             "Guanajuato","Guerrero","Hidalgo","Jalisco", "Estado de México",    
             "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
             "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
             "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
nomb <- c("Ags", "BC", "BCS", "Camp", 
          "Coah", "Col", "Chiap", "Chih", "CDMX", "Dgo", 
          "Gto", "Gro", "Hgo", "Jal", "EdoMx",
          "Mich", "Mor", "Nay", "NL", "Oax", "Pue", 
          "Qro", "Q_Roo", "SLP", "Sin", "Son", 
          "Tab", "Tamps", "Tlax", "Ver", "Yuc", "Zac")
vars <- c(variables_poblacion,variables_economicas) 

ratios <- read_excel("www/mapas_ratios.xlsx")
ratios <- ratios%>%rename(CVE_EDO=CVE_ENT)

# Bases de datos análisis Joel

percentiles_municipios <- read_excel("www/percentiles_municipios.xls")
tasas_mun <- c("Tasa de mortalidad", "Tasa de embarazos adolescentes", 
               "Tasa de nacimientos")
per_ing <- c("Sin rezago", "1 rezago (1 mes)", "2 rezagos (2 meses)")
coef_emb_1 <- read_dta("www/coef_tasa_emb_ado_final_1.dta")
coef_emb_2 <- read_dta("www/coef_tasa_emb_ado_final_2.dta")
coef_emb_3 <- read_dta("www/coef_tasa_emb_ado_final_3.dta")
coef_mort_1 <- read_dta("www/coef_tasa_mort_final_1.dta")
coef_mort_2 <- read_dta("www/coef_tasa_mort_final_2.dta")
coef_mort_3 <- read_dta("www/coef_tasa_mort_final_3.dta")
coef_nac_1 <- read_dta("www/coef_tasa_nac_final_1.dta")
coef_nac_2 <- read_dta("www/coef_tasa_nac_final_2.dta")
coef_nac_3 <- read_dta("www/coef_tasa_nac_final_3.dta")

# Bases de datos análisis Will

prom_cuantiles_estados <- read_xls("www/econ_pob_prom_cuantiles.xls")
coef_cuantiles_estados <- read_xls("www/coef_reg_cuantil_econ_pob.xls")
nom_var_ols <- read_xls("www/nomb_var_ols.xls")
