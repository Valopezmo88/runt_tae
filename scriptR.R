library(readxl);library(dplyr);library(ggplot2);library(lubridate);library(car);library(leaps);library(corrplot)
library(forecast);library(tseries);library(stats);library(MASS);library(QuantPsyc);library(dynlm);library(gamlss)

rm(list=ls())

#setwd("C:\Users\ctf1vlo\Documents")

#base <- read.csv("base.csv", header = TRUE, sep = ";")

#PIB <- read_excel("PIB.xlsx", col_names = TRUE)

#base_completa <- left_join(base,PIB, by= "llave")

#write.csv(base_completa, "base_final.csv", sep = ";")

#datos <- read.csv2("raw/base.csv")
datos <-read_excel("raw/base_1.xlsx")


# Descripción de las variables: 
#   
#   En las variables se decidieron incluir 3 variables macroeconómicas importantes a la hora de vender un vehículo, estas son la TRM que es la tasa representativa de mercado (se calcula como el promedio aritmético de las tasas ponderadas de las operaciones de compra y de venta de dólares de los Estados Unidos de América a cambio de moneda legal Colombiana), las tasas de colocación del banco de la república y el PIB.
# 
# Periodicidades de las variables macro:
# - PIB: trimestral
# - TRM: diaria
# - TC: mensual 
# 
# 
# https://www.banrep.gov.co/es/estadisticas/producto-interno-bruto-pib
# https://www.datos.gov.co/Econom-a-y-Finanzas/Tasa-de-Cambio-Representativa-del-Mercado-Historic/mcec-87by/data
# https://www.repository.fedesarrollo.org.co/bitstream/handle/11445/1180/Repor_Abril_1989_Monroy_y_Ocampo.pdf?sequence=3&isAllowed=y
# https://www.larepublica.co/empresas/los-5-factores-que-estarian-frenando-la-venta-de-autos-2033943


base <- datos %>% 
  filter(year < 2018)
base_2018 <- datos %>% 
  filter(year == 2018)


# Modelamiento: 
#   Para hacer las pruebas respectivas al modelo se toma desde 2012 hasta 2016 para hacer el entrenamiento del modelo y el 2017 para hacer el test.
# 

train = base %>% 
  filter(year < 2017)  # conjunto de entrenamiento 

test = base %>% 
  filter(year == 2017) # conjunto de test 
unique(train$year)



base_1 = base %>%
  group_by(llave) %>%
  summarise(year_month = sum(Unidades, na.rm = T))%>%
  ungroup()



base$month2 <- factor(base$month2, levels=c("enero", "febrero", "marzo", "abril", "mayo", "junio","julio","agosto","septiembre","octubre", "noviembre", "diciembre")) 

base$day_week <- factor(base$day_week,  levels = c("lunes","martes", "miércoles", "jueves","viernes","sábado","domingo"))

ggplot(data = base) +
  geom_point(mapping = aes(x = month2, y= Unidades), color = "#00AFBB", size = 2) + 
  ggtitle("Figura 1: Total de registros por mes")+
  xlab(NULL)+
  ylab(NULL)
 
ggplot(data = base) +
  geom_point(mapping = aes(x = day_week, y= Unidades), color = "#00AFBB", size = 2) + 
  ggtitle("Figura 4: Total de registros por día de la semana")+
  xlab(NULL)+
  ylab(NULL)



base_date <- base %>%
  mutate(date = as.character(date),
         date = gsub("/","-",date),
         date_ok = ymd(date),
         year = year(date_ok),
         month = month(date_ok))

base_dm <- base_date %>%
  group_by(year,month) %>%
  summarize(unidades_m = sum(Unidades, na.rm = T)) %>%
  ungroup


g1 <- ggplot(data = base_date) + 
  geom_line(mapping = aes(x = month, y = Unidades)) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  facet_wrap(~ year, nrow = 3)

g2 <- ggplot(data = base_date) + 
  geom_point(mapping = aes(x = month, y = Unidades)) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  facet_wrap(~ year, nrow = 3)+
  ggtitle("Figura 2: Número histórico de registros entre 2012 y 2018")+ 
  xlab(NULL)+
  ylab(NULL)


g3 <- ggplot(data = base_dm) + 
  geom_line(mapping = aes(x = month, y = unidades_m)) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  facet_wrap(~ year, nrow = 6)+
  ggtitle("Figura 3: Número mensual de registros entre 2012 y 2017 ")+ 
  xlab(NULL)+
  ylab(NULL)

g2
g3

str(base)
pairs(numericas)



str(base)
base$TRM <- as.numeric(base$TRM)
base$TRM_1 <- as.numeric(base$TRM_1)
base$TRM_2 <- as.numeric(base$TRM_2)
base$TRM_8 <- as.numeric(base$TRM_8)
y <- base$Unidades


#######################################
# Criterio de selección de variables: #
# #####################################  

# Para mirar el desempeño de todas las variables que se incluyen en el modelo se va a utilizar el método de selección del mejor subconjunto.

mod_sat<- lm(Unidades ~ TC_1 + festive_day  + semana_santa + 
               PIB_1 + TRM_8 + TRM_1 + TRM_2 + salon_inter_auto + as.factor(month) + day_week , data = base)

summary(mod_sat)
vif(mod_sat)



# se realiza el analisis de vif sobretodo para revisar si existen problemas de multicolinealidad entre las variables macroeconómicas
# propuestas, teniendo todos los valores de las mismas por debajo de 5, se decide continuar con el uso de las mismas en las pruebas
# de los modelos. 
base$month <- as.factor(base$month)
base$day <- as.factor(base$day)

regfull<- regsubsets(Unidades ~ TC_1 + festive_day  + semana_santa +
                       PIB_1 + TRM_8 + TRM_1 + TRM_2 + salon_inter_auto + month + day_week  , data = base, nvmax =11, really.big = TRUE)

regsummary <- summary(regfull)

plot(regsummary$rss, xlab = "Número de variables", ylab = "RSS", type = "l")

plot(regsummary$adjr2, xlab = "Número de variables", ylab = "R2 ajustado", type = "l")


plot(regfull, scale = "adjr2", main = "Figura 5: Mejor subconjunto R2")

plot(regfull, scale = "bic", main = "Figura 6: Mejor subconjunto BIC")

plot(regfull, scale = "Cp",main = "Figura 7: Mejor subconjunto Cp Mallows")


# -En el gráfico del R2 el mejor subconjunto es el de la tasa de colocación y el PIB con un rezago, la indicadora de festivos y semana santa y por el lado de los factores los días de la semana y 
# los meses, teniendo un ajuste del 74%.
# 
# -El conjunto de variables que tiene el menor BIC es el mismo que el mencionado para el R2 incluyendo la TRM rezagada 8 días.
# -El conjunto de variables que tiene el menor Cp de Mallows es el mismo que para el del BIC.

#Teniendo en cuenta los resultados obtenidos en el regsubsets, se va a empezar con un modelo que incluya todas estas variables e ir evaluando su desempeño.


mod_lm<- lm(Unidades ~  festive_day + semana_santa +
                PIB_1 + as.factor(month) + day_week + as.factor(day) + TC_1 + TRM_8, data = base)

summary(mod_lm)
vif(mod_lm) 
# La teoría indica que si alguna variable regresora tiene un VIF > 5 hay indicios de problemas de multicol.


#Test de Correlación en los errores
Box.test(mod_prob$residuals,type="Ljung",lag=20,fitdf=2) 


##############################################
#Definición de los conjuntos de modelamiento #
#############################################

unique(base$year)

train = base %>% 
  filter(year < 2017)

test= base%>% 
  filter(year == 2017)

#########################################
#Propuesta Metodologica 1: Modelo lineal
#########################################

train %>% 
  filter_all(any_vars(is.na(.))) 


mod_lm_train<- lm(Unidades ~  festive_day + semana_santa +
                PIB_1 + as.factor(month) + day_week + as.factor(day) + TC_1, data = train)

summary(mod_lm_train)


pred_lm_train <- predict(mod_lm, newdata = train)
pred_lm_test<- predict(mod_lm_train, newdata = test)

y.train <- train$Unidades
y.test <- test$Unidades

R2_test <- function(y_real,y_pred){
  y_bar <- mean(y_real)
  return(1- sum((y_real - y_pred)^2)/sum((y_real - y_bar)^2))
}


R2_train_lm <-R2_test(y.train,pred_lm_train)
R2_test_lm <-R2_test(y.test,pred_lm_test)

R2_train_lm;R2_test_lm

base$day_week = as.factor(base$day_week)
base$month = as.factor(base$month)
base$day = as.factor(base$day)
base$year = as.factor(base$year)
factor <- data.frame(select_if(base, is.factor))
ncol(factor)


##########################################################
#Propuesta Metodologica 2: Modelo lineal generalizado GLM
##########################################################

# Variables categóricas 

var(base$Unidades)
summary(base$Unidades)


formula <- Unidades ~  festive_day  + semana_santa +
           PIB_1 + month + day_week + day + TC_1 

mod_pois <- glm(formula, data = train, family = 'poisson')
summary(mod_pois)


pred_pois_train <-predict(mod_pois, newdata = train, type = "response")
pred_pois_test <-predict(mod_pois, newdata = test, type = "response")

R2_glm_train <- R2_test(y.train,pred_pois_train)
R2_glm_test <-R2_test(y.test,pred_pois_test)

R2_glm_train;R2_glm_test



####################################
# Probando GAMLSS 
####################################
#Ajustando una distr 

fitUnds <- fitDist(Unidades, data = train, type = "counts", try.gamlss = TRUE)
fitUnds$fits

# 
# Al usar la función fitDist del paquete GAMLSS nos recomienda las distribución _ZANBI_  para el ajuste del modelo, pues su desviación es 
# la menor observada 


###################################

mod_zanbi_train <- gamlss(formula, data = train, family = ZANBI)
summary(mod_zanbi_train)

pred_zanbi_train <- predict(mod_zanbi_train, newdata = train, type = "response")
pred_zanbi_test <- predict(mod_zanbi_train, newdata = test, type = "response")
pred_zanbi_test

R2_zanbi_train<- R2_test(y.train,pred_zanbi_train)
R2_zanbi_test<- R2_test(y.test,pred_zanbi_test)

R2_zanbi_train;R2_zanbi_test


######################################
###### Pronosticos para el 2018 #####
#####################################

base_2018$day_week = as.factor(base_2018$day_week)
base_2018$month = as.factor(base_2018$month)
base_2018$day = as.factor(base_2018$day)
base_2018$year = as.factor(base_2018$year)
factor <- data.frame(select_if(base_2018, is.factor))
head(base_2018)

pred18 <- predict(mod_zanbi_train,newdata = base_2018, type = "response")
pred18

pron <- cbind(pred18,base_2018)
pron <- pron[,1:2]
pron

pron_18<- pron[,c("date","pred18")]
pron_18
names(pron_18) = c("date","pronostico2018")
head(pron_18,3)
write.table(pron_18,"predicciones_2018.txt", sep = ";", dec = ".")

######################################
###### Pronosticos 2012-2017 ########
#####################################

str(base)
pred_base <- predict(mod_zanbi_train,newdata = base, type = "response")
pred_base

pred_base <- round(pred_base,3)
head(pred_base)

tabla<- cbind(pred_base,base)
pred_hist <- tabla[, c("date","pred_base","Unidades")]
names(pred_hist) = c("date","pronostico","Unidades")

R2_test(pred_hist$Unidades,pred_hist$pronostico)

write.table(pred_hist,"predicciones_base.txt", sep = ";", dec = ".")



