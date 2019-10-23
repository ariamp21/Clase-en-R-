#install.packages("ggplot2")
#install.packages("rvest")
library("ggplot2")
library("rvest")
#install.packages("pastecs")
#install.packages ("effsize")
library(pastecs)
library(effsize)
#install.packages("readr")
library("readr")
install.packages("RODBC")
library("RODBC")
#install.packages("moments")
library("moments")

##########
##DATA####
##########

data(cars)

#Asigno data de autos dentro de una variable

autos <-data.frame(cars)
write.csv(autos, file = "autos.csv")

#Estadistica descriptiva de la data
summary(autos)



#¿Como los leemos? 

#Desviacion estandar de cada variable
sd(autos$speed)


sd(autos$dist)


#Regresion lineal
reg1 <- lm(speed ~ dist, data = autos)
summary(reg1)

  
  #Los parámetros de la ecuación de la recta de mínimos cuadrados que relaciona la cantidad de 
  #velocidad en función de la distancia vienen dados por la columna ´Estimate´ de la tabla ´Coefficients´
  #de la salida anterior. Por lo tanto, en este ejemplo la ecuación de la recta de mínimos cuadrados es:  
  
  #  Y =  8.28391+0.16557*X

#######################
###### PREDICCION #####
#######################

for (x in autos$dist){
 print(8.28391+0.16557*x)
  
}


#######################
###### INTERVALOS #####
#######################

confint(reg1, level = 0.95)
#                2.5 %     97.5 %
#(Intercept) 6.5258378   10.0419735
#dist        0.1303926   0.2007426

######################
###### ANOVA #########
######################

anova(reg1)

#Analysis of Variance Table

#Response: speed
#           Df Sum    Sq      Mean   Sq F value   Pr(>F)    
#dist       1 891.98  891.98  89.567 1.49e-12 ***
#  Residuals 48 478.02    9.96                     
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  
#########################
###### TEST NORMALIDAD ##
#########################

#load("autos.dat")

data <- read.csv("autos.csv", header=T,sep=",")
shapiro.test(autos$dist)
#data:  autos$dist
#W = 0.95144, p-value = 0.0391

#######################
# CURTOSIS ASIMETRIA ##
######################

skewness(autos$dist)

kurtosis(autos$dist)

#####################
### OUTLIERS #######
####################

boxplot(autos$dist, main = "Distancia", boxwex = 0.5,col="blue")

impute_outliers <- function(z, removeNA = TRUE){
  quantiles <- quantile(z, c(0.05, 0.95), na.rm = removeNA)
  z[z<quantiles[1]] <- mean(z, na.rm = removeNA)
  z[z>quantiles[2]] <- median(z, na.rm = removeNA)
  z
}
imputed_data <- impute_outliers(autos$dist)

par(mfrow = c(1,2))

boxplot(imputed_data, main = "Distancia sin outliers", col = 2)

########################
###### TEST T ##########
########################

t.test(autos$dist, paired = F)

#One Sample t-test

#data:  autos$dist
#t = 11.794, df = 49, p-value = 6.384e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  35.65642 50.30358
#sample estimates:
#  mean of x 
#42.98 

##########################
##### GRAFICOS ###########
##########################

plot(autos$dist, autos$speed, xlab='dist', ylab='speed')
hist(main='Histograma Distancia', autos$dist, xlab='dist', ylab='speed')
hist(main='Histograma Velocidad', autos$speed, xlab='dist', ylab='speed')
