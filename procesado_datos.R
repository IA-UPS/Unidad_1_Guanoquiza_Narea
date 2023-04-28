



## Nos hace un listado de archivos
## Especificamos full.names=T para que nos de la ruta
directorio <- "./IA/tema1/datos/"
archivo <- list.files(directorio, full.names = T)
## leemos las lineas del archivo
predata <- readLines(archivo)
## obtenemos un objeto de la clase character
## obtenemos los datos y los convertimos a una matriz
## el signo negativo nos dice que no tome esos indices
## grep trabaja con expresiones regulares

predatos <- predata[-grep("@", predata)]
## convertimos a matriz para extraer el nombre
columnas <- t(matrix(
  unlist(strsplit(predata[grep("@attribute",
                               predata)], " ")),
  ncol = length(predata[grep("@attribute", predata)]),
  byrow = F
))[, 2]

datos <-
  as.data.frame(matrix(
    unlist(strsplit(predatos, ",")),
    ncol = length(columnas),
    byrow = T
  ))
colnames(datos) <- columnas

## hacemos un sumario de los datos
summary(datos)

## convertimos los datos a las clases correspondientes
str(datos)
colnames(datos)
datos[, c(1, 5:ncol(datos))] <-
  lapply(datos[, c(1, 5:ncol(datos))], as.factor)
datos[, 2:4] <- lapply(datos[, 2:4], as.numeric)

str(datos)

## Comparaciones dos a dos entre categoricas

## El chisq.test
chisq.test(table(datos$Gender,
                 datos$family_history_with_overweight))

chisq.csa <- chisq.test(table(datos$Gender,
                              datos$family_history_with_overweight))

chisq.csa$p.value

columnas.factores <- colnames(datos)[unlist(lapply(datos,
                                                   is.factor))]
## realizamos tantas combinaciones como se permiten
combinaciones <- as.data.frame(t(combn(columnas.factores, 2)))

combinaciones$p.value <- 1

datos.factoriales <- datos[, columnas.factores]

combinaciones$menor5 <- NA

## iteramos sobre cada variable factorial
## si es incorrecta la aproximacion, tener cuidado

for (i in 1:nrow(combinaciones)) {
    test.object <- chisq.test(table(datos[,combinaciones[i,1]],
                                                datos[,combinaciones[i,2]]))
    if(mean(test.object$observed)<5){
      
      combinaciones$menor5[i] <- mean(test.object$observed)
      
    }else{
      combinaciones$menor5[i] <- NA
    }
    combinaciones$p.value[i] <- test.object$p.value
    
}

library(data.table)
library(dplyr)
datos_dt <- as.data.table(datos)

datos_dt %>% group_by(Gender,SMOKE) %>% summarise(n=n())

csa <- datos %>% group_by(Gender,NObeyesdad) %>% group_by(n=n())

datos$obesidadsino <- (as.factor(grepl("obesity",ignore.case = T,datos$NObeyesdad)))

datos$obesidadsino

datos$CH20 <- as.numeric(datos$CH2O)

### Haced una tabla con la n de cada subgrupo formado
### y el p valor asociado y los valores esperados medios

### ¿La obecidad esta asiciada al genero?

obesidad <- datos$NObeyesdad
genero <- datos$Gender

(tabla<- (table(obesidad,genero)))
###tabla <- as.data.table()

###no estan asociados el genero y la obecidad?
### si esta asociado
### p<0.05 si estan asociados, si el valor de p es >0.05 no estan asociados

chisp.test(tabla)

familia <- datos$family_history_with_overweight

### el genero no esta relacionado con la historia familiar de la obecidad?

(tabla<- (table(obesidad,familia,genero)))



### no hay asociacion an la obecidad segun el genero
### dicho genero, su familia esta asociada con obesidad?
### ¿Cuales comen calorico pero no tienen obesidad? ¿
### De los que fuman, y no tienen obesidad, tienen actividad fisica
### en este paso habra preguntas, pregunten !
### beben alcohol y caminan ?

### Si puedes calcula el p valor y de estas comparaciones, 
### y cuanta es la diferencia en el numero de observaciones por variable

### De forma similar a lo anterior
### Realiza t.test tantos como sean necesarios entre las variables numericas
### y las categoricas cuando hay solo dos niveles
###   que sucede cuando hay más de un nivel ?



###ejemplos jueves

tablo <- table(datos$Gender,datos$family_history_with_overweight)
tablo
chisq.test(tablo)
binom.test(811,811+915,p=0.5)

###l1=split(altura,genero)
### para ver si cumplen una normalidad 
###p.valores=unlist(lapply(l,function(x),us;test(x,"pnorm")))
