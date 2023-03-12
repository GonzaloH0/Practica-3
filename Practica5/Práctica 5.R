

#Practica 5, Estadística descriptiva

#1. Crea un vector llamado ‘numArtefactos’ a partir de los siguientes valores referidos al número de artefactos por yacimiento: 

numArtefactos <- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos

#1.1. ¿Cómo almacena los valores numéricos: integer o double? 
#Para conocer el tipo de almacenamiento de los valores numéricos se emplea la función "typeof".
typeof(numArtefactos) #Se pregunta cómo son los valores y te indica que son dobles.

#1.2. Transforma el tipo de dato a número entero llamando al objeto ‘numArtefactos_int’.
#Para convertir el conjunto de datos a "integer", se emplea la función "as.integer"
numArtefactos_int <- as.integer(numArtefactos)
numArtefactos_int
#De esta manera si volvemos a preguntar que tipo de datos tiene ahora el objeto, nos responde que son enteros.
typeof(numArtefactos_int)


# 2. Calcula la media del objeto ‘numArtefactos_int’.
#Para calcular la media de un objeto, se aplica la función "mean".
media <-mean(numArtefactos_int) #La media es 45.55
media

#3. Calcula la mediana del objeto ‘numArtefactos_int’.
#Para calcular la mediana de un objeto, se aplica la función "median".
mediana <- median(numArtefactos_int) #La mediana es 40.5
mediana

#3.1. Define brevemente la mediana: concepto y cálculo
#La mediana es aquel valor que divide un conjunto de datos, ordenados de menor a mayor, en dos partes con el mismo númerode observaciones.
#Para calcular la mediana se ordenan los datos de menor a mayor, y se coge el valor central si existe un número impar de medidas,
# o se escoge la media entre las dos puntuaciones centrales si se trata de un número de observaciones par.


#4. Calcula la moda del objeto ‘numArtefactos_int’. 
#4.1.Explica detalladamente el procedimiento para su cálculo.

#Para calcular la moda de un conjunto de datos, que es el valor que más veces se repite, primero hay que 
# seleccionar los valores únicos que no se repitan (mediante la función "unique"), y crear una función 
# en la que se observe la frecuencia de los valores absolutos, y se seleccionen aquellos valores que más 
# se repiten. En este caso la función es la siguiente:

moda <- function(x){
  u <- unique(x)
tab <- tabulate(match(x, u))
 u[tab == max(tab)] }
 
moda1 <-moda(numArtefactos_int)
moda1
#Para este caso la  moda, o el valor que más se repite es el 10.


#5. Calcula el número de veces que se repite el valor correspondiente con la moda.
#Para calcular el número de veces que se repite la moda en un conjunto de datos, se aplica la función "table".
table(numArtefactos_int) #La moda se repite dos veces.


#6. Calcula los cuartiles del objeto ‘numArtefactos_int’.
#Para calcular los cuartiles de un objeto se emplea la función "quantile".
cuartiles <-quantile(numArtefactos_int)
cuartiles
#El resultado es 4.0 (0%), 21.5 (25%), 40.5 (50%), 61.5 (75%), 102 (100%).


#7. Calcula el rango intercuartílico del objeto ‘numArtefactos_int’. 
#Para calcular el rango intercuartílico de un objeto se aplica la función "IQR".
IQR(numArtefactos_int)

#7.1. Interpreta el resultado.
#El resultado del rango intercuartílico es 40, ya que es la diferencia que existe entre el Q1 y Q3.


#8. Calcula el rango del objeto ‘numArtefactos_int’.
#Para calcular el rango de un objeto se emplea la función "range".
range(numArtefactos_int)

#8.1. Almacena el rango en un vector denominado ‘rango_artefactos’.
rango_artefactos <- range(numArtefactos_int)
rango_artefactos
#El rango de este objeto es 4-102 acorde con sus cuartiles.

  
#9. Calcula la varianza del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo.
#Para calcular la varianza de un objeto se puede emplear la función "var".
var <-var(numArtefactos_int)
var
#Otra manera de lograr la varianza de un objeto es calculando su  desviación estándar y elevarla al cuadrado.
var2 <- sd(numArtefactos_int)^2
var2


#10.Calcula la desviación estándar del objeto ‘numArtefactos_int’. Emplea 2 funciones para su cálculo.
#Para calcular la desviación estándar de un objeto se puede aplicar la función "sd", o la función "sqrt(var())"
de <- sd(numArtefactos_int)
de
sqrt(var(numArtefactos_int))
#La desviación estandar de este objeto es 30.44836


#11.¿En qué se diferencia la desviación estándar de la varianza?
# La desviación estándar y la varianza son dos medidas que indican cómo de dispersos están los datos con 
# respecto a la media. Cuanto mayor sean estas medidas, mayor será la dispersión de los datos.
# La diferencia radica en que la desviación estándar es la raíz cuadrada de la varianza, o que la varianza
# es la desviación estándar al cuadrado.
 

#12.Visualiza gráficamente de manera horizontal la dispersión del objeto ‘numArtefactos_int’.
#Para crear una gráfica de dispersión se emplea la función "plot".
plot(numArtefactos_int)


#13.Crea un vector llamado ‘vector3’ a partir de la siguiente secuencia de valores 
vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)


#14.Calcula el coeficiente de variación de los objetos: 1)‘numArtefactos_int’ y 2) ‘vector3’. Emplea 2 funciones para su cálculo. 
#Para calcular el coeficiente de variación de un objeto hay que emplear la función sd(objeto)/mean(objeto)*100".
cv1 <- sd(numArtefactos_int) / mean(numArtefactos_int) * 100 
cv1
cv2 <- sd(vector3) / mean(vector3) * 100 
cv2

#También se puede emplear la función "coefficient.variaton", aunque esto requiere la instalación del paquete "Fincal".
install.packages("FinCal")
library(FinCal)
coefficient.variation(sd=sd(numArtefactos_int), avg = mean(numArtefactos_int))*100
coefficient.variation(sd=sd(vector3), avg = mean(vector3))*100

#14.1Compara e interpreta los resultados.

#El coeficiente de variación del objeto " vector 3" es de 63.59067
#El coeficiente de variación del objeto "numArtefactos_int" es 66.84602
#Esto quiere decir que los datos se encuentran ligeramente más dispersados en el objeto "numArtefactos_int",
#que en el "vector3".


#15.Genera una tabla-resumen de los estadísticos descriptivos expuestos: media, mediana, desviación estándar etc.
#Para crear una tabla resumen se utiliza la función "table", y se incluyen los resultados estadísticos deseados.
tablares <-table(media, mediana, moda1, var)
tablares


#16.Calcula el coeficiente de asimetría del objeto ‘vector3’. 
#Para calcular el coeficiente de asimetría de un objeto se puede emplear un cálculo basado en el método "mfv".
#Para ello previamente hay que instalar el paquete "modeest".
install.packages("modeest")
library(modeest)
CA1 <- (mean (vector3)- mlv(vector3, method="mfv")) /mean(vector3)
CA1

# Otra forma de calcular el coeficiente de asimetría es empleando la función "skewness"
# Previamente hay que instalar el paquete "e1071".
library(e1071)
CA2 <- skewness(vector3, na.rm = TRUE, type = 3)
CA2

#16.1.Interpreta su resultado. 
#El resultado con este método es 0.3138528, lo cual implica que la distribución es asimétrica hacia la derecha.


#16.2.Exponga ejemplos de distribuciones de variables con asimetría positiva y negativa y simétricas. 
#Si el resultado fuera negativo, la distribución sería asimétrica hacia la izquierda, y si el resultado 
# fuera 0, entonces la distribución sería simétrica.


#17.Calcula la curtosis del objeto ‘vector3’. 
#Para calcular la curtosis de un objeto se emplea la función "kurtosi".
#Previamente hay que instalar el paquete "psych".
install.packages("psych")
library(psych)
kurtosi(vector3)

# 17.1 ¿Qué tipo de curtosis se encuentra asociada al anterior objeto? Justifica tu respuesta. 
# El resultado de la curtosis del objeto "vector3" es -1.237981, lo cual se conoce como playkurtik.
# Esto quiere decir que al tener una curtosis menor que tres, lo que implica que el objeto tiende a producir
# menos valores atípicos y menos extremos que la distribución normal.

