############################
#### Primer programa
#### 
#### Mitzi Naomi Camargo Arellano
#### 17/02/2022
####
#############################

## Lo primero que haremos es subir la base de datos y convertir nuestra matriz, 
# para poder trabajar con ella.
matriz_amix <- read.csv( "red_amiguis.csv" )
matriz_amix

# Convertimos en matriz
class(matriz_amix)

# Selecciono la primera columna y los nombres
matriz_amix[,1]<-row.names(matriz_amix)
matriz_amix

# Quito la primer columna
matriz_amix<-matriz_amix[,-1]
matriz_amix

# Quitamos el segundo renglón, para quitar las NA
matriz_amix<-matriz_amix[-2,-2]
matriz_amix

# Conviertimos el dataframe a la matriz
matriz_amix <-as.matrix(matriz_amix)
class(matriz_amix)
matriz_amix

## Cargamos la librería "igraph"
library(igraph)

#Construimos la red
red_amix<-graph_from_adjacency_matrix(matriz_amix,mode="directed")
red_amix #Imprimimos


## 1. Grafique la red 
plot(red_amix) #A partir de la función plor se grafica la red

## 2. Determine a las tres personas con más amigues
mas_popus <-degree(red_amix,mode="in") #Usamos in porque es el de entrada
sort (mas_popus, decreasing = TRUE)[1:3] #sort, para solo seleccionar los primeros 3


## 3. Determine a las tres personas que consideran que tiene más amigues
se_creen<- -degree(red_amix,mode="out") #Usamos out porque es el de salida
sort (se_creen, decreasing = TRUE)[1:3] #sort, para solo seleccionar los primeros 3


## 4. Las tres personas más importantes por tres medidas de centralidad

degree(red_amix)[1:3]
closeness(red_amix)[1:3]
eccentricity(red_amix)[1:3] 

##5. Clusteriza la red con al menos dos métodos y determine cuáles son los clústers.

#Primer método de clustarización 
clusterizando_la_red <- cluster_leading_eigen (red_amix)
clusterizando_la_red

membership(clusterizando_la_red) #Lo agrupa por clusters
table (membership (clusterizando_la_red))

# Segundo método 
clusterizando_la_red2 <- cluster_walktrap (red_amix)
clusterizando_la_red

membership(clusterizando_la_red2) #Lo agrupa en clusters
table (membership (clusterizando_la_red2))


## 6. Calcule el diámetro
diameter(red_amix)


## 7. La matriz de distancias y dibuje un heatmap
m_d <-dist(matriz_amix)#matriz de distancia
m_d
#Hacemos el heatmap
h <-shortest.paths(red_amix)
heatmap(histo)
