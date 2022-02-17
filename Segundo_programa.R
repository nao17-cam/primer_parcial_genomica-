############################
#### Segundo programa
#### 
#### Mitzi Naomi Camargo Arellano
#### 17/02/2022
####
#############################

## Cargamos la librería " igraphdata" 
library(igraphdata)

#Para cargar la red "karate"
data("karate")
plot(karate) #Visualizar la red en un plot

# 1.Encuentre las tres personas más conectadas.
tres_mas <-degree(karate,mode="in") #Usamos in porque es el de entrada
sort (tres_mas, decreasing = TRUE)[1:3]


# 2. La gráfica de la distribución de conectividades. Mediante "degree_distribution"
plot(degree_distribution(karate))


# 3. El diámetro de la red.
diameter(karate)


# 4. El coeficiente de clusterización cada una de las 3 personas más conectadas
transitivity(karate, v="Jhon_A")
transitivity(karate, v="Mr Hi")
transitivity(karate, v="Actor 33 ")
#Se usa "v" para seleccionar a la persona 

# 5. Encuentre si los hay, a los nodos con coeficiente de clusterización de 1. Discute su significado.
coeficiente <- make_ring(5)
plot(coeficiente)
transitivity(coeficiente)


# 6. El porcentaje de conexiones respecto al total.
para_porcentaje1 <-34*33/2
para_porcentaje2 <-sum(degree(karate))
porcentaje_final <-para_porcentaje2/para_porcentaje1*100
porcentaje_final

# 7. El promedio de conectividades.
#Primero se saca el degree y después el promedio
para_promedio<- sort(degree(karate), decreasing = TRUE)
para_promedio
mean(para_promedio)


# 8. Encuentre QUIÉNES son las 3 personas más importantes con al menos 3 distintos métodos
#Método 1: Excentricidad: Los valores más pequeños, nos dan los valores más centrales
metodo1 <-sort(eccentricity(karate))[1:3] 
metodo1


#Método 2: closeness_centrality : Son más importantes los que tengan valores más altos
metodo2 <-sort(closeness(karate), decreasing= TRUE)[1:3]
metodo2


#Método 3: Degree
metodo3 <- sort(degree(karate), decreasing = TRUE)[1:3]
metodo3


# 9. Encuentre la trayectoria entre las personas más alejadas.
farthest_vertices(karate)
# Me dice que Actor 16 y Actor 17 son los más alejados

#10. Clusteriza la red con al menos 4 métodos distintos
#y discute tu resultado sabiendo que ese grupo de personas
#se separo en dos clubes distintos con el tiempo.

#Primer método 
clusterm1 <- cluster_leading_eigen (karate)
clusterm1

# Segundo método 
clusterm2 <- cluster_walktrap (karate)
clusterm2

# Tercer método
clusterm3 <- cluster_fast_greedy (karate)
clusterm3

# Cuarto método
clusterm4 <- cluster_edge_betweenness (karate)
clusterm4


