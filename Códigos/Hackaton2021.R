library(dplyr)
library(ggplot2)
library(guf)

clientes <- read.csv("base01_V3.csv", header = T, sep = "|")[,-(13:16)]

length(unique(clientes$CODIGO_CLIENTE)) # Hay 60002 clientes en la base

clientes$ANTIGUEDAD <- 0

c <- split(clientes, clientes$PERIODO)

n_distinct(c[[1]]$CODIGO_CLIENTE)

for(i in 30:1){
  grid <- which(clientes$CODIGO_CLIENTE %in% c[[i]]$CODIGO_CLIENTE)
  clientes$ANTIGUEDAD[grid] <- 30-i+1
}

# Realizar un analisis por semestre

# 1er Semestre 2019
s1_2019 <- subset(clientes, PERIODO < 201907)

s2_2019 <- subset(clientes, PERIODO < 202001 & PERIODO > 201906)

s1_2020 <- subset(clientes, PERIODO < 202007 & PERIODO > 202001)

s2_2020 <- subset(clientes, PERIODO < 202101 & PERIODO > 202006)

s1_2021 <- subset(clientes, PERIODO >= 202101)

n_distinct(s1_2019$CODIGO_CLIENTE);n_distinct(s2_2019$CODIGO_CLIENTE);n_distinct(s1_2020$CODIGO_CLIENTE)
n_distinct(s2_2020$CODIGO_CLIENTE);n_distinct(s1_2021$CODIGO_CLIENTE)

# Podemos ver que el numero de clientes va aumentando, y curiosamente no se pierde ningun cliente en la temporalidad
# de los datos

# Vamos a mover un poco la base de clientes para que sea manejable

base <- s1_2021

base[is.na(s1_2021$INGRESO_MENSUAL),11] <- 0

a <- base %>% 
  group_by(CODIGO_CLIENTE) %>%
  summarise(INGRESO_MENSUAL = max(INGRESO_MENSUAL))

codigo <- subset(a, INGRESO_MENSUAL == 0)[,1]

ingresos <- clientes[ifelse(clientes$CODIGO_CLIENTE %in% codigo$CODIGO_CLIENTE,T,F),c(1,11)] %>%
  filter(!is.na(INGRESO_MENSUAL)) %>%
  group_by(CODIGO_CLIENTE) %>%
  summarise(max(INGRESO_MENSUAL))

grid <- which(base$CODIGO_CLIENTE %in% ingresos$CODIGO_CLIENTE)

tantos <- 0
for(i in grid){
  ingreso <- ingresos[which(ingresos$CODIGO_CLIENTE == base[i,1]),2]
  base[i,11] <- ingreso
  tantos <- tantos + ingreso
}
tantos <- as.numeric(tantos)

# Se agreo el último ingreso maximo de los clientes que no tenian ingresos en el último semestre
# Ya se tiene la base final para los clusters

base$EDAD[which(is.na(base$EDAD))] <- mean(base$EDAD, na.rm = T)

str(base)

# base final para el modelo

base_f <-data.frame(CODIGO_CLIENTE = base$CODIGO_CLIENTE)

base_f <- cbind(base_f,base[,c(4,6,11)])

base_f$INGRESO_MENSUAL <- scale(base$INGRESO_MENSUAL)
base_f$EDAD <- scale(base$EDAD)
base_f$UBIGEO_DIRECCION <- scale(base$UBIGEO_DIRECCION)

base_f$TIPO_DOCUMENTO <- as.factor(base$TIPO_DOCUMENTO)

base_f$GENERO_M <- ifelse(base$GENERO == "M",1,0)
base_f$GENERO_F <- ifelse(base$GENERO == "F",1,0)

base_f$ESTADO_CIVIL_S <- ifelse(base$ESTADO_CIVIL == "S",1,0)
base_f$ESTADO_CIVIL_V <- ifelse(base$ESTADO_CIVIL == "V",1,0)
base_f$ESTADO_CIVIL_D <- ifelse(base$ESTADO_CIVIL == "D",1,0)
base_f$ESTADO_CIVIL_C <- ifelse(base$ESTADO_CIVIL == "C",1,0)

base_f$AFILIACION_SMS <- ifelse(base$AFILIACION_SMS == "SI",1,0)
base_f$AFILIACION_BANCA_ONLINE <- ifelse(base$AFILIACION_BANCA_ONLINE == "SI",1,0)

base_f$GRUPO_RIESGO_1 <- ifelse(base$GRUPO_RIESGO == 1,1,0)
base_f$GRUPO_RIESGO_2 <- ifelse(base$GRUPO_RIESGO == 2,1,0)

base_f$SEGMENTO_COMERCIAL_1 <- base$SEGMENTO_COMERCIAL

base_f$ANTIGUEDAD <- scale(base$ANTIGUEDAD)

base_f_2 <- base_f %>%
  group_by(CODIGO_CLIENTE) %>%
  summarise(INGRESO_MENSUAL = max(INGRESO_MENSUAL),
            EDAD = max(EDAD),
            UBIGEO_DIRECCION = max(UBIGEO_DIRECCION),
            GENERO_M = max(GENERO_M),
            GENERO_F = max(GENERO_F),
            ESTADO_CIVIL_S = max(ESTADO_CIVIL_S),
            ESTADO_CIVIL_V = max(ESTADO_CIVIL_V),
            ESTADO_CIVIL_D = max(ESTADO_CIVIL_D),
            ESTADO_CIVIL_C = max(ESTADO_CIVIL_C),
            AFILIACION_SMS = max(AFILIACION_SMS),
            AFILIACION_BANCA_ONLINE = max(AFILIACION_BANCA_ONLINE),
            GRUPO_RIESGO_1 = max(GRUPO_RIESGO_1),
            GRUPO_RIESGO_2 = max(GRUPO_RIESGO_2),
            SEGMENTO_COMERCIAL_1 = max(SEGMENTO_COMERCIAL_1),
            ANTIGUEDAD = max(ANTIGUEDAD))

### Agrupando las rentabilidades de los clientes

r_clientes <- read.csv("base04_V3.csv", header = T, sep = "|")
c <- split(r_clientes, r_clientes$PERIODO)

n_distinct(r_clientes$CODIGO_CLIENTE)

la_chida <- r_clientes %>%
  group_by(CODIGO_CLIENTE) %>%
  summarise(R_NUM = sum(NUMERADOR_RATIO_RENTABILIDAD), R_DEN = sum(DENOMINADOR_RATIO_RENTABILIDAD)
            ,SALDO_MEDIO = mean(SALDO_PUNTUAL))

# Cruze con rentabilidades medias del cliente

base_f_3 <- base_f_2

base_f_3$RATIO_NUM_CL <- 0
base_f_3$RATIO_DEN_CL <- 0
base_f_3$SALDO_MEDIO_CL <- 0

base_f_3$RATIO_NUM_CL[which(base_f_3$CODIGO_CLIENTE %in% la_chida$CODIGO_CLIENTE)] <- la_chida$R_NUM
base_f_3$RATIO_DEN_CL[which(base_f_3$CODIGO_CLIENTE %in% la_chida$CODIGO_CLIENTE)] <- la_chida$R_DEN
base_f_3$SALDO_MEDIO_CL[which(base_f_3$CODIGO_CLIENTE %in% la_chida$CODIGO_CLIENTE)] <- scale(la_chida$SALDO_MEDIO)

# Clusters

base_final <- split(base_f_3,base_f_3$SEGMENTO_COMERCIAL_1)
x <- c(5:14,16,19)
# SEGMENTO COMERCIAL 1 Personas Físicas

base_final_1 <- base_final$`1`

vec <- 0
rent_max <- 0
indice_f <- 0
for(j in 1:10){
  indice <- cbind(2,3,t(combn(x,j)))
  for(i in 1:length(indice[,1])){
    vec <- indice[i,]
    set.seed(15486)
    clusters <- kmeans(x = base_final_1[,vec], centers = 5)
    base_final_1$CLUSTER <- as.factor(clusters$cluster)
    
    rent <- base_final_1 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    
    if(mean(rent$RATIO_RENT) > rent_max){
      indice_f <- indice[i,]
      rent_max <- mean(rent$RATIO_RENT)}
  }
}

set.seed(15486)
clusters_seg1 <- kmeans(x = base_final_1[,indice_f], centers = 5) #indice final c(2,3,8,9,12,16,19)
base_final_1$CLUSTER <- as.factor(clusters_seg1$cluster)

clusters_seg1 

base_final_1 %>%
  group_by(CLUSTER) %>%
  summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))

ggplot(data = base_final_1, aes(x = EDAD, y = INGRESO_MENSUAL, color = CLUSTER))+
  geom_point(size = 2.5)+
  theme_bw()

# SEGMENTO COMERCIAL 3 Personas Físicas con actividad empresarial

base_final_3 <- base_final$`3`

sum(base_final_3$RATIO_NUM_CL)/sum(base_final_3$RATIO_DEN_CL) # esta base tiene en general rentabilidad negativa :(

vec <- 0
rent_max <- -10
indice_f <- 0
for(j in 1:10){
  indice <- cbind(2,3,t(combn(x,j)))
  for(i in 1:length(indice[,1])){
    vec <- indice[i,]
    set.seed(13432)
    clusters <- kmeans(x = base_final_3[,vec], centers = 4)
    base_final_3$CLUSTER <- as.factor(clusters$cluster)
    
    rent <- base_final_3 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    
    if(mean(rent$RATIO_RENT) > rent_max){
      indice_f <- indice[i,]
      rent_max <- mean(rent$RATIO_RENT)}
  }
}

clusters_seg3 <- kmeans(x = base_final_3[,indice_f], centers = 4) #indice final c(2,3,12,13,14,19)
base_final_3$CLUSTER <- as.factor(clusters_seg3$cluster)

clusters_seg3

base_final_3 %>%
  group_by(CLUSTER) %>%
  summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))

ggplot(data = base_final_3, aes(x = EDAD, y = INGRESO_MENSUAL, color = CLUSTER))+
  geom_point(size = 2.5)+
  theme_bw()

# SEGMENTO COMERCIAL 4 Personas morales

base_final_4 <- base_final$`4`

vec <- 0
rent_max <- 0
indice_f <- 0
for(j in 1:10){
  indice <- cbind(2,3,t(combn(x,j)))
  for(i in 1:length(indice[,1])){
    vec <- indice[i,]
    set.seed(123654)
    clusters <- kmeans(x = base_final_4[,vec], centers = 3)
    base_final_4$CLUSTER <- as.factor(clusters$cluster)
    
    rent <- base_final_4 %>%
      group_by(CLUSTER) %>%
      summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))
    
    if(mean(rent$RATIO_RENT) > rent_max){
      indice_f <- indice[i,]
      rent_max <- mean(rent$RATIO_RENT)}
  }
}

clusters_seg4 <- kmeans(x = base_final_4[,indice_f], centers = 3) # indice final c(2,3,13,14,16)
base_final_4$CLUSTER <- as.factor(clusters_seg4$cluster)

clusters_seg4

base_final_4 %>%
  group_by(CLUSTER) %>%
  summarise(RATIO_RENT = sum(RATIO_NUM_CL/sum(RATIO_DEN_CL)))

ggplot(data = base_final_4, aes(x = EDAD, y = INGRESO_MENSUAL, color = CLUSTER))+
  geom_point(size = 2.5)+
  theme_bw()

write.csv(base_final_1,"Base_final_seg1.csv")
write.csv(base_final_3,"Base_final_seg3.csv")
write.csv(base_final_4,"Base_final_seg4.csv")

base_super_final <- rbind(base_final_1,
                          base_final_3,
                          base_final_4)

# Ahora vamos a agregar a las bases ya segmentadas que productos tienen contratados

r_productos <- read.csv("base05_V3.csv", header = T, sep = "|")
str(r_productos)
unique(r_productos$PRODUCTO)

produc <- split(r_productos,r_productos$PRODUCTO)

e <- produc$TARJETAS
n_distinct(r_productos$CODIGO_CLIENTE)

r_productos_2 <- r_productos %>%
  group_by(CODIGO_CLIENTE, PRODUCTO) %>%
  summarise(R_NUM = sum(NUMERADOR_RATIO_RENTABILIDAD), R_DEN = sum(DENOMINADOR_RATIO_RENTABILIDAD)
            ,SALDO_MEDIO = mean(SALDO_PUNTUAL))

n_distinct(r_productos$CODIGO_CLIENTE) # Solo tenemos información de productos de 12262 clientes

produc <- split(r_productos_2,r_productos_2$PRODUCTO)

r_tarjetas <- produc$TARJETAS
r_autos <- produc$AUTOS
r_comext <-produc$COMEXT
r_cartera <- produc$CARTERA
r_hipotecarios <- produc$HIPOTECARIO
r_leasing <- produc$LEASING
r_pr_comerciales <-produc$PRESTAMOS_COMERCIALES
r_tj_empresas <- produc$TJ_EMPRESAS
r_consumo <- produc$CONSUMO

base_super_final$TARJETAS <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_tarjetas$CODIGO_CLIENTE,1,0)
base_super_final$AUTOS <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_autos$CODIGO_CLIENTE,1,0)
base_super_final$COMEXT <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_comext$CODIGO_CLIENTE,1,0)
base_super_final$CARTERA <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_cartera$CODIGO_CLIENTE,1,0)
base_super_final$HIPOTECARIO <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_hipotecarios$CODIGO_CLIENTE,1,0)
base_super_final$LEASING <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_leasing$CODIGO_CLIENTE,1,0)
base_super_final$PR_COMERCIALES <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_pr_comerciales$CODIGO_CLIENTE,1,0)
base_super_final$TJ_EMPRESAS <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_tj_empresas$CODIGO_CLIENTE,1,0)
base_super_final$CONSUMO <- ifelse(base_super_final$CODIGO_CLIENTE %in% r_consumo$CODIGO_CLIENTE,1,0)

write.csv(base_super_final,"base_super_final.csv")

base_super_final %>% 
  group_by(SEGMENTO_COMERCIAL_1,CLUSTER) %>%
  summarise(TARJETAS = sum(TARJETAS),
            AUTOS = sum(AUTOS),
            COMEXT = sum(COMEXT),
            CARTERA = sum(CARTERA),
            HIPOTECARIO = sum(HIPOTECARIO),
            LEASING = sum(LEASING),
            PR_COMERCIALES = sum(PR_COMERCIALES),
            TJ_EMPRESAS = sum(TJ_EMPRESAS),
            CONSUMO = sum(CONSUMO))

# Porductos sugeridos para cada cluster
# Segmento1

cluster_1_1 <- c("TARJETAS","CONSUMO")
cluster_1_2 <- c("TARJETAS","CONSUMO")
cluster_1_3 <- c("TARJETAS","CONSUMO","AUTOS","HIPOTECARIO")
cluster_1_4 <- c("TARJETAS","CONSUMO","AUTOS","HIPOTECARIO")
cluster_1_5 <- c("TARJETAS","CONSUMO","AUTOS")

cluster_3_1 <- c("PR_COMERCIALES","TJ_EMPRESAS","CONSUMO","AUTOS")
cluster_3_2 <- c("TJ_EMPRESAS","LEASING")
cluster_3_3 <- c("PR_COMERCIALES","TJ_EMPRESAS","CONSUMO")
cluster_3_4 <- c("PR_COMERCIALES","CONSUMO")

cluster_4_1 <- c("COMEXT","CARTERA","LEASING","PR_COMERCIALES","TJ_EMPRESAS")
cluster_4_2 <- c("CARTERA","LEASING","PR_COMERCIALES","TJ_EMPRESAS")
cluster_4_3 <- c("COMEXT","CARTERA","PR_COMERCIALES","TJ_EMPRESAS")
