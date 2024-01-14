# ON 13TH JAN AT 0.20 AM (GMT+7)

######################### PACKAGE ########################################
library(dplyr)
library(reshape)
library(ggplot2)
library(FactoMineR)
library(factoextra)


######################### DATA SPLITING BY POS ###########################
gk <- data3[data3$position == "Goalkeeper",]
def <- data3[data3$position == "Defender",]
mid <- data3[data3$position == "Midfielder",]
fwd <- data3[data3$position == "Forward",]

######################### GOALKEEPER ###################################
gk <- gk[gk$goals_conceded_per_90<5,] #remi matthews has 30
gk2 <- gk
rownames(gk2) <- gk2$player
gk2 <- gk2[,-c(1:5, 10, 13, 14, 17:21, 24:26, 28, 30, 31)]
glimpse(gk2)
zgk <- as.data.frame(scale(gk2))
gk.melt <- melt(zgk)
ggplot(gk.melt, aes(factor(variable), value))+
  geom_boxplot()+
  theme_classic()+
  coord_flip()

### PCA
pca.gk <- PCA(gk2, scale.unit=TRUE, graph=FALSE)
ev.gk <- get_eigenvalue(pca.gk)
ev.gk

### ELBOW CURVE
fviz_nbclust(zgk, kmeans, method="wss")+
  geom_vline(xintercept=5, linetype=2)+
  labs(subtitle="Elbow method")

### KMEANS
set.seed(2023)
km.gk <- kmeans(x=zgk, centers=5, nstart=50)
agg.gk <- aggregate(gk2, by=list(cluster=km.gk$cluster), mean)
agg.gk
fin.gk <- cbind(gk, cluster=km.gk$cluster)
fin.gk[fin.gk$cluster == 1, c(1,2,4)] 
fin.gk[fin.gk$cluster == 2, c(1,2,4)] 
fin.gk[fin.gk$cluster == 3, c(1,2,4)] 
fin.gk[fin.gk$cluster == 4, c(1,2,4)] 
fin.gk[fin.gk$cluster == 5, c(1,2,4)] 


######################### DEFENDER #####################################
def2 <- def
rownames(def2) <- def2$player
def2 <- def2[, -c(1:5, 18, 19, 20:22, 29)]
zdef <- as.data.frame(scale(def2))
def.melt <- melt(zdef)
ggplot(def.melt, aes(factor(variable), value))+
  geom_boxplot()+
  theme_classic()+
  coord_flip()

### PCA
pca.def <- PCA(def2, scale.unit=TRUE, graph=FALSE)
ev.def <- get_eigenvalue(pca.def)
ev.def

### ELBOW CURVE
fviz_nbclust(zdef, kmeans, method="wss")+
  geom_vline(xintercept=8, linetype=2)+
  labs(subtitle="Elbow method")

### KMEANS
set.seed(2023)
km.def <- kmeans(x=zdef, centers=8, nstart=50)
agg.def <- aggregate(def2, by=list(cluster=km.def$cluster), mean)
agg.def
fin.def <- cbind(def, cluster=km.def$cluster)
fin.def[fin.def$cluster == 1, c(1,2,4)]
fin.def[fin.def$cluster == 2, c(1,2,4)] 
fin.def[fin.def$cluster == 3, c(1,2,4)]
fin.def[fin.def$cluster == 4, c(1,2,4)] 
fin.def[fin.def$cluster == 5, c(1,2,4)] 
fin.def[fin.def$cluster == 6, c(1,2,4)]
fin.def[fin.def$cluster == 7, c(1,2,4)]
fin.def[fin.def$cluster == 8, c(1,2,4)]


######################### MIDFIELDER ###################################
mid2 <- mid
rownames(mid2) <- mid2$player
mid2 <- mid2[, -c(1:5, 18, 19, 20:22, 29)]
zmid <- as.data.frame(scale(mid2))
mid.melt <- melt(zmid)
ggplot(mid.melt, aes(factor(variable), value))+
  geom_boxplot()+
  theme_classic()+
  coord_flip()

### PCA
pca.mid <- PCA(mid2, scale.unit=TRUE, graph=FALSE)
ev.mid <- get_eigenvalue(pca.mid)
ev.mid

### ELBOW CURVE
fviz_nbclust(zmid, kmeans, method="wss")+
  geom_vline(xintercept=7, linetype=2)+
  labs(subtitle="Elbow method")

### KMEANS
set.seed(2023)
km.mid <- kmeans(x=zmid, centers=7, nstart=50)
agg.mid <- aggregate(mid2, by=list(cluster=km.mid$cluster), mean)
agg.mid
fin.mid <- cbind(mid, cluster=km.mid$cluster)
fin.mid[fin.mid$cluster == 1, c(1,2,4)] 
fin.mid[fin.mid$cluster == 2, c(1,2,4)] 
fin.mid[fin.mid$cluster == 3, c(1,2,4)] 
fin.mid[fin.mid$cluster == 4, c(1,2,4)] 
fin.mid[fin.mid$cluster == 5, c(1,2,4)] 
fin.mid[fin.mid$cluster == 6, c(1,2,4)] 
fin.mid[fin.mid$cluster == 7, c(1,2,4)] 


######################### MIDFIELDER ###################################
fwd2 <- fwd
rownames(fwd2) <- fwd2$player
fwd2 <- fwd2[, -c(1:5, 17:19, 20:22, 29)]
zfwd <- as.data.frame(scale(fwd2))
fwd.melt <- melt(zfwd)
ggplot(fwd.melt, aes(factor(variable), value))+
  geom_boxplot()+
  theme_classic()+
  coord_flip()

### PCA
pca.fwd <- PCA(fwd2, scale.unit=TRUE, graph=FALSE)
ev.fwd <- get_eigenvalue(pca.fwd)
ev.fwd

### ELBOW CURVE
fviz_nbclust(zfwd, kmeans, method="wss")+
  geom_vline(xintercept=6, linetype=2)+
  labs(subtitle="Elbow method")

### KMEANS
set.seed(2023)
km.fwd <- kmeans(x=zfwd, centers=6, nstart=50)
agg.fwd <- aggregate(fwd2, by=list(cluster=km.fwd$cluster), mean)
agg.fwd
fin.fwd <- cbind(fwd, cluster=km.fwd$cluster)
fin.fwd[fin.fwd$cluster == 1, c(1,2, 4)] #3
fin.fwd[fin.fwd$cluster == 2, c(1,2, 4)] #2
fin.fwd[fin.fwd$cluster == 3, c(1,2, 4)] #1
fin.fwd[fin.fwd$cluster == 4, c(1,2, 4)] #1
fin.fwd[fin.fwd$cluster == 5, c(1,2, 4)] #2
fin.fwd[fin.fwd$cluster == 6, c(1,2, 4)] #1

