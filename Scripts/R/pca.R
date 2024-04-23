library(rgl)
library(scatterplot3d)
library(ggplot2)
library(e1071)
library(plotly)

data_sporco1 <- `cbb.(1)`
data <- data_sporco1 [, 3:21]

risultato <- as.factor(data_sporco1[,22])

#nuova variabile
win_ratio <- data[,2]*100/data[,1]
min(win_ratio)
max(win_ratio)

data_new <- cbind(data, win_ratio)

dati_2015 <- data_new[data_sporco1$YEAR == 2015, ]
dati_2016 <- data_new[data_sporco1$YEAR == 2016, ]
dati_2017 <- data_new[data_sporco1$YEAR == 2017, ]
dati_2018 <- data_new[data_sporco1$YEAR == 2018, ]
dati_2019 <- data_new[data_sporco1$YEAR == 2019, ]


vettore_numeri_colonne <- c(3,4,14,15,16,17)
data_sei_colonne <- dati_2015[,vettore_numeri_colonne]



par(mfrow=c(1,1))
boxplot(data_sei_colonne, las=2, col='gold')

boxplot(scale(x=data_sei_colonne, center=T, scale=F), las=2, col='gold')

boxplot(scale(x=data_sei_colonne, center=T, scale=T), las=2, col='gold')


pairs(data_sei_colonne)



# Normalizzazione per intervallo
normalize_by_range <- function(x) {
  return((x - mean(x)) / (max(x) - min(x)))
}

# Applica la normalizzazione per intervallo alle colonne desiderate
df_normalized_range <- as.data.frame(lapply(data_sei_colonne, normalize_by_range))
boxplot(df_normalized_range)

# Calcola la PCA
pca <- prcomp(df_normalized_range, scale=F)
summary(pca)

# Visualizza il grafico a dispersione in 2d
plot(pca$x[,1], pca$x[,2], main = "2D PC Plane (Normalized by Range)", xlab = "PC1", ylab = "PC2")

#ora in 3d sulle prime tre principal component
scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], main = "3D PC Plane (Normalized by Range)", xlab = "PC1", ylab = "PC2", zlab = "PC3")


pca <- princomp(df_normalized_range, scores=T)
load.basket<- pca$loadings
load.basket


# Normalizzazione per deviazioni standard
normalize_by_sd <- function(x) {
  return((x - mean(x)) / sd(x))
}

# Applica la normalizzazione per deviazioni standard alle colonne desiderate
df_normalized_sd <- as.data.frame(lapply(data_sei_colonne, normalize_by_sd))
boxplot(df_normalized_sd)

# Calcola la PCA
pca <- prcomp(df_normalized_sd, scale=F)
summary(pca)

# Visualizza il grafico a dispersione
plot(pca$x[,1], pca$x[,2], main = "2D PC Plane (Normalized by Standard Deviations)", xlab = "PC1", ylab = "PC2")

#ora in 3d sulle prime tre principal component
scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], main = "3D PC Plane (Normalized by Standard Deviations)", xlab = "PC1", ylab = "PC2", zlab = "PC3")

pca <- princomp(df_normalized_sd, scores=T)
load.basket<-pca$loadings
load.basket




#3.c
data_provvisorio<- cbind(data_sei_colonne, data_sporco1[data_sporco1$YEAR == 2015, 22 ] )

risultato_2015 <- as.factor(data_provvisorio[,7])


# Create a grouping variable (e.g., based on some condition or criteria)
# Create 'data$group' based on 'risultato_2015'
data_sei_colonne$group <- ifelse(is.na(risultato_2015) | risultato_2015 %in% c("R68"), "Group 1",
                     ifelse(risultato_2015 %in% c("S16", "R32", "R64"), "Group 2",
                            ifelse(risultato_2015 %in% c("F4", "2ND", "Champions", "E8"), "Group 3", NA)))

# Visualize in 2D with distinct colors for groups
pca <- prcomp(data_sei_colonne[,1:6], scale=F)
summary(pca)

ggplot(data_sei_colonne, aes(x = pca$x[,1], y = pca$x[,2], color = group)) +
  geom_point() +
  ggtitle("Data with Pre-specified Groups") +
  theme_minimal()


# Visualize in 3D with distinct colors for groups
plot_ly(data = data_sei_colonne, x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3], 
        color = ~group, type = "scatter3d", mode = "markers")




#Visualize in 2D with distinct colors for groups with normalization by range
pca <- prcomp(df_normalized_range, scale=F)
ggplot(data_sei_colonne, aes(x = pca$x[,1], y = pca$x[,2], color = group)) +
  geom_point() +
  ggtitle("Data with Pre-specified Groups") +
  theme_minimal()

# Visualize in 3D with distinct colors for groups with normalization by range
plot_ly(data = data_sei_colonne, x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3], 
        color = ~group, type = "scatter3d", mode = "markers")




# Visualize in 2D with distinct colors for groups with normalization by sd
pca <- prcomp(df_normalized_sd, scale=F)

ggplot(data_sei_colonne, aes(x = pca$x[,1], y = pca$x[,2], color = group)) +
  geom_point() +
  ggtitle("Data with Pre-specified Groups") +
  theme_minimal()

# Visualize in 3D with distinct colors for groups with normalization by sd
pca <- prcomp(df_normalized_sd, scale=F)
plot_ly(data = data_sei_colonne, x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3], 
        color = ~group, type = "scatter3d", mode = "markers")




##visualizzazione dei loadings e della percentuale di variabilità spiegata
data_pca <- scale(data_sei_colonne[,1:6])
pca.basket <-princomp(data_pca)
summary(pca.basket)

load.basket<-pca.basket$loadings
load.basket

par(mfcol=c(3,2))
for(i in 1:6) barplot(load.basket[,i], ylim = c(-1, 1), main=paste("PC",i))



#Exploratory analysis
group_colors <- rainbow(length(unique(data_sei_colonne$group)))

# Assign colors to groups
colors <- group_colors[as.factor(data_sei_colonne$group)]

# Create pairs plot with different colors for each group
pairs(data_sei_colonne[,1:6], col = colors, pch = 19)





### provo a rifare lo stesso tipo di analisi ma cambio il criterio
### con cui scelgo i gruppi: invece di utilizzare il risultato nella postseason
### scelgo di dividerle in base alla loro percentuale di vittorie durante l'anno

win_ratio_2015<-win_ratio[which(list_provvisoria)]


# Function to categorize win ratios into three groups
categorize_win_ratio <- function(win_ratio) {
  groups <- cut(win_ratio, breaks = c(0, 60, 85, 100), labels = c("Group 1", "Group 2", "Group 3"))
  return(groups)
}

# Categorize win ratios
win_ratio_groups <- categorize_win_ratio(win_ratio_2015)

data_sei_colonne$group <- win_ratio_groups


pca <- prcomp(data_sei_colonne[,1:6], scale=F)
summary(pca)

ggplot(data_sei_colonne, aes(x = pca$x[,1], y = pca$x[,2], color = group)) +
  geom_point() +
  ggtitle("Data with Pre-specified Groups") +
  theme_minimal()


# Visualize in 3D with distinct colors for groups
plot_ly(data = data_sei_colonne, x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3], 
        color = ~group, type = "scatter3d", mode = "markers")




#Visualize in 2D with distinct colors for groups with normalization by range
pca <- prcomp(df_normalized_range, scale=F)
ggplot(data_sei_colonne, aes(x = pca$x[,1], y = pca$x[,2], color = group)) +
  geom_point() +
  ggtitle("Data with Pre-specified Groups") +
  theme_minimal()

# Visualize in 3D with distinct colors for groups with normalization by range
plot_ly(data = data_sei_colonne, x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3], 
        color = ~group, type = "scatter3d", mode = "markers")




# Visualize in 2D with distinct colors for groups with normalization by sd
pca <- prcomp(df_normalized_sd, scale=F)

ggplot(data_sei_colonne, aes(x = pca$x[,1], y = pca$x[,2], color = group)) +
  geom_point() +
  ggtitle("Data with Pre-specified Groups") +
  theme_minimal()

# Visualize in 3D with distinct colors for groups with normalization by sd
pca <- prcomp(df_normalized_sd, scale=F)
plot_ly(data = data_sei_colonne, x = ~pca$x[,1], y = ~pca$x[,2], z = ~pca$x[,3], 
        color = ~group, type = "scatter3d", mode = "markers")

















###SVD

# Esegui la SVD del dataframe normalizzato
svd_result <- svd(df_normalized_sd)

# Singoli valori singolari
singular_values <- svd_result$d

# Matrice di sinistra (U)
left_singular_vectors <- svd_result$u

# Matrice di destra (V)
right_singular_vectors <- svd_result$v

# Visualizzazione dei risultati
cat("Singoli valori singolari:\n")
print(singular_values)

cat("\nMatrice di sinistra (U):\n")
print(left_singular_vectors)

cat("\nMatrice di destra (V):\n")
print(right_singular_vectors)

# Grafico dei singoli valori singolari
plot(singular_values, type = "b", main = "Singoli Valori Singolari", xlab = "Indice", ylab = "Valore Singolare")

# Grafico delle prime due colonne della matrice di sinistra (U)
plot(left_singular_vectors[, 1], left_singular_vectors[, 2], main = "Matrice di Sinistra (U)", xlab = "Prima Colonna", ylab = "Seconda Colonna")

# Grafico delle prime due colonne della matrice di destra (V)
plot(right_singular_vectors[, 1], right_singular_vectors[, 2], main = "Matrice di Destra (V)", xlab = "Prima Colonna", ylab = "Seconda Colonna")






##svd
#visualization of data with SVD
std.data<-df_normalized_sd

# Perform Singular Value Decomposition (SVD)
svd_result <- svd(std.data) #choose data between range.data or std.data

# Extract the left singular vectors (U matrix)
U <- svd_result$u

# If you want to visualize in 2D, consider the first two principal components
pc_scores <- U[, 1:2]  # 2D PC plane

# If you want to visualize in 3D, consider the first three principal components
 pc_scores <- U[, 1:3]  # 3D PC plane

# Plot 2D or 3D PC plane
if (ncol(pc_scores) == 2) {
  # 2D Plot
  plot(pc_scores[, 1], pc_scores[, 2], 
       xlab = "PC1", ylab = "PC2", 
       main = "PC Plane (2D)")
} else if (ncol(pc_scores) == 3) {
  # 3D Plot
  library(rgl)
  plot3d(pc_scores[, 1], pc_scores[, 2], pc_scores[, 3], 
         xlab = "PC1", ylab = "PC2", zlab = "PC3", 
         main = "PC Plane (3D)")
}


# Define colors based on groups
data_sei_colonne$group <- ifelse(is.na(risultato_2015) | risultato_2015 %in% c("R68"), "Group 1",
                                 ifelse(risultato_2015 %in% c("S16", "R32", "R64"), "Group 2",
                                        ifelse(risultato_2015 %in% c("F4", "2ND", "Champions", "E8"), "Group 3", NA)))

# Define colors for each group
colors <- c("Group 1" = "red", "Group 2" = "green", "Group 3" = "blue")

# Define size for each group
sizes <- c("Group 1" = 4, "Group 2" = 6, "Group 3" = 9)

# Plot 2D or 3D PC plane with colors and varying point sizes
if (ncol(pc_scores) == 2) {
  # 2D Plot
  plot(pc_scores[, 1], pc_scores[, 2], 
       xlab = "PC1", ylab = "PC2", 
       main = "PC Plane (2D)",
       col = colors[data_sei_colonne$group],
       cex = sizes[data_sei_colonne$group])
} else if (ncol(pc_scores) == 3) {
  # 3D Plot
  library(rgl)
  plot3d(pc_scores[, 1], pc_scores[, 2], pc_scores[, 3], 
         xlab = "PC1", ylab = "PC2", zlab = "PC3", 
         main = "PC Plane (3D)",
         col = colors[data_sei_colonne$group])
  
  # Add points with different sizes iteratively
  for (i in unique(data_sei_colonne$group)) {
    subset_pc_scores <- pc_scores[data_sei_colonne$group == i, ]
    points3d(subset_pc_scores[, 1], subset_pc_scores[, 2], subset_pc_scores[, 3],
             col = colors[i],
             size = sizes[i])
  }
}






### pca sull'intero dataset

# Normalizzazione per intervallo
normalize_by_range <- function(x) {
  return((x - mean(x)) / (max(x) - min(x)))
}

# Applica la normalizzazione per intervallo alle colonne desiderate
df_normalized_range_prov <- as.data.frame(lapply(dati_2015[,3:18], normalize_by_range))
df_normalized_range<- df_normalized_range_prov[,-3]
boxplot(df_normalized_range)

# Calcola la PCA
pca <- prcomp(df_normalized_range, scale=F)
summary(pca)

# Visualizza il grafico a dispersione in 2d
plot(pca$x[,1], pca$x[,2], main = "2D PC Plane (Normalized by Range)", xlab = "PC1", ylab = "PC2")

#ora in 3d sulle prime tre principal component
scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], main = "3D PC Plane (Normalized by Range)", xlab = "PC1", ylab = "PC2", zlab = "PC3")


pca <- princomp(df_normalized_range, scores=T)
load.basket<- pca$loadings
load.basket

pca <- prcomp(dati_2015, scale=F)
summary(pca)

# Visualizza il grafico a dispersione
plot(pca$x[,1], pca$x[,2], main = "2D PC Plane (Normalized by Standard Deviations)", xlab = "PC1", ylab = "PC2")

#ora in 3d sulle prime tre principal component
scatterplot3d(pca$x[,1], pca$x[,2], pca$x[,3], main = "3D PC Plane (Normalized by Standard Deviations)", xlab = "PC1", ylab = "PC2", zlab = "PC3")

pca <- princomp(dati_2015, scores=T)
load.basket<-pca$loadings
load.basket
