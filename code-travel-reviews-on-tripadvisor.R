# 1. Loading the dataset

dataset_tripadvisor <- read.csv('data/dataset-tripadvisor.csv')

# Removing the ‘User Id’ column and use it as the row header
rownames(dataset_tripadvisor) <- dataset_tripadvisor$User.ID
tripadvisor <- dataset_tripadvisor[, -1]


# 2. Loading the libraries 

library(car)
library(corrplot)
library(knitr)
library(ade4)
library(plotrix)
library(cluster)
library(factoextra)
library(plotly)

# 3. Renaming columns and assign colours to columns

colnames(tripadvisor) <- c("Art", "Clubs", "Juices", "Rests.", "Museums", 
                           "Resorts", "Parks", "Beaches", "Theaters", "Religious")
valeurs_couleurs <- c(
  "lavender",      # Art
  "mistyrose",     # Clubs 
  "wheat1",        # Juices
  "rosybrown1",    # Rests.
  "antiquewhite2", # Museums
  "paleturquoise1",# Resorts 
  "darkseagreen1", # Parks
  "lightblue1",    # Beaches
  "lightpink1",    # Theaters 
  "lemonchiffon1"  # Religious
)
mes_couleurs <- valeurs_couleurs
names(mes_couleurs) <- colnames(tripadvisor)

  
 
 
  # The Exploration
  
  
  ## Key indicators


# 1. Creating a table summarising the data (minimum, maximum, median, mean, quantiles, standard deviations)

# Calculate each statistic separately for all columns
mins    <- sapply(tripadvisor, min)
q1s     <- sapply(tripadvisor, quantile, probs = 0.25)
medians <- sapply(tripadvisor, median)
means   <- sapply(tripadvisor, mean)
q3s     <- sapply(tripadvisor, quantile, probs = 0.75)
maxs    <- sapply(tripadvisor, max)
sds     <- sapply(tripadvisor, sd)
# Compile all the data into a data frame
tableau_resume <- data.frame(
  Minimum = mins,
  Q1 = q1s,
  Median = medians,
  Mean = means,
  Q3 = q3s,
  Maximum = maxs,
  SD = sds
)
# Round the figures so that they are easy to read (to two decimal places)
tableau_resume <- round(tableau_resume, 2)
# Display the result in the console
print(tableau_resume)

# BARPLOT
# Margins
par(mar=c(5, 10, 4, 2))
# Horizontal barplot of mean rating
bp <- barplot(sort(means), 
              horiz = TRUE,               
              las = 1,
              col = mes_couleurs[names(sort(colMeans(tripadvisor, na.rm = TRUE)))],
              main = "Mean rating per activity",
              xlab = "Mean rating",
              border = NA,
              xlim = c(0, 4))
# Adding values
text(x = sort(means), 
     y = bp, 
     labels = round(sort(means), 2), 
     pos = 4,
     cex = 0.8,
     col = "black")
# Adding a visual marker
abline(v = mean(sort(means)), col = rgb(0, 0, 0, 0.4), lty = 2, lwd = 2)

# BARPLOT
# Graphical window
par(mar = c(5, 10, 4, 4))
# Horizontal barplot of standard deviations
barplot(sort(sds),
        horiz = TRUE, 
        las = 1,
        col = mes_couleurs[names(sort(sds))],
        main = "Variations in opinions", 
        xlab = "Variation in ratings",
        border = NA,
        xlim = c(0, max(sds) * 1.2))
# Adding values
text(x = sort(sds), 
     y = bp, 
     labels = round(sort(sds), 2), 
     pos = 4,      
     cex = 0.8,
     col = "#505050")
# Adding a visual marker
abline(v = mean(sort(sds)), col = rgb(0, 0, 0, 0.4), lty = 2, lwd = 2)

# HISTOGRAM FOR EACH ACTIVITY AND NORMAL DISTRIBUTION
# Graphical window
par(mfrow=c(2, 5), mar=c(2, 2, 2, 1)) 
# Display
for (i in 1:ncol(tripadvisor)) { 
  hist(tripadvisor[, i], 
       main = names(tripadvisor)[i],     
       xlab = "", ylab = "",
       col = mes_couleurs[names(tripadvisor)[i]],  
       border = "white",        
       breaks = 15,
       prob = TRUE,
       main.cex = 1.2,
       yaxt = "n")          
  x_vals <- seq(min(tripadvisor[, i]), max(tripadvisor[, i]), length = 100)
  y_vals <- dnorm(x_vals, mean = mean(tripadvisor[, i]), sd = sd(tripadvisor[, i]))
  lines(x_vals, y_vals, col = "#505050", lwd = 1.5)
}
# Reset
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)

# BARPLOT FOR PARKS
# Display
barplot(table(tripadvisor$Parks), 
        main = "Rating scale: Parks",
        xlab = "Rating assigned",
        ylab = "Number of users",
        col= "darkseagreen1", 
        border = NA,
        las = 1)

# BOXPLOTS FOR EACH ACTIVITIES
# Margins
par(mar=c(8, 4, 2, 1)) 
# Display
boxplot(tripadvisor, 
        main = NULL,
        col = mes_couleurs[names(tripadvisor)], 
        las = 2, 
        ylab = "Rating assigned")
# Reset
par(mar=c(5, 4, 4, 2)) 

# CORRELATION MATRIX
# Display
corrplot(cor(tripadvisor), 
         method = "ellipse",       
         type = "upper",           
         col = COL2('RdBu', 10),   
         addCoef.col = "black",    
         number.cex = 0.7,        
         tl.col = "black",         
         tl.srt = 45,              
         diag = FALSE,             
         order = "hclust",        
         addrect = 3,              
         rect.col = "black",      
         rect.lwd = 2,             
         mar = c(0,0,1,0))

# SCATTERPLOT AND REGRESSION LINE
# Custom function for the bottom panel
panel.custom <- function(x, y, ...) {
  points(x, y, pch = 20, col = rgb(0, 0, 0, 0.3), cex = 0.5)
  abline(lm(y ~ x), col = "#e78ac3", lwd = 2)
}
# Display
pairs(tripadvisor, 
      lower.panel = panel.custom, 
      upper.panel = NULL,         
      diag.panel = NULL,          
      gap = 0,
      cex.labels = 0.8)

# The relationship between Parks and Juices
scatterplot(Parks ~ Juices, 
            data = tripadvisor, 
            pch = 19,                  
            cex = 0.5,
            col = rgb(0, 0, 0, 0.4),
            ylim = c(2, 4),
            cex.lab = 1.25,
            cex.main = 1.5,
            cex.axis = 1.125,
            col.main = "#e78ac3",
            regLine = list(col = "#e78ac3", lwd = 2),
            smooth = FALSE,
            ellipse = FALSE,
            boxplots = FALSE,
            grid = FALSE, 
            xlab = "Juice Bars ratings", 
            ylab = "Parks ratings",
            main = paste("Strong correlation (R =", round(cor(tripadvisor$Juices, tripadvisor$Parks, use = "complete.obs"), 2), ") but with no impact"))

# Relationship entre Museums et Resorts
scatterplot(Museums ~ Resorts, 
            data = tripadvisor, 
            pch = 19,                  
            cex = 0.5,
            col = rgb(0, 0, 0, 0.4),
            cex.lab = 1.25,
            cex.main = 1.5,
            cex.axis = 1.125,
            col.main = "#e78ac3",
            regLine = list(col = "#e78ac3", lwd = 2),
            smooth = FALSE,
            ellipse = list(levels = 0.95, col="#e78ac3", fill=TRUE, fill.alpha=0.1),
            boxplots = FALSE,
            grid = FALSE, 
            xlab = "Resorts ratings", 
            ylab = "Museums ratings",
            main = paste("Active relationship (R =", round(cor(tripadvisor$Museums, tripadvisor$Resorts, use = "complete.obs"), 2), ")"))

# Relationship between Beaches and Resorts
scatterplot(Beaches ~ Resorts, 
            data = tripadvisor, 
            pch = 19,                  
            cex = 0.5,
            col = rgb(0, 0, 0, 0.4),
            cex.lab = 1.25,
            cex.main = 1.5,
            cex.axis = 1.125,
            col.main = "#e78ac3",
            regLine = list(col = "#e78ac3", lwd = 2),
            smooth = FALSE,
            ellipse = list(levels = 0.95, col="#e78ac3", fill=TRUE, fill.alpha=0.1),
            boxplots = FALSE,
            grid = FALSE, 
            xlab = "Resorts ratings", 
            ylab = "Beaches ratings",
            main = paste("Independence of preferences (R =", round(cor(tripadvisor$Beaches, tripadvisor$Resorts, use = "complete.obs"), 2), ")"))


  # The structure


  ## Cleaning 

# 1. Preparing the data

# Remove "Parks" from the dataset as its variance is zero
tripadvisor2 <- tripadvisor[, colnames(tripadvisor) != "Parks"]

# 2. Calculating PCA (dudi.pca)

# Display, center = TRUE, scale = TRUE : standardised PCA
trip_acp <- dudi.pca(tripadvisor2, center = TRUE, scale = TRUE, scannf = FALSE, nf = 5)

# 3. Principal component analysis: how much information does it provide?

# Calculating percentages
eig_val <- trip_acp$eig
eig_pct <- round(eig_val / sum(eig_val) * 100, 1)

# PLOT OF SCREENING MATERIAL
# Graphical window
par(mfrow=c(1, 1), mar=c(5, 5, 4, 2))
# Display
bp <- barplot(eig_pct, 
              names.arg = paste0("Axis ", 1:length(eig_pct)),
              main = "Cascade of eigenvalues",
              ylab = "% variance",
              col = "#505050",
              ylim = c(0, 110))
# Adding points and lines for cumulative variance
cumexpvar<-round(cumsum(trip_acp$eig/sum(trip_acp$eig)*100),0)
lines(bp,cumexpvar,col ="#e78ac3")
points(bp,cumexpvar,col ="#e78ac3")
text(x = bp, 
     y = cumexpvar, 
     labels = paste0(round(cumexpvar, 0), "%"), 
     pos = 3,       
     cex = 0.8,    
     col = "#e78ac3", 
     font = 2)

# Table of contributions
# Calculation of contributions
contrib_var <- trip_acp$co^2
for(i in 1:5) {
  contrib_var[,i] <- (contrib_var[,i] / trip_acp$eig[i]) * 100
}
# Rounding 
tableau_arrondi <- round(contrib_var[, 1:5], 2)
# Creating a table
tableau_final <- as.data.frame(lapply(tableau_arrondi, function(x) paste0(x, " %")))
# Naming lines (activities) that disappear with lapply
rownames(tableau_final) <- rownames(contrib_var)
# Display of the results
print(tableau_final)
# Calculating mean
moyennes_calculees <- colMeans(tableau_arrondi) 
# Rounding and adding the %
moyenne_tableau <- as.data.frame(t(paste0(round(moyennes_calculees, 2), " %")))
# Naming of columns
colnames(moyenne_tableau) <- colnames(tableau_arrondi)
# Naming the line
rownames(moyenne_tableau) <- "Limit"
# Display
print(moyenne_tableau)

# CORRELATION CIRCLE
# Margins
par(mar=c(2, 2, 2, 2))
# Display
s.corcircle(trip_acp$co, 
            xax = 1, yax = 2,   
            label = rownames(trip_acp$co), 
            clabel = 1,        
            grid = TRUE,         
            sub = "Structure of activities, PC1 vs PC2", 
            csub = 1.2)

# PROJECTION OF INDIVIDUALS
s.label(trip_acp$li, 
        xax = 1, yax = 2,
        clabel = 0,
        cpoint = 1,
        pch = 20,
        sub = "User projection",
        csub = 1.2)


  # The segmentation


# 1. How many clusters ?

# Silhouette method
# Parameters
k.max <- 10
Silhouette <- rep(0, k.max)
for(i in 2:k.max){
  km.res <- kmeans(tripadvisor, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(tripadvisor))
  Silhouette[i] <- mean(ss[, 3])
}
# Display
plot(1:k.max, Silhouette, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k", main="Number of optimal clusters")
abline(v = which.max(Silhouette), lty = 2, col = "#e78ac3")

# 2. Hierarchical clustering of our individuals

# Hierarchical clustering of the dataset excluding "Parks"
distances <- dist(scale(tripadvisor2), method = "euclidean")
clustering_i <- hclust(distances, method = "ward.D2")
groupes_2 <- cutree(clustering_i, k = 2)
comptage_voyageurs <- table(groupes_2)

# Table of ratings mean
groupes_finaux <- cutree(clustering_i, k = 2)
profils <- aggregate(tripadvisor2, by = list(Cluster = groupes_finaux), FUN = mean)
rownames(profils) <- paste("Group", profils$Cluster)
profils <- profils[, -1] 
ligne_ecart <- abs(profils[1, ] - profils[2, ])
tableau_final <- rbind(profils, Difference = ligne_ecart)
tableau_final <- round(tableau_final, 2)
print(tableau_final)

# PIECHART
# Graphical window
par() 
# Display
pie(comptage_voyageurs, 
    main = "",
    col = c("#e78ac3", "#505050"), 
    labels = paste("Group", names(comptage_voyageurs), ":", comptage_voyageurs, "travelers"))

# 3D PLOT (CLUSTERS)
# Preparing 3D data
set.seed(123)
donnees_3d <- data.frame(
  Axe1 = trip_acp$li[, 1],
  Axe2 = trip_acp$li[, 2],
  Axe3 = trip_acp$li[, 3],
  Cluster = as.factor(groupes_2)
)
# Display
plot_ly(donnees_3d, 
        x = ~Axe1, 
        y = ~Axe2, 
        z = ~Axe3, 
        color = ~Cluster, 
        colors = c("#e78ac3", "#505050"), 
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 2, opacity = 0.8))%>%
  layout(
    showlegend = FALSE,
    scene = list(
      xaxis = list(title = "Axis 1", showgrid = TRUE),
      yaxis = list(title = "Axis 2", showgrid = TRUE),
      zaxis = list(title = "Axis 3", showgrid = TRUE),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
    )
  )

# 3. Hierarchical clustering of our activities

# Clustering of the transpose of the dataset excluding "Parks"
distances <- dist(t(tripadvisor2), method = "euclidian")
clustering_a <- hclust(distances, method = "ward.D2")

# DENDOGRAM
# Margins
par(mar=c(5, 4, 4, 2))
# Display
plot(clustering_a, 
     main = "Similarities between activities",
     xlab = "", 
     ylab = "",
     sub = "",
     hang = -1,
     col = "black",
     lwd = 2)
# Adding of a box to separate the groups
rect.hclust(clustering_a, k = 3, border = "#e78ac3")

