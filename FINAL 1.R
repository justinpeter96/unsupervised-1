rm(list=ls())

### Initialize data sets to work with
# Webscrape Statistics from Baskballreference
library(rvest)
thelink <- xml2::read_html("https://www.basketball-reference.com/leagues/NBA_2021_per_game.html")
nba <- as.data.frame(thelink %>%html_nodes("table")%>%.[[1]] %>%html_table())

# Remove repeated header row
unn <- subset(nba, nba$Rk != 'Rk')

# Remove duplicated player records (duplicated if player traded, keep only 'Total' record)
nba2 <- unn[!duplicated(unn$Player),]

# Convert regressor variables to integer values
i <- c(6:30)
nba2[ , i] <- apply(nba2[ , i], 2, function(x) as.numeric(as.character(x)))

# Filter out players not having played >= [pct] of total games played
pct <- .7 #define percentage of games player must have played in as compared to max player
nba3 <- subset(nba2,nba2$G >= max(nba2$G)*pct)

# Remove attempts and attempts made columns (as they are simple divisors and dividends of percentage columns)
#nba4 <- nba3[-c(1,2,4:8,9,10,12,13,15,16,19,20)]
#summary(nba4)

################### CSV SAVED ON APRIL 8th 2021 ####################
#write.csv(nba3, "nba3.csv")
nba3 <- read.csv('~/Desktop/STAT6440/6440-Project/nba3.csv')
#nba3 <- read.csv('C:/Users/super/Documents/MSA/Sp21/MSA 6440/Final Project/nba3.csv')
colnames(nba3)[11] <- 'FG%'
colnames(nba3)[12] <- '3P'
colnames(nba3)[13] <- '3PA'
colnames(nba3)[14] <- '3P%'
colnames(nba3)[15] <- '2P'
colnames(nba3)[16] <- '2PA'
colnames(nba3)[17] <- '2P%'
colnames(nba3)[18] <- 'eF%'
colnames(nba3)[21] <- 'FT%'

nba4 <- nba3[-c(1,2,4:8,9,10,12,13,15,16,19,20)]

#nba4 <- na.omit(nba3) #remove records with 'na's
nba3$`3P%`[is.na(nba3$`3P%`)]<-0 
nba4$`3P%`[is.na(nba4$`3P%`)]<-0 
summary(nba4)

# Construct position specific dataframes and assign values to specific positions
table(nba4$Pos)

nba3[nba3$Pos =='C-PF',] # Kelly Olynyk
nba3[nba3$Pos =='PG-SG',] # James Harden
nba3[nba3$Pos =='SF-SG',] # Sviatoslav Mykhailiuk
nba3[nba3$Pos =='SG-PG',] # Delon Wright
nba3[nba3$Pos =='SG-SF',] # Hamidou Diallo & Norman Powell

################# KNN TO DEFINE PLAYERS ################# 
#### Olynyk: C OR PF?? ####
source("~/Desktop/STAT6440/W2/myfunctions.R")
#source("C:/Users/super/Documents/MSA/Sp21/MSA 6440/Data/myfunctions.R")

knn <- subset(nba4,nba4$Pos =='C' | nba4$Pos =='PF')

knn$Pos <- as.factor(knn$Pos)

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knn, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

### Rescale the data
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE)
training.scaled.wY <- cbind(training.scaled, training.data[,1])
training.scaled.attr <- attributes(training.scaled)

test.scaled <- scale(test.data[,-1], 
                     center = training.scaled.attr$`scaled:center`, 
                     scale = training.scaled.attr$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
library(FNN)
library(caret)

K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = test.scaled,
             cl = training.data[,1], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(test.data[,1]), 
                       positive = "C")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "#FC4E07")

max.k <- which.max(kappa)
max.k #1 is too low, use 13, next highest, instead
max.k <- 13

### Fit kNN model on a single new observation with k=[max.k] 
x <- nba4[(nba4$Pos=="C-PF"),]
x0 <- x[-1]

x0.scaled <- scale(x0, center = training.scaled.attr$`scaled:center`, scale = training.scaled.attr$`scaled:scale`) 

Knn <- knn(train = training.scaled, test = x0.scaled,
           cl = training.data[,1], k = max.k)
Knn

## labels of nearest neighbors
Knn.attr <- attributes(Knn)
training.data[Knn.attr$nn.index,1]
summary(training.data[Knn.attr$nn.index,1]) #Label as PF

#### HARDIN: PG OR SG?? ####
knn <- subset(nba4,nba4$Pos =='PG' | nba4$Pos =='SG')

knn$Pos <- as.factor(knn$Pos)

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knn, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

### Rescale the data
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE)
training.scaled.wY <- cbind(training.scaled, training.data[,1])
training.scaled.attr <- attributes(training.scaled)

test.scaled <- scale(test.data[,-1], 
                     center = training.scaled.attr$`scaled:center`, 
                     scale = training.scaled.attr$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = test.scaled,
             cl = training.data[,1], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(test.data[,1]), 
                       positive = "PG")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "#FC4E07")

max.k <- which.max(kappa)
max.k

### Fit kNN model on a single new observation with k=[max.k] 
x <- nba4[(nba4$Pos=="PG-SG"),]
x0 <- x[-1]

x0.scaled <- scale(x0, center = training.scaled.attr$`scaled:center`, scale = training.scaled.attr$`scaled:scale`) 

Knn <- knn(train = training.scaled, test = x0.scaled,
           cl = training.data[,1], k = max.k)
Knn

## labels of nearest neighbors
Knn.attr <- attributes(Knn)
training.data[Knn.attr$nn.index,1]
summary(training.data[Knn.attr$nn.index,1]) #label as PG

#### Mykhailiuk : SF OR SG?? ####
knn <- subset(nba4,nba4$Pos =='SF' | nba4$Pos =='SG')

knn$Pos <- as.factor(knn$Pos)

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knn, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

### Rescale the data
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE)
training.scaled.wY <- cbind(training.scaled, training.data[,1])
training.scaled.attr <- attributes(training.scaled)

test.scaled <- scale(test.data[,-1], 
                     center = training.scaled.attr$`scaled:center`, 
                     scale = training.scaled.attr$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = test.scaled,
             cl = training.data[,1], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(test.data[,1]), 
                       positive = "SF")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "#FC4E07")

max.k <- which.max(kappa)
max.k

### Fit kNN model on a single new observation with k=[max.k] 
x <- nba4[(nba4$Pos=="SF-SG"),]
x0 <- x[-1]

x0.scaled <- scale(x0, center = training.scaled.attr$`scaled:center`, scale = training.scaled.attr$`scaled:scale`) 

Knn <- knn(train = training.scaled, test = x0.scaled,
           cl = training.data[,1], k = max.k)
Knn

## labels of nearest neighbors
Knn.attr <- attributes(Knn)
training.data[Knn.attr$nn.index,1]
summary(training.data[Knn.attr$nn.index,1]) #label as SG

#### Wright: SG OR PG?? ####
knn <- subset(nba4,nba4$Pos =='SG' | nba4$Pos =='PG')

knn$Pos <- as.factor(knn$Pos)

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knn, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

### Rescale the data
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE)
training.scaled.wY <- cbind(training.scaled, training.data[,1])
training.scaled.attr <- attributes(training.scaled)

test.scaled <- scale(test.data[,-1], 
                     center = training.scaled.attr$`scaled:center`, 
                     scale = training.scaled.attr$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = test.scaled,
             cl = training.data[,1], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(test.data[,1]), 
                       positive = "SG")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "#FC4E07")

max.k <- which.max(kappa)
max.k

### Fit kNN model on a single new observation with k=[max.k] 
x <- nba4[(nba4$Pos=="SG-PG"),]
x0 <- x[-1]

x0.scaled <- scale(x0, center = training.scaled.attr$`scaled:center`, scale = training.scaled.attr$`scaled:scale`) 

Knn <- knn(train = training.scaled, test = x0.scaled,
           cl = training.data[,1], k = max.k)
Knn

## labels of nearest neighbors
Knn.attr <- attributes(Knn)
training.data[Knn.attr$nn.index,1]
summary(training.data[Knn.attr$nn.index,1]) #label as SG

#### Diallo & Powell: SG OR SF?? ####
knn <- subset(nba4,nba4$Pos =='SG' | nba4$Pos =='SF')

knn$Pos <- as.factor(knn$Pos)

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knn, 0.7)
training.data <- p2$data.train
test.data <- p2$data.test

### Rescale the data
training.scaled <- scale(training.data[,-1], center = TRUE, scale = TRUE)
training.scaled.wY <- cbind(training.scaled, training.data[,1])
training.scaled.attr <- attributes(training.scaled)

test.scaled <- scale(test.data[,-1], 
                     center = training.scaled.attr$`scaled:center`, 
                     scale = training.scaled.attr$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  Knn <- knn(train = training.scaled, test = test.scaled,
             cl = training.data[,1], k = kk)
  c <- confusionMatrix(as.factor(Knn), as.factor(test.data[,1]), 
                       positive = "SG")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "#FC4E07")

max.k <- which.max(kappa)
max.k

### Fit kNN model on a single new observation with k=[max.k] 
x <- nba4[(nba4$Pos=="SG-SF"),]
x0 <- x[1,][-1] #Diallo
x1 <- x[2,][-1] #Powell

x0.scaled <- scale(x0, center = training.scaled.attr$`scaled:center`, scale = training.scaled.attr$`scaled:scale`) 
x1.scaled <- scale(x1, center = training.scaled.attr$`scaled:center`, scale = training.scaled.attr$`scaled:scale`) 

Knn0 <- knn(train = training.scaled, test = x0.scaled,
            cl = training.data[,1], k = max.k)
Knn0

Knn1 <- knn(train = training.scaled, test = x1.scaled,
            cl = training.data[,1], k = max.k)
Knn1

## labels of nearest neighbors
Knn.attr <- attributes(Knn0)
training.data[Knn.attr$nn.index,1]
summary(training.data[Knn.attr$nn.index,1]) 

table(knn$Pos) #label Diallo as SG MAJORITY RULE

Knn.attr <- attributes(Knn1)
training.data[Knn.attr$nn.index,1]
summary(training.data[Knn.attr$nn.index,1]) #label Powell as SG

#################### POP OUT OF KNN CLASSIFICIATION ###################
#define Olynyk as KNN result, viz. "PF"
nba4$Pos[nba4$Pos=="C-PF"] <- "PF"
nba3$Pos[nba3$Pos=="C-PF"] <- "PF"

#define Hardin as KNN result, viz. "PG"
nba4$Pos[nba4$Pos=="PG-SG"] <- "PG"
nba3$Pos[nba3$Pos=="PG-SG"] <- "PG"

#define Mykhailiuk  as KNN result, viz. "SG"
nba4$Pos[nba4$Pos=="SF-SG"] <- "SG"
nba3$Pos[nba3$Pos=="SF-SG"] <- "SG"

#define Write as KNN result, viz. "SG"
nba4$Pos[nba4$Pos=="SG-PG"] <- "SG"
nba3$Pos[nba3$Pos=="SG-PG"] <- "SG"

#define Powell as KNN result, viz. "SG"
nba4$Pos[nba4$Pos=="SG-SF"] <- "SG"
nba3$Pos[nba3$Pos=="SG-SF"] <- "SG"

Dat <- nba4
Dat2 <- nba3[-c(1,4:8,9,10,12,13,15,16,19,20)]

Dat$Pos <- as.factor(Dat$Pos)

table(nba4$Pos) #good

################### CSV SAVED ON APRIL 11th 2021 ####################
#write.csv(Dat2, "NBAper.csv")

## Create training and test data ##
#source("C:/Users/super/Documents/MSA/Sp21/MSA 6440/Data/myfunctions.R")

RNGkind (sample.kind = "Rounding") 
set.seed(0) ## set seed so that you get same partition each time
p2 <- partition.2(Dat, 0.7) ## creating 70:30 partition
training.data <- p2$data.train
test.data <- p2$data.test

nba.nolbl <- training.data[,-1]
nba.scaled <- scale(nba.nolbl, center = TRUE, scale = TRUE)

# https://predictivehacks.com/how-to-determine-the-number-of-clusters-of-k-means-in-r/
library(factoextra)
library(NbClust)
library(mclust)

set.seed(123)

fviz_nbclust(nba.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(nba.scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(nba.scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method") # none of these charts give us good idea for number of Clusters

############# Clustering ##################
#### Mclust #####
library(mclust)
RNGkind (sample.kind = "Rounding") 
set.seed(0)

mc.list <- list()
bic <- NULL
for (k in 1:10){
  mc <- Mclust(Dat[-1], k)
  bic <- c(bic, mc$bic)
  mc.list[[k]] <- c(mc.list, mc)
}

plot(seq(1,10,1), bic, type = "l", xlab = "Number of clusters")

plot(bic[1:10], type = "l", xlab = "Number of clusters")

maxbic <- which.max(bic[1:10])
maxbic

mc3 <- Mclust(Dat[-1], maxbic)

table(mc3$classification, Dat$Pos)

lblper <- c('FG%','3P%','2P%','eFG%','FT%')
lbloff <- c('ORB','AST','PTS')
lbldef <- c('BLK','STL','DRB')
lblsp <- c('TOV','PF')

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,5), c(0,1), type = "n", xlab = "", ylab = "Est. Mean", 
     xaxt='n', main = "Clustering using Mclust: Percentages")
axis(side = 1, at=seq(1,5,1), las=2, labels = lblper)
xseq <- seq(1,5,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, mc3$parameters$mean[1:5,k], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,3), c(0,20), type = "n", xlab = "", ylab = "Est. Mean", 
     xaxt='n', main = "Clustering using Mclust: Offense")
axis(side = 1, at=seq(1,3,1), las=2, labels = lbloff)
xseq <- seq(1,3,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, mc3$parameters$mean[c(6,9,14),k], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,3), c(0,5), type = "n", xlab = "", ylab = "Est. Mean", 
     xaxt='n', main = "Clustering using Mclust: Defense")
axis(side = 1, at=seq(1,3,1), las=2, labels = lbldef)
xseq <- seq(1,3,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, mc3$parameters$mean[c(11,10,7),k], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,2), c(0,3), type = "n", xlab = "", ylab = "Est. Mean", 
     xaxt='n', main = "Clustering using Mclust: Special")
axis(side = 1, at=seq(1,2,1), las=2, labels = lblsp)
xseq <- seq(1,2,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, mc3$parameters$mean[c(12,13),k], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

# Mclust Clustering Plots
library(tidyr)
library(ggplot2)

dat.mc <- as.data.frame(t(mc3$parameters$mean))
dat.mc$clusters <- row.names(dat.mc)

# Percentages Clusters
dat.mc1 <- dat.mc[c(1:5,15)]

dat.g <- gather(dat.mc1, stats, mc3mean, -clusters)

mc.1 <- ggplot(dat.g, aes(stats, mc3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Mclust: Percentages")

mc.1

# Offense Clusters
dat.mc1 <- dat.mc[c(6,9,14,15)]

dat.g <- gather(dat.mc1, stats, mc3mean, -clusters)

mc.2 <- ggplot(dat.g, aes(stats, mc3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Mclust: Offense")

mc.2

# Defense Clusters
dat.mc1 <- dat.mc[c(11,10,7,15)]

dat.g <- gather(dat.mc1, stats, mc3mean, -clusters)

mc.3 <- ggplot(dat.g, aes(stats, mc3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Mclust: Defense")

mc.3

# Special Clusters
dat.mc1 <- dat.mc[c(12,13,15)]

dat.g <- gather(dat.mc1, stats, mc3mean, -clusters)

mc.4 <- ggplot(dat.g, aes(stats, mc3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Mclust: Special")

mc.4

#################
#### Kmeans #####
#################
RNGkind (sample.kind = "Rounding") 
set.seed(1)

### Rescale the data
nba.scaled <- scale(Dat[-1], center = TRUE, scale = TRUE)
nba.scaled.attr <- attributes(nba.scaled)

km.list <- list()
wss <- NULL
for (k in 1:10){
  tmp.wss <- NULL
  ## Running kmeans 10 times for each k to get the best result
  for (itr in 1:10){
    set.seed(itr)
    km <- kmeans(nba.scaled, k)
    tmp.wss <- c(tmp.wss, km$tot.withinss)
  }
  select.seed <- which.min(tmp.wss)
  set.seed(select.seed)
  km <- kmeans(nba.scaled, k)
  wss <- c(wss, km$tot.withinss)
  km.list[[k]] <- c(km.list, km)
}

plot(seq(1,10,1), wss, type = "l", xlab = "Number of clusters")

km3 <- kmeans(nba.scaled, 3) #to match nicely with mclust choose 3, can try 4, too

table(mc3$classification, km3$cluster)

# Manually match the labels, as much as possible, 
# to the mclust classification for easier comparison
km3id <- ifelse(km3$cluster == 3, 2, 
                ifelse(km3$cluster == 2, 3,1))

table(km3id, Dat$Pos)
table(mc3$classification, km3id)

# Convert the kmeans centroids to the original scale for plotting
km3.centers.rescaled <-  km3$centers
for (j in 1:14){
  km3.centers.rescaled[,j] <- nba.scaled.attr$`scaled:center`[j] +
    km3$centers[,j]*nba.scaled.attr$`scaled:scale`[j]
}
# Now change the indices to approximately match 
# mclust classification for easier comparison
km3.centers.rescaled.tmp <- km3.centers.rescaled
km3.centers.rescaled[1,] <- km3.centers.rescaled.tmp[1,]
km3.centers.rescaled[2,] <- km3.centers.rescaled.tmp[3,]
km3.centers.rescaled[3,] <- km3.centers.rescaled.tmp[2,]

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,5), c(0,1), type = "n", xlab = "", ylab = "Kmeans centroids", 
     xaxt='n', main = "Clustering using Kmeans: Percentages")
axis(side = 1, at=seq(1,5,1), las=2, labels = lblper)
xseq <- seq(1,5,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, km3.centers.rescaled[k,1:5], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,3), c(0,6), type = "n", xlab = "", ylab = "Kmeans centroids", 
     xaxt='n', main = "Clustering using Kmeans: Offense")
axis(side = 1, at=seq(1,3,1), las=2, labels = lbloff)
xseq <- seq(1,3,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, km3.centers.rescaled[k,c(6,9,14)], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,3), c(0,6), type = "n", xlab = "", ylab = "Kmeans centroids", 
     xaxt='n', main = "Clustering using Kmeans: Defense")
axis(side = 1, at=seq(1,3,1), las=2, labels = lbldef)
xseq <- seq(1,3,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, km3.centers.rescaled[k,c(11,10,7)], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

par(oma=c(6,2,1,1), mar = c(4,4,2,1) + 0.1)
plot(c(0,2), c(0,3), type = "n", xlab = "", ylab = "Kmeans centroids", 
     xaxt='n', main = "Clustering using Kmeans: Defense")
axis(side = 1, at=seq(1,2,1), las=2, labels = lblsp)
xseq <- seq(1,2,1)
col.seq <- c("#00AFBB","#E7B800","#FC4E07")
for (k in 1:3){
  lines(xseq, km3.centers.rescaled[k,c(12,13)], col=col.seq[k], lty=2)
}
legend("topleft", col = col.seq, lty = 2,
       legend = c("Cluster 1", "Cluster 2", "Cluster 3"))
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/uni_mclust.pdf")

# Kmeans Clustering Plots
library(tidyr)
library(ggplot2)

dat.km <- as.data.frame(km3.centers.rescaled)
dat.km$clusters <- row.names(dat.km)

# Percentages Clusters
dat.km1 <- dat.km[c(1:5,15)]

dat.g <- gather(dat.km1, stats, km3mean, -clusters)

km.1 <- ggplot(dat.g, aes(stats, km3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Kmeans: Percentages")

km.1

# Offense Clusters
dat.km1 <- dat.km[c(6,9,14,15)]

dat.g <- gather(dat.km1, stats, km3mean, -clusters)

km.2 <- ggplot(dat.g, aes(stats, km3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Kmeans: Offense")

km.2 

# Defense Clusters
dat.km1 <- dat.km[c(11,10,7,15)]

dat.g <- gather(dat.km1, stats, km3mean, -clusters)

km.3 <- ggplot(dat.g, aes(stats, km3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Kmeans: Defense")

km.3

# Special Clusters
dat.km1 <- dat.km[c(12,13,15)]

dat.g <- gather(dat.km1, stats, km3mean, -clusters)

km.4 <- ggplot(dat.g, aes(stats, km3mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Kmeans: Special")

km.4

#### Hierarchical #####
### Create distance matrix
d <- dist(nba.scaled, method = "euclidean") 

hc.complete <- hclust(d, method="ward.D2")
plot(hc.complete) # display dendogram
hc.complete.id <- cutree(hc.complete, k=3) # cut tree into 3 clusters
rect.hclust(hc.complete, k = 3, border = "red")
#dev.copy2pdf(file = "E:/Data mining/Lecture Notes/plots/dend1.pdf")

nba.hier <- Dat[-1]
nba.hier$clusters <- as.data.frame(hc.complete.id)

temp1 <- subset(nba.hier,nba.hier$clusters =='1')
temp2 <- subset(nba.hier,nba.hier$clusters =='2')
temp3 <- subset(nba.hier,nba.hier$clusters =='3')

a <- colMeans(temp1)
b <- colMeans(temp2)
c <- colMeans(temp3)

dat.hier <- rbind(a,b,c)
dat.hier <- as.data.frame(dat.hier)
dat.hier$clusters <- as.character(dat.hier$clusters)

# Hierarchical Clustering Plots
library(tidyr)
library(ggplot2)

# Percentages Clusters
dat.hier1 <- dat.hier[c(1:5,15)]

dat.g <- gather(dat.hier1, stats, hier4mean, -clusters)

hier.1 <- ggplot(dat.g, aes(stats, hier4mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Hierarchical: Percentages")

hier.1

# Offense Clusters
dat.hier1 <- dat.hier[c(6,9,14,15)]

dat.g <- gather(dat.hier1, stats, hier4mean, -clusters)

hier.2 <- ggplot(dat.g, aes(stats, hier4mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Hierarchical: Offense")

hier.2

# Defense Clusters
dat.hier1 <- dat.hier[c(11,10,7,15)]

dat.g <- gather(dat.hier1, stats, hier4mean, -clusters)

hier.3 <- ggplot(dat.g, aes(stats, hier4mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Hierarchical: Defense")

hier.3

# Special Clusters
dat.hier1 <- dat.hier[c(12,13,15)]

dat.g <- gather(dat.hier1, stats, hier4mean, -clusters)

hier.4 <- ggplot(dat.g, aes(stats, hier4mean)) + 
  geom_bar(aes(fill = clusters), stat = "identity", position = "dodge") +
  ggtitle("Clustering using Hierarchical: Special")

hier.4

# Table analysis
t <- table(km3id, hc.complete.id)
colnames(t) <- c("H 1", "H 2", "H 3")
rownames(t) <- c("KM 1", "KM 2", "KM 3")

t2 <- table(mc3$classification, hc.complete.id)
colnames(t2) <- c("H 1", "H 2", "H 3")
rownames(t2) <- c("MC 1", "MC 2", "MC 3")

table(mc3$classification, km3id)
t
t2

nba.clust <- cbind(Dat2, mc3$classification, km3id,hc.complete.id)
colnames(nba.clust)[17] <- 'MCid'
colnames(nba.clust)[18] <- 'KMid'
colnames(nba.clust)[19] <- 'Hierid'

nba.clust$MCid <- as.factor(nba.clust$MCid)
nba.clust$KMid <- as.factor(nba.clust$KMid)
nba.clust$Hierid <- as.factor(nba.clust$Hierid)

nba.clust

################### CSV SAVED ON APRIL 9th 2021 ####################
#write.csv(nba.clust, "nba.clust3.csv")

# Compare Mcluster, Kmeans, and Hierarchical Clusters
library(gridExtra)
grid.arrange(mc.1, km.1, hier.1, nrow=1, ncol=3)
grid.arrange(mc.2, km.2, hier.2, nrow=1, ncol=3)
grid.arrange(mc.3, km.3, hier.3, nrow=1, ncol=3)
grid.arrange(mc.4, km.4, hier.4, nrow=1, ncol=3)

# Clusters and Positions
nbatoday <- nba.clust[,c(1,2,17,18,19)]

dat.g <- gather(nbatoday[-1], type, cluster,-Pos)

today <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("2020 NBA Cluster Distributions")

posclus.mc <- ggplot(nbatoday[-1], aes(Pos)) + 
  geom_bar(aes(fill = MCid), stat = "count", position = "dodge") +
  ggtitle("2020 NBA Position-Cluster Relation Mclust")

posclus.km <- ggplot(nbatoday[-1], aes(Pos)) + 
  geom_bar(aes(fill = KMid), stat = "count", position = "dodge") +
  ggtitle("2020 NBA Position-Cluster Relation Kmeans")

posclus.h <- ggplot(nbatoday[-1], aes(Pos)) + 
  geom_bar(aes(fill = Hierid), stat = "count", position = "dodge") +
  ggtitle("2020 NBA Position-Cluster Relation Hierarchical")

posclus <- ggplot(dat.g, aes(Pos)) + 
  geom_bar(aes(fill = cluster), stat = "count", position = "dodge") +
  ggtitle("2020 NBA Position-Cluster Relation Concatenated")

today
posclus.mc
posclus.km
posclus.h
posclus

# Pairs plot view clusters
nba.nolbl <- nba.clust[,-c(1,2,17,18,19)]
nba.lbl.mc <- nba.clust[,17]
nba.lbl.km <- nba.clust[,18]
nba.lbl.h <- nba.clust[,19]
n <- nrow(nba.nolbl)
p <- ncol(nba.nolbl)

col.seq <- c("red", "darkgreen", "navy")
species.col.mc <- col.seq[as.numeric(nba.lbl.mc)]
species.col.km <- col.seq[as.numeric(nba.lbl.km)]
species.col.h <- col.seq[as.numeric(nba.lbl.h)]

# Mclust
par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[1:5], col = species.col.mc,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.mc)),
       fill = unique(species.col.mc))
par(xpd = NA)

par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[c(6,9,14,12)], col = species.col.mc,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.mc)),
       fill = unique(species.col.mc))
par(xpd = NA)

par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[c(11,10,7,13)], col = species.col.mc,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.mc)),
       fill = unique(species.col.mc))
par(xpd = NA)

# Kmeans
par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[1:5], col = species.col.km,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.km)),
       fill = unique(species.col.km))
par(xpd = NA)

par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[c(6,9,14,12)], col = species.col.km,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.km)),
       fill = unique(species.col.km))
par(xpd = NA)

par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[c(11,10,7,13)], col = species.col.km,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.km)),
       fill = unique(species.col.km))
par(xpd = NA)

# Hierarchical
par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[1:5], col = species.col.h,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.h)),
       fill = unique(species.col.h))
par(xpd = NA)

par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[c(6,9,14,12)], col = species.col.h,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.h)),
       fill = unique(species.col.h))
par(xpd = NA)

par(oma=c(2,2,4,1), mar = c(4,4,4,1) + 0.1)
pairs(nba.nolbl[c(11,10,7,13)], col = species.col.h,
      lower.panel = NULL, cex.labels=2, pch=19, cex = 1.2)
mtext(text= "Model Based Cluster",side=3,line=3, font=2,
      cex = 1.25,outer=TRUE)
legend(x = 0.005, y = 0.2, cex = 1.2, bty = "n",
       legend = as.character(levels(nba.lbl.h)),
       fill = unique(species.col.h))
par(xpd = NA)

# Help us define Cluster with background knowledge of NBA (e.g. height, strength, weight, etc.)
MCid1 <- nba.clust[(nba.clust$MCid=="1"),]
MCid2 <- nba.clust[(nba.clust$MCid=="2"),]
MCid3 <- nba.clust[(nba.clust$MCid=="3"),]

MCid1
MCid2
MCid3

KMid1 <- nba.clust[(nba.clust$KMid=="1"),]
KMid2 <- nba.clust[(nba.clust$KMid=="2"),]
KMid3 <- nba.clust[(nba.clust$KMid=="3"),]

KMid1
KMid2
KMid3

Hid1 <- nba.clust[(nba.clust$Hierid=="1"),]
Hid2 <- nba.clust[(nba.clust$Hierid=="2"),]
Hid3 <- nba.clust[(nba.clust$Hierid=="3"),]

Hid1
Hid2
Hid3
