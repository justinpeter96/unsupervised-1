rm(list=ls())
library(rvest)
nba.clust.x <- read.csv('~/Desktop/STAT6440/6440-Project/nba.clust3.csv')
#nba.clust.x <- read.csv('C:/Users/super/Documents/MSA/Sp21/MSA 6440/Final Project/nba.clust3.csv')
colnames(nba.clust.x)[3] <- 'FG%'
colnames(nba.clust.x)[4] <- '3P%'
colnames(nba.clust.x)[5] <- '2P%'
colnames(nba.clust.x)[6] <- 'eF%'
colnames(nba.clust.x)[7] <- 'FT%'

nba.clust.x$MCid <- as.factor(nba.clust.x$MCid)
nba.clust.x$KMid <- as.factor(nba.clust.x$KMid)
nba.clust.x$Hierid <- as.factor(nba.clust.x$Hierid)

nba.clust <- nba.clust.x[-2]

####### Post Analysis Interesting Question: Champion Clusters ##############
####### 2020 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/2020.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

#### Lakers categorizations McCluster####
knnMC <- nba.clust[-c(1,17,18)]

RNGkind (sample.kind = "Rounding") 
set.seed(0)
source("~/Desktop/STAT6440/W2/myfunctions.R")
#source("C:/Users/super/Documents/MSA/Sp21/MSA 6440/Data/myfunctions.R")

### create 70:30 partition
p2 <- partition.2(knnMC, 0.7)
training.data.mc <- p2$data.train
test.data.mc <- p2$data.test

### Rescale the data
training.scaled.mc <- scale(training.data.mc[,-15], center = TRUE, scale = TRUE)
training.scaled.wY.mc <- cbind(training.scaled.mc, training.data.mc[,15])
training.scaled.attr.mc <- attributes(training.scaled.mc)

test.scaled.mc <- scale(test.data.mc[,-15], 
                        center = training.scaled.attr.mc$`scaled:center`, 
                        scale = training.scaled.attr.mc$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
library(FNN)
library(caret)

K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  knnMC <- knn(train = training.scaled.mc, test = test.scaled.mc,
               cl = training.data.mc[,15], k = kk)
  c <- confusionMatrix(as.factor(knnMC), as.factor(test.data.mc[,15]), 
                       positive = "C")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "blue")

max.kmc <- which.max(kappa)
max.kmc

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

######Lakers categorizations Kmeans#########
knnKM <- nba.clust[-c(1,16,18)]

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knnKM, 0.7)
training.data.km <- p2$data.train
test.data.km <- p2$data.test

### Rescale the data
training.scaled.km <- scale(training.data.km[,-15], center = TRUE, scale = TRUE)
training.scaled.wY.km <- cbind(training.scaled.km, training.data.km[,15])
training.scaled.attr.km <- attributes(training.scaled.km)

test.scaled.km <- scale(test.data.km[,-15], 
                        center = training.scaled.attr.km$`scaled:center`, 
                        scale = training.scaled.attr.km$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  knnMC <- knn(train = training.scaled.km, test = test.scaled.km,
               cl = training.data.km[,15], k = kk)
  c <- confusionMatrix(as.factor(knnMC), as.factor(test.data.km[,15]), 
                       positive = "C")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "blue")

max.kkm <- which.max(kappa)
max.kkm

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

###### categorizations Hierarchical #########
knnH <- nba.clust[-c(1,16,17)]

RNGkind (sample.kind = "Rounding") 
set.seed(0)
### create 70:30 partition
p2 <- partition.2(knnH, 0.7)
training.data.h <- p2$data.train
test.data.h <- p2$data.test

### Rescale the data
training.scaled.h <- scale(training.data.h[,-15], center = TRUE, scale = TRUE)
training.scaled.wY.h <- cbind(training.scaled.h, training.data.h[,15])
training.scaled.attr.h <- attributes(training.scaled.h)

test.scaled.h <- scale(test.data.h[,-15], 
                       center = training.scaled.attr.h$`scaled:center`, 
                       scale = training.scaled.attr.h$`scaled:scale`)

### fit k-nn model for k = 1, ..., 60
K <- 60
kappa <- rep(0, K)
for (kk in 1:K){
  knnMC <- knn(train = training.scaled.h, test = test.scaled.h,
               cl = training.data.h[,15], k = kk)
  c <- confusionMatrix(as.factor(knnMC), as.factor(test.data.h[,15]), 
                       positive = "C")
  kappa[kk] <- c$overall["Kappa"]
  cat("K", kk, "Kappa", kappa[kk], "\n")
}

# create a plot for k vs. kappa
plot(c(1:K), kappa, xlab = "k", ylab = "Kappa", type = "l", col = "blue")

max.h <- which.max(kappa)
max.h
max.h <- 3 #1 too low and 3 has same kappa

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers2020 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers2020)[4] <- 'Hierid'

lakers2020

####### 2019 Champions: Toronto Raptors ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/TOR/2019.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

raptors2019 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(raptors2019)[4] <- 'Hierid'

raptors2019

####### 2018 Champions: Golden State Warriors ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/GSW/2018.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

warriors2018 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(warriors2018)[4] <- 'Hierid'

warriors2018

####### 2017 Champions: Golden State Warriors ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/GSW/2017.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

warriors2017 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(warriors2017)[4] <- 'Hierid'

warriors2017

####### 2016 Champions: Cleveland Cavaliers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CLE/2016.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

cavaliers2016 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(cavaliers2016)[4] <- 'Hierid'

cavaliers2016

####### 2015 Champions: Golden State Warriors ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/GSW/2015.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

warriors2015 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(warriors2015)[4] <- 'Hierid'

warriors2015

####### 2014 Champions: San Antonio Spurs ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/SAS/2014.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

spurs2014 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(spurs2014)[4] <- 'Hierid'

spurs2014

####### 2013 Champions: Miami Heat ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/MIA/2013.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

heat2013 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(heat2013)[4] <- 'Hierid'

heat2013

####### 2012 Champions: Miami Heat ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/MIA/2012.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

heat2012 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(heat2012)[4] <- 'Hierid'

heat2012

####### 2011 Champions: Dallas Mavericks ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/DAL/2011.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Raptors3 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

mavericks2011 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(mavericks2011)[4] <- 'Hierid'

mavericks2011

########### OLD TIMER CHAMPIONS CATEGORIZED WITH MODELED CATEGORIES ###################
####### 1980 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/1980.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers1980 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers1980)[4] <- 'Hierid'

lakers1980

####### 1981 Champions: Boston Celtics ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/BOS/1981.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Celtics 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Celtics 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

celtics1981 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(celtics1981)[4] <- 'Hierid'

celtics1981

####### 1982 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/1982.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers1982 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers1982)[4] <- 'Hierid'

lakers1982

####### 1983 Champions: Philadelphia 76ers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/PHI/1983.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on 76ers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on 76ers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

s76ers1983 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(s76ers1983)[4] <- 'Hierid'

s76ers1983

####### 1984 Champions: Boston Celtics ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/BOS/1984.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Celtics 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Celtics 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

celtics1984 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(celtics1984)[4] <- 'Hierid'

celtics1984

####### 1985 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/1985.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers1985 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers1985)[4] <- 'Hierid'

lakers1985

####### 1986 Champions: Boston Celtics ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/BOS/1986.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Celtics 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Celtics 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

celtics1986 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(celtics1986)[4] <- 'Hierid'

celtics1986

####### 1987 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/1987.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

celtics1987 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(celtics1987)[4] <- 'Hierid'

celtics1987

####### 1988 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/1988.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers1988 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers1988)[4] <- 'Hierid'

lakers1988

####### 1989 Champions: Detroit Pistons ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/DET/1989.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Pistons 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Pistons 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

pistons1989 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(pistons1989)[4] <- 'Hierid'

pistons1989

####### 1990 Champions: Detroit Pistons ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/DET/1990.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Pistons 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Pistons 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

pistons1990 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(pistons1990)[4] <- 'Hierid'

pistons1990

####### 1991 Champions: Chicago Bulls ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CHI/1991.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Bulls 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Bulls 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

bulls1991 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(bulls1991)[4] <- 'Hierid'

bulls1991

####### 1992 Champions: Chicago Bulls ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CHI/1992.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Bulls 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Bulls 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

bulls1992 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(bulls1992)[4] <- 'Hierid'

bulls1992

####### 1993 Champions: Chicago Bulls ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CHI/1993.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Bulls 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Bulls 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

bulls1993 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(bulls1993)[4] <- 'Hierid'

bulls1993

####### 1994 Champions: Houston Rockets ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/HOU/1994.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Rockets 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Rockets 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

rockets1994 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(rockets1994)[4] <- 'Hierid'

rockets1994

####### 1995 Champions: Houston Rockets ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/HOU/1995.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Rockets 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Rockets 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

rockets1995 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(rockets1995)[4] <- 'Hierid'

rockets1995

####### 1996 Champions: Chicago Bulls ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CHI/1996.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Bulls 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Bulls 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

bulls1996 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(bulls1996)[4] <- 'Hierid'

bulls1996

####### 1997 Champions: Chicago Bulls ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CHI/1997.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Bulls 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Bulls 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

bulls1997 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(bulls1997)[4] <- 'Hierid'

bulls1997

####### 1998 Champions: Chicago Bulls ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/CHI/1998.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Bulls 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Bulls 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

bulls1998 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(bulls1998)[4] <- 'Hierid'

bulls1998

####### 1999 Champions: San Antonio Spurs ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/SAS/1999.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Spurs 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Spurs 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

spurs1999 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(spurs1999)[4] <- 'Hierid'

spurs1999

####### 2000 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/2000.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers2000 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers2000)[4] <- 'Hierid'

lakers2000

####### 2001 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/2001.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers2001 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers2001)[4] <- 'Hierid'

lakers2001

####### 2002 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/2002.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers2002 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers2002)[4] <- 'Hierid'

lakers2002

####### 2003 Champions: San Antonio Spurs ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/SAS/2003.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Spurs 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Spurs 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

spurs2003 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(spurs2003)[4] <- 'Hierid'

spurs2003

####### 2004 Champions: Detroit Pistons ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/DET/2004.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Pistons 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Pistons 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

pistons2004 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(pistons2004)[4] <- 'Hierid'

pistons2004

####### 2005 Champions: San Antonio Spurs ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/SAS/2005.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Spurs 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Spurs 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

spurs2005 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(spurs2005)[4] <- 'Hierid'

spurs2005

####### 2006 Champions: Miami Heat ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/MIA/2006.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Heat 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Heat 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

heat2006 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(heat2006)[4] <- 'Hierid'

heat2006

####### 2007 Champions: San Antonio Spurs ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/SAS/2007.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Spurs 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Spurs 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

spurs2007 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(spurs2007)[4] <- 'Hierid'

spurs2007

####### 2008 Champions: Boston Celtics ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/BOS/2008.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Celtics 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Celtics 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

celtics2008 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(celtics2008)[4] <- 'Hierid'

celtics2008

####### 2009 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/2009.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]
dat4$`3P%`[is.na(dat4$`3P%`)]<-0 

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers2009 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers2009)[4] <- 'Hierid'

lakers2009

####### 2010 Champions: Los Angeles Lakers ###########
newlink <- xml2::read_html("https://www.basketball-reference.com/teams/LAL/2010.html")
dat <- as.data.frame(newlink %>%html_nodes("table")%>%.[[2]] %>%html_table())
dat2 <- subset(dat,dat$G > 0.7 * max(dat$G))
dat3 <- dat2[-c(1,3:8,10,11,13,14,17,18)]
colnames(dat3)[1] <- "Player"
colnames(dat3)[15] <- "PTS"

dat3

dat4 <- dat3[-1]

### Fit kNN model on Lakers 
levels_a <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.mc$`scaled:center`, scale = training.scaled.attr.mc$`scaled:scale`)
  c <- knn(train = training.scaled.mc, test = b,
           cl = training.data.mc[,15], k = max.kmc)
  test <- levels(c)
  levels_a[i] <- test
}

data <- cbind(dat3[1], t(as.data.frame(levels_a)))
colnames(data)[2] <- 'MCid'
data

### Fit kNN model on Lakers 
levels_b <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.km$`scaled:center`, scale = training.scaled.attr.km$`scaled:scale`)
  c <- knn(train = training.scaled.km, test = b,
           cl = training.data.km[,15], k = max.kkm)
  test <- levels(c)
  levels_b[i] <- test
}

data2 <- cbind(data, t(as.data.frame(levels_b)))
colnames(data2)[3] <- 'KMid'

### Fit Hierarchical model 
levels_c <- list()
for (i in c(1:nrow(dat3))){
  a <- dat4[i,]
  b <- scale(a, center = training.scaled.attr.h$`scaled:center`, scale = training.scaled.attr.h$`scaled:scale`)
  c <- knn(train = training.scaled.h, test = b,
           cl = training.data.h[,15], k = max.h)
  test <- levels(c)
  levels_c[i] <- test
}

lakers2010 <- cbind(data2, t(as.data.frame(levels_c)))
colnames(lakers2010)[4] <- 'Hierid'

lakers2010

########### Row binds Champions in two types, past 5 years and 10 years ###############
nbatoday <- nba.clust[,c(1,16,17,18)]

champs80s <- rbind(lakers1980,celtics1981,lakers1982,s76ers1983,celtics1984,lakers1985,celtics1986,celtics1987,lakers1988,pistons1989)
champs90s <- rbind(pistons1990,bulls1991,bulls1992,bulls1993,rockets1994,rockets1995,bulls1996,bulls1997,bulls1998,spurs1999)
champs00s <- rbind(lakers2000,lakers2001,lakers2002,spurs2003,pistons2004,spurs2005,heat2006,spurs2007,celtics2008,lakers2009)
champs10s <- rbind(lakers2010,mavericks2011,heat2012,heat2013,spurs2014,warriors2015,cavaliers2016,warriors2017,warriors2018,raptors2019)
champsfrom90tonow <- rbind(pistons1990,bulls1991,bulls1992,bulls1993,rockets1994,rockets1995,bulls1996,bulls1997,bulls1998,spurs1999,
                           lakers2000,lakers2001,lakers2002,spurs2003,pistons2004,spurs2005,heat2006,spurs2007,celtics2008,lakers2009,
                           lakers2010,mavericks2011,heat2012,heat2013,spurs2014,warriors2015,cavaliers2016,warriors2017,warriors2018,raptors2019)
e1980s <- rbind(lakers1980,celtics1981,lakers1982,s76ers1983,celtics1984)
l1980s <- rbind(lakers1985,celtics1986,celtics1987,lakers1988,pistons1989)
e1990s <- rbind(pistons1990,bulls1991,bulls1992,bulls1993,rockets1994)
l1990s <- rbind(rockets1995,bulls1996,bulls1997,bulls1998,spurs1999)
e2000s <- rbind(lakers2000,lakers2001,lakers2002,spurs2003,pistons2004)
l2000s <- rbind(spurs2005,heat2006,spurs2007,celtics2008,lakers2009)
e2010s <- rbind(lakers2010,mavericks2011,heat2012,heat2013,spurs2014)
l2010s <- rbind(warriors2015,cavaliers2016,warriors2017,warriors2018,raptors2019)

# Lakers 10, Celtics 5, 76ers 1, Pistons 3
# Bulls 6, Rockets 2, Spurs 5, Heat 3, Mavericks 1, Warriors 3, Cavaliers 1, Raptors 1

library(tidyr)
library(ggplot2)

dat.g <- gather(champs80s, type, cluster,-Player)

clust80s <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("1980's Championship Teams Cluster Distributions")

dat.g <- gather(champs90s, type, cluster,-Player)

clust90s <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("1990's Championship Teams Cluster Distributions")

dat.g <- gather(champs00s, type, cluster,-Player)

clust00s <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("2000's Championship Teams Cluster Distributions")

dat.g <- gather(champs10s, type, cluster,-Player)

clust10s <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("2010's Championship Teams Cluster Distributions")

dat.g <- gather(champsfrom90tonow,type,cluster,-Player)

clustfrom90tonow <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("1990 to 2020 Championship Teams Cluster Distributions")

dat.g <- gather(nbatoday, type, cluster,-Player)

today <- ggplot(dat.g[-1], aes(type,fill=cluster)) +
  geom_bar(position = "dodge") +
  ggtitle("NBA Today Cluster Distributions")

clustfrom90tonow
clust80s
clust90s
clust00s
clust10s

today