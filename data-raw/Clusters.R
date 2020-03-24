setwd("~/Dropbox (UPC)/00-curso1920/MH1920q2/FinalProject")

# Source:
# http://cs.joensuu.fi/sipu/datasets/
# 
# Datasets url:
# http://cs.joensuu.fi/sipu/datasets/s1.txt

instances <- list(s1=c(instance="s1", N=5000, k=15),
                  s2=c(instance="s2", N=5000, k=15),
                  s3=c(instance="s3", N=5000, k=15),
                  s4=c(instance="s4", N=5000, k=15),
                  a1=c(instance="a1", N=3000, k=20),
                  a2=c(instance="a2", N=5250, k=35),
                  a3=c(instance="a3", N=7500, k=50))

readFranti_Cluster <- function(instance){
  
  name <- instance[1]
  url <- paste0("http://cs.joensuu.fi/sipu/datasets/", name, ".txt")
  file <- readLines(url)
  file <- sapply(file, function(x) strsplit(x, " "))
  file <- lapply(file, function(x) as.numeric(x[which(x != "")]))
  values <- data.frame(x1=sapply(file, function(x) x[1]), x2=sapply(file, function(x) x[2]), row.names = NULL)
  
  return(list(values=values, clusters=instance[3]))
  
}

FrantiClusters <- lapply(instances, readFranti_Cluster)

names(FrantiClusters) <- sapply(instances, function(x) x[1])

save(FrantiClusters, file="FrantiClusters.RData")


# https://archive.ics.uci.edu/ml/datasets/wine+quality

library(tidyverse)

white <- read_delim("winequality-white.csv", ";")
red <- read_delim("winequality-red.csv", ";")

white <- white %>% select(-quality) %>% mutate_all(scale)
red <- red %>% select(-quality) %>% mutate_all(scale)

names(red) <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", "chlorides", "free_SO2", "total_S02", "density", "pH", "sulphates", "alcohol")
names(white) <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", "chlorides", "free_SO2", "total_S02", "density", "pH", "sulphates", "alcohol")

WineQuality <- list(red=red, white=white)

save(WineQuality, file="WineQuality.RData")
