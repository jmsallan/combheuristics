setwd("~/Dropbox (UPC)/00-curso1920/MH1920q2/FinalProject")

# reference
# http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/index.html

# file url
# EUC_2D
# http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/a280.tsp
# 
# LOWER_DIAG_ROW
# http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/dantzig42.tsp
# 
# GEO
# http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/ali535.tsp

#---- the function -----

get_instance_2d <- function(instance){
  
  # loading instance
  url <- paste0("http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/", instance, ".tsp")
  file <- readLines(url)
  l_file <- grep("EOF", file)
  #description
  description <- strsplit(file[grep("^COMMENT", file)], ": ")[[1]][2]
  
  #getting size
  size <- as.numeric(strsplit(file[grep("^DIMENSION", file)], ":")[[1]][2])
  
  #looking for a node coord section
  pos1 <- grep("NODE_COORD_SECTION", file)
  pos2 <- grep("EDGE_WEIGHT_SECTION", file)
  
  if(length(pos1)!=0){ #instance as node coordinates
    
    coord_type <- strsplit(file[grep("EDGE_WEIGHT_TYPE", file)], ": ")[[1]][2]
    coords <- file[(pos1 + 1):(l_file - 1)]
    coords <- sapply(coords, function(x) strsplit(x, " "))
    coords <- lapply(coords, function(x) x[which(x != "")])
    
    if(coord_type=="EUC_2D"){ #Euclidean 2D distance
      x <- sapply(coords, function(x) as.numeric(x[2]))
      y <- sapply(coords, function(x) as.numeric(x[3]))
      points <- data.frame(x=x, y=y, row.names = NULL)
      D <- as.matrix(dist(points))
    }
    
    if(coord_type=="GEO"){ #Geographical distance
      Lat <- sapply(coords, function(x) as.numeric(x[2]))
      Lon <- sapply(coords, function(x) as.numeric(x[3]))
      points <- data.frame(Lon=Lon, Lat=Lat, row.names = NULL)
      library(geosphere)
      D <- matrix(0, size, size)
      for(i in 1:size)
        for(j in 1:size)
          D[i, j] <- distGeo(points[i, ], points[j, ])/1000
    }
  }
  
  if(length(pos2)!=0){ #lower diagonal row
    
    pos3 <- grep("DISPLAY_DATA_SECTION", file)
    if(length(pos3)!=0) l_file <- pos3
    
    distances <- file[(pos2 + 1):(l_file - 1)]
    distances <- sapply(distances, function(x) strsplit(x, " "))
    distances <- sapply(distances, function(x) x[which(x != "")])
    distances <- as.numeric(unlist(distances))
    
    D <- matrix(0, size, size)
    #filling the lower diagonal
    counter <- 1
    for(i in 1:size)
      for(j in 1:i){
        D[i,j] <- distances[counter]
        counter <- counter + 1
      }
    #filling the upper diagonal
    for(i in 1:(size - 1))
      for(j in (i + 1):size)
        D[i, j] <- D[j, i]
  }
  
  
  #loading optimal tour
  library(RCurl)
  url <- paste0("http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/", instance, ".opt.tour")
  if(url.exists(url)==TRUE){
    file <- readLines(url)
    start <- grep("TOUR_SECTION", file)
    end <- which(file=="-1")
    tour <- file[(start + 1):(end - 1)]
    tour <- sapply(tour, function(x) strsplit(x, " "))
    tour <- sapply(tour, function(x) x[which(x != "")])
    tour <- as.numeric(unlist(tour))
  }else
    tour <- "Not available"
  
  
  
  if(length(pos1)!=0)
    return(list(size=size, description=description, points=points, D=D, opt.tour=tour))
  else
    return(list(size=size, description=description, D=D, opt.tour=tour))
  
}

instances_geo <- c("gr96", "gr137", "ali535")
instances_2d <- c("berlin52", "eil51", "eil76", "eil101", "a280")

#---- the instance collection ----

instances_geo <- c("gr96", "gr202", "gr666")
instances_2d <- c("eil76", "eil101", "a280")
instances_ld <- c("gr21", "gr24", "gr48", "fri26", "dantzig42")

all_instances <- c(instances_ld, instances_2d, instances_geo)

TSPLIB_sample <- lapply(all_instances, get_instance_2d)
names(TSPLIB_sample) <- all_instances

save(TSPLIB_sample, file="TSPLIB_sample.RData")
