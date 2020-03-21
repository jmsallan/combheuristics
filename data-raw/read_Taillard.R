read_Taillard <- function(case){
  
  url <- paste0("http://mistic.heig-vd.ch/taillard/problemes.dir/ordonnancement.dir/flowshop.dir/tai", case, ".txt")

  text <- readLines(url)
  
  text.split <- strsplit(text, " ")
  clean <- lapply(text.split, function(x) x[which(nchar(x)!=0)])
  
  lines <- length(clean)
  k <- 1
  num.instance <- 1
  instances <- list()
  
  while(k < lines){
    
    refs <- as.numeric(clean[[k+1]])
    n <- refs[1]
    m <- refs[2]
    seed <- refs[3]
    upper <- refs[4]
    lower <- refs[5]
    
    tij <- numeric(0)
    
    for(i in (k+3):(k+2+m)) tij <- c(tij, as.numeric(clean[[i]]))
    
    tij <- matrix(tij, m, n, byrow=TRUE)
    
    instances[[num.instance]] <- list(m=m, n=n, seed=seed, upper=upper, lower=lower, tij=tij)
    
    num.instance <- num.instance+1
    k <- k + m  + 3
  }
  
  return(instances)
}

# a <- read_Taillard("20_5")
