# Info about scp instances
# http://people.brunel.ac.uk/~mastjjb/jeb/orlib/scpinfo.html

# Sample web
# http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/scp42.txt

#---- list of names -----

#names of 4
names4 <- sapply(1:10, function(x) paste0("scp4", x))
#names of 5
names5 <- sapply(1:9, function(x) paste0("scp5", x))
#names of 6
names6 <- sapply(1:5, function(x) paste0("scp6", x))

#names of A
namesA <- sapply(1:5, function(x) paste0("scpa", x))
#names of B
namesB <- sapply(1:5, function(x) paste0("scpb", x))
#names of C
namesC <- sapply(1:5, function(x) paste0("scpc", x))
#names of D
namesD <- sapply(1:5, function(x) paste0("scpd", x))
#names of E
namesE1 <- sapply(1:5, function(x) paste0("scpe", x))

#names of E2
namesE2 <- sapply(1:5, function(x) paste0("scpnre", x))
#names of F
namesF <- sapply(1:5, function(x) paste0("scpnrf", x))
#names of G
namesG <- sapply(1:5, function(x) paste0("scpnrg", x))
#names of H
namesH <- sapply(1:5, function(x) paste0("scpnrh", x))

list06_11 <- substr(106:111, 2, 3)
list10_13 <- substr(110:113, 2, 3)

#names of CYC
namesCYC <- sapply(list06_11, function(x) paste0("scpcyc", x))
#names of CLR
namesCLR <- sapply(list10_13, function(x) paste0("scpclr", x))

all_names <- c(names4, names5, names6, 
                   namesA, namesB, namesC, namesD, namesE1, 
                   namesE2, namesF, namesG, namesH, 
                   namesCYC, namesCLR)

names(all_names) <- NULL

rm(names4, names5, names6, 
   namesA, namesB, namesC, namesD, namesE1, 
   namesE2, namesF, namesG, namesH, 
   namesCYC, namesCLR, list06_11, list10_13)

root_url <- "http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/"

all_url <- paste0(root_url, all_names, ".txt")

rm(root_url)

read_scp <- function(url){
  
  cat("reading url:", url, "\n")
  
  a <- readLines(url)
  b <- lapply(a, function(x) unlist(strsplit(x, " ")))
  b <- lapply(b, function(x) as.numeric(x[-1]))   #numeric values for each row of the file

  n <- b[[1]][1] #number of elements
  m <- b[[1]][2] #number of subsets
  
  b[[1]] <- NULL #remove first row, we don't need it now
  
  c <- sapply(b, function(x) length(x)) #vector with length of each row of the file
  end <- which(cumsum(c)==m) #last element that contains the cost elements
  
  #suppressing null elements
  
  if(sum(which(c == 0)) != 0){
    b[[which(c==0)]] <- NULL
    c <- c[-which(c==0)]
  }
  
  
  #getting the costs vector
  costs <- b[[1]]
  for(i in 2:end) costs <- c(costs, b[[i]])
  
  #obtaining a list with the number of subsets of each element
  counter <- end+1
  item <- 1
  subsets <- list()
  while(counter < length(c)){
    item_subsets <- b[[counter]][1]
    lines <- which(cumsum(c[(counter+1):length(c)])==item_subsets)
    if(length(lines)>1) print(item)
    subsets[[item]] <- numeric()
    for(i in 1:lines) subsets[[item]] <- c(subsets[[item]], b[[counter+i]])
    counter <- counter + lines + 1
    item <- item+1
    
  }
  length_subsets <- sapply(subsets, length)
  
  #vector with element definition
  element <- numeric()
  for(i in 1:n) element <- c(element, rep(i, length_subsets[i]))
  subset <- numeric()
  for(i in 1:n) subset <- c(subset, subsets[[i]])
  instance <- data.frame(element=element, subset=subset)
  
  return(list(m=m, n=n, costs=costs, instance=instance))
  
}



#---- saving all Beasley instances -----


Beasley_SCP <- lapply(all_url, read_scp)
names(Beasley_SCP) <- all_names
save(Beasley_SCP, file="Beasley_SCP.RData")

lapply(1:length(Beasley_SCP), function(x){
  cat("instance name:", all_names[x], "\n")
  cat("number of sets:", Beasley_SCP[[x]]$m, "\n")
  cat("number of elements:", Beasley_SCP[[x]]$n, "\n")
  return(NULL)
})

save(Beasley_SCP, file = "~/Dropbox (UPC)/quanti_docs/02-RLab/00-packages/combheuristics/data/Beasley_SCP.RData")


