setwd("~/Dropbox (UPC)/00-curso1920/MH1920q2/FinalProject")

# Info about scp instances
# http://people.brunel.ac.uk/~mastjjb/jeb/orlib/scpinfo.html

# Sample web
# http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/scp42.txt


#---- list URLs ------

root_url <- "http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/"

list10 <- as.character(1:10)
list5 <- as.character(1:5)

#set 4
urlset4 <- sapply(list10, function(x) paste0(root_url, "scp4", x, ".txt"))
#set 5
urlset5 <- sapply(list10, function(x) paste0(root_url, "scp5", x, ".txt"))
#set 6
urlset6 <- sapply(list5, function(x) paste0(root_url, "scp6", x, ".txt"))

#set A
urlsetA <- sapply(list5, function(x) paste0(root_url, "scpa", x, ".txt"))
#set B
urlsetB <- sapply(list5, function(x) paste0(root_url, "scpb", x, ".txt"))
#set C
urlsetC <- sapply(list5, function(x) paste0(root_url, "scpc", x, ".txt"))
#set D
urlsetD <- sapply(list5, function(x) paste0(root_url, "scpd", x, ".txt"))
#set E
urlsetE1 <- sapply(list5, function(x) paste0(root_url, "scpe", x, ".txt"))

#set E2
urlsetE2 <- sapply(list5, function(x) paste0(root_url, "scpnre", x, ".txt"))
#set F
urlsetF <- sapply(list5, function(x) paste0(root_url, "scpnrf", x, ".txt"))
#set G
urlsetG <- sapply(list5, function(x) paste0(root_url, "scpnrg", x, ".txt"))
#set H
urlsetH <- sapply(list5, function(x) paste0(root_url, "scpnrh", x, ".txt"))

list06_11 <- substr(106:111, 2, 3)
list10_13 <- substr(110:113, 2, 3)

#set CYC
urlsetCYC <- sapply(list06_11, function(x) paste0(root_url, "scpcyc", x, ".txt"))
#set CLR
urlsetCRL <- sapply(list10_13, function(x) paste0(root_url, "scpclr", x, ".txt"))

#---- list of names -----

#names of 4
names4 <- sapply(list10, function(x) paste0("scp4", x))
#names of 5
names5 <- sapply(list10, function(x) paste0("scp5", x))
#names of 6
names6 <- sapply(list5, function(x) paste0("scp6", x))

#names of A
namesA <- sapply(list5, function(x) paste0("scpa", x))
#names of B
namesB <- sapply(list5, function(x) paste0("scpb", x))
#names of C
namesC <- sapply(list5, function(x) paste0("scpc", x))
#names of D
namesD <- sapply(list5, function(x) paste0("scpd", x))
#names of E
namesE1 <- sapply(list5, function(x) paste0("scpe", x))

#names of E2
namesE2 <- sapply(list5, function(x) paste0("scpnre", x))
#names of F
namesF <- sapply(list5, function(x) paste0("scpnrf", x))
#names of G
namesG <- sapply(list5, function(x) paste0("scpnrg", x))
#names of H
namesH <- sapply(list5, function(x) paste0("scpnrh", x))

#names of CYC
namesCYC <- sapply(list06_11, function(x) paste0("scpcyc", x))
#names of CLR
namesCLR <- sapply(list10_13, function(x) paste0("scpclr", x))

read_SCP <- function(url){
  
  url <- "http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/scpclr11.txt"
  
  a <- readLines(url)
  b <- lapply(a, function(x) unlist(strsplit(x, " ")))
  b <- lapply(b, function(x) as.numeric(x[-1]))   #numeric values for each row of the file

  n <- b[[1]][1] #number of elements
  m <- b[[1]][2] #number of subsets
  
  b[[1]] <- NULL #remove first row, we don't need it now
  
  c <- sapply(b, function(x) length(x)) #vector with length of each row of the file
  end <- which(cumsum(c)==m) #last element that contains the cost elements
  
  #suppressing null elements
  b[[which(c==0)]] <- NULL
  c <- c[-which(c==0)]
  
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

#---- saving 50 of the 80 Beasley instances -----

some_url <- c(urlset4, urlset5, urlset6, urlsetA, urlsetB, urlsetC, urlsetD, urlsetE1)
names_some_url <- c(names4, names5, names6, namesA, namesB, namesC, namesD, namesE1)

Beasley_SCP <- lapply(some_url, read_SCP)
names(Beasley_SCP) <- names_some_url
save(Beasley_SCP, file="Beasley_SCP.RData")

#---- saving all Beasley instances -----

all_url <- c(urlset4, urlset5, urlset6, urlsetA, urlsetB, urlsetC, urlsetD, urlsetE1, urlsetE2, urlsetF, urlsetG, urlsetH, urlsetCYC, urlsetCRL)
names_all_url <- c(names4, names5, names6, namesA, namesB, namesC, namesD, namesE1, namesE2, namesF, namesG, namesH, namesCYC, namesCLR)


Beasley_SCP <- lapply(all_url, read_SCP)
names(Beasley_SCP) <- names_all_url
save(Beasley_SCP, file="Beasley_SCP.RData")


z <- lapply(urlsetCRL, read_SCP)
