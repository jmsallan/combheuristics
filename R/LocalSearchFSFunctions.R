makespan <- function(M, sol){

  m <- dim(M)[1]
  n <- dim(M)[2]

  M <- M[ , sol]

  for(j in 2:n) M[1, j] <- M[1, (j-1)] + M[1, j]
  for(i in 2:m) M[i, 1] <- M[(i-1), 1] + M[i, 1]

  for(i in 2:m)
    for(j in 2:n)
      M[i,j] <- max(M[i-1,j], M[i, j-1]) + M[i, j]


  return(M[m, n])
}

#' Palmer and trapezes heuristics
#'
#' @description
#' Palmer and Trapezes heuristics for the permutative flowshop problem.
#'
#' @param M A matrix object storing the time to process task j (column) in machine i (row).
#'
#' @return pal A vector with the positions of the solution of the Palmer heuristic.
#' @return tra A vector with the positions of the solution of the trapezes heuristic.
#'
#' @examples
#' set.seed(2020)
#' instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
#' PalmerTrapezes(instance)
PalmerTrapezes <- function(M){

  m <- dim(M)[1]
  n <- dim(M)[2]
  S <- matrix(0, 2, n)

  for(j in 1:n)
    for(i in 1:m){
      S[1, j] <- S[1, j] + (m - i)*M[i, j]
      S[2, j] <- S[2, j] + (i - 1)*M[i, j]
    }

  palmer <- order(S[1, ] - S[2, ])
  trapezes <- Johnson(S)

  return(list(pal=palmer, tra=trapezes))
}

Johnson <- function(M){

  m <- dim(M)[1]
  n <- dim(M)[2]

  if(m!=2) return(success=FALSE)

  values <- apply(M, 2, min)

  mask <- M[1, ] == values

  sol <- numeric(n)
  sequence <- order(values)
  head <- 1
  tail <- n


  for(i in 1:n){
    if(mask[sequence[i]]==TRUE){
      sol[head] <- sequence[i]
      head <- head + 1
    }else{
      sol[tail] <- sequence[i]
      tail <- tail - 1
    }
  }

  return(sol)

}

swap <- function(v, i , j){

  aux <- v[i]
  v[i] <- v[j]
  v[j] <- aux
  return(v)
}

insertion <- function(v, i, j){

  n <- length(v)

  if(i!=j){
    aux <- v[i]
    if(i < j)
      v[i:(j-1)] <- v[(i+1):j]
    else
      v[(j+1):i] <- v[j:(i-1)]
    v[j] <- aux
  }
  return(v)
}

#' Hill climbing heuristic for permutative flowshop
#'
#' @description
#' A hill climbing heuristic for pemutative flowshop. In each iteration, it finds the element of the neighbourhood of the present solution with lowest makespan. If the obtained solution improves the existing solution, a new iteration is performed. If not, the algorithm stops.
#'
#' @param M A matrix object storing the time to process task j (column) in machine i (row).
#' @param inisol The starting solution.
#' @param op The neighbourhood operator. Presently are implemented `swap` (the default) and `insertion` neighbourhoods.
#'
#' @return sol The obtained solution.
#' @return obj The makespan of the obtained solution.
#'
#' @examples
#' set.seed(2020)
#' instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
#' HCFS(M=instance, inisol=PalmerTrapezes(instance)$tra, op="insertion")
HCFS <- function(M, inisol, op="swap"){

  if(!op %in% c("swap", "insertion"))
    return(success==FALSE)

  n <- length(inisol)
  sol <- inisol
  obj <- makespan(M, sol)
  k <- TRUE

  while(k){
    #obtaining the solution of minimum value of o.f. from neighbourhood
    soltest <- numeric(n)
    objtest <- Inf

    if(op=="swap"){
      for(i in 1:(n-1))
        for(j in (i+1):n){
          test <- makespan(M, swap(sol, i, j))
          if(test < objtest){
            objtest <- test
            soltest <- swap(sol, i, j)
          }
        }

    }

    if(op=="insertion"){
      for(i in 1:n)
        for(j in 1:n)
          if(i!=j & i!=(j-1)){
            test <- makespan(M, insertion(sol, i, j))
            if(test < objtest){
              objtest <- test
              soltest <- insertion(sol, i, j)
            }
          }
    }

    #comparing the obtained solution with the obtained in previous iteration
    #loop breaks if there is no improvement
    if(objtest < obj){
      obj <- objtest
      sol <- soltest
    }
    else
      k <- FALSE
  }

  return(list(sol=sol, obj=obj))
}

#' Simulated annealing heuristic for permutative flowshop
#'
#' @description An implementation of a simulated annealing heuristic for the flowshop.
#'
#' @param M A matrix object storing the time to process task j (column) in machine i (row).
#' @param inisol The starting solution.
#' @param Tmax the maximum value of temperature, representing the number of iterations of the algorithm.
#' @param mu a parameter tuning the value of probability of selection a solution worse than the presently explored. Higher values of `mu` reduce the probability of selecting a worse solution.
#' @param op The neighbourhood operator. Presently are implemented `swap` (the default) and `insertion` neighbourhoods.
#' @param eval if set to `TRUE` returns information about the evolution of the algorithm.
#'
#' @return sol The obtained solution.
#' @return obj The makespan of the obtained solution.
#' @return evaltest (returned if `eval=TRUE`) the value of the tested solution in each iteration.
#' @return evalfit (returned if `eval=TRUE`)  the value of the makespan of the examined solution in each iteration.
#' @return evalbest (returned if `eval=TRUE`) the value of the best solution obtained in each iteration.
#'
#' @examples
#' set.seed(2020)
#' instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
#' SAFS(M=instance, inisol=PalmerTrapezes(instance)$tra, Tmax=10000, mu=1000, op="insertion")
SAFS <- function(M, inisol, Tmax=1000, mu=1, op="swap", eval=FALSE){

  #checking operator
  if(!op %in% c("swap", "insertion"))
    return(success==FALSE)

  #tracking evaluation
  if(eval){
    evalfit <- numeric(Tmax)
    evalbest <- numeric(Tmax)
    evaltest <- numeric(Tmax)
  }

  #initialization
  sol <- inisol
  bestsol <- inisol
  fit <- makespan(M, sol)
  bestfit <- fit
  T <- Tmax
  n <- length(inisol)

  while (T > 0) {

    move <- sample(1:n, 2)

    if(op=="swap")
      testsol <- swap(sol, move[1], move[2])

    if(op=="insertion")
      testsol <- insertion(sol, move[1], move[2])

    testfit <- makespan(M, testsol)

    if(exp(-mu*(testfit-fit)/T) > runif(1)){
      sol <- testsol
      fit <- testfit
    }

    if(testfit <= bestfit){
      bestsol <- testsol
      bestfit <- testfit
    }

    if(eval){
      evalfit[Tmax - T + 1] <- fit
      evalbest[Tmax - T + 1] <- bestfit
      evaltest[Tmax - T + 1] <- testfit
    }

    T <- T - 1
  }
  if(eval)
    return(list(sol=bestsol, fit=bestfit, evaltest=evaltest, evalfit=evalfit, evalbest=evalbest))
  else
    return(list(sol=bestsol, obj=bestfit))
}


#' Tabu search heuristic for permutative flowshop
#'
#' @description An implementation of a tabu search heuristic for the flowshop.
#' @param M A matrix object storing the time to process task j (column) in machine i (row).
#' @param inisol The starting solution.
#' @param iter the number of iterations to run (see `early` parameter).
#' @param tabu.size the length of the tabu list. The tabu list stores the number of tabu moves obtained in the last `tabu.size` iterations.
#' @param asp if `TRUE` implements an aspiration condition: if a tabu move has better makespan than the best solution found so far, it is allowed to be considered as candidate solution.
#' @param eval if set to `TRUE` returns information about the evolution of the algorithm.
#' @param early if set to `TRUE`, the algorithm stops if it has not improved the solution after `iter` iterations. For `early=TRUE` we can set lower values of `iter` to reduce time of execution.
#'
#' @return sol The obtained solution.
#' @return obj The makespan of the obtained solution.
#' @return evalfit (returned if `eval=TRUE`)  the value of the makespan of the examined solution in each iteration.
#' @return evalbest (returned if `eval=TRUE`) the value of the best solution obtained in each iteration.
#' @examples
#' set.seed(2020)
#' instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
#' TSFS(M=instance, inisol=PalmerTrapezes(instance)$tra, iter=25, op="insertion", early=TRUE)
TSFS <- function(M, inisol, iter=100, tabu.size=5, op="swap", asp=TRUE, eval=FALSE, early=FALSE){

  #tracking evaluation
  if(eval){
    evalfit <- numeric(iter)
    evalbest <- numeric(iter)
  }

  #initialization
  sol <- inisol
  bestsol <- inisol
  bestfit <- makespan(M, sol)

  T <- 1
  Ttabu <- 1
  flag.tabu <- FALSE
  tabu.list <- matrix(numeric(2*tabu.size), tabu.size, 2)

  check_tabu <- function(M, v) any(colSums(apply(M, 1, function(x) v == x))==2)

  n <- length(sol)


  while (T<=iter){

    found_best <- FALSE

    #find the best move
    fit <- Inf
    bestmove <- c(0,0)

    if(op=="swap"){

      #examining all swap moves
      for(i in 1:(n-1))
        for(j in (i+1):n){
          testsol <- swap(sol, i, j)
          testfit <- makespan(M, testsol)

          #improvement with non-tabu move
          if(testfit <= fit & check_tabu(tabu.list, c(i,j))==FALSE){
            fit <- testfit
            bestmove <- c(i, j)
            flag.tabu <- TRUE
          }

          #improvement with tabu move, and aspiration condition
          if(testfit < bestfit & check_tabu(tabu.list, c(i,j))==TRUE & asp==TRUE){
            fit <- testfit
            bestmove <- c(i, j)
            flag.tabu <- FALSE
          }
        }

      #obtain sol
      sol <- swap(sol, bestmove[1], bestmove[2])

    }


    if(op=="insertion"){

      for(i in 1:n)
        for(j in 1:n)
          if(i!=j & i!=(j-1)){

            testsol <- insertion(sol, i, j)
            testfit <- makespan(M, testsol)

            #improvement with non-tabu move
            if(testfit <= fit & check_tabu(tabu.list, c(i,j))==FALSE){
              fit <- testfit
              bestmove <- c(i, j)
              flag.tabu <- TRUE
            }

            #improvement with tabu move, and aspiration condition
            if(testfit < bestfit & check_tabu(tabu.list, c(i,j))==TRUE & asp==TRUE){
              fit <- testfit
              bestmove <- c(i, j)
              flag.tabu <- FALSE
            }
          }

      #obtain sol
      sol <- insertion(sol, bestmove[1], bestmove[2])

    }

    #update bestsol
    if(fit < bestfit){
      bestfit <- fit
      bestsol <- sol
      found_best <- TRUE
    }

    #update tabu list (works the same for both moves)
    if(flag.tabu){
      if(op=="swap")
        tabu.list[Ttabu%%tabu.size+1, ] <- bestmove

      if(op=="insertion")
        tabu.list[Ttabu%%tabu.size+1, ] <- rev(bestmove)

      Ttabu <- Ttabu + 1
    }

    if(eval){
      evalfit[T] <- fit
      evalbest[T] <- bestfit
    }


    if(early & found_best)
      T <- 0
    else
      T <- T + 1
  }

  if(eval)
    return(list(sol=bestsol, fit=bestfit, evalfit=evalfit, evalbest=evalbest))
  else
    return(list(sol=bestsol, obj=bestfit))
}

PalmerGenerator <- function(M, rcl=4){

  m <- dim(M)[1]
  n <- dim(M)[2]
  S <- matrix(0, 2, n)

  for(j in 1:n)
    for(i in 1:m){
      S[1, j] <- S[1, j] + (m - i)*M[i, j]
      S[2, j] <- S[2, j] + (i - 1)*M[i, j]
    }


  diff <- S[1, ] - S[2, ]
  palmer <- order(diff)

  grasp <- numeric(n)

  chosen <- rep(FALSE, n)

  for(i in 1:n){
    k <- sample(1:min(rcl, n-i+1), 1)
    j <- which(chosen==FALSE)[k]
    grasp[i] <- palmer[j]
    chosen[j] <- TRUE
  }

  return(grasp)
}

#' A GRASP algorithm for the permutative flowshop
#'
#' @description A greedy randomized adaptive search procedure (GRASP) for the permutative flowshop. At each iteration, it generates a solution based on the Palmer heuristic. Instead of picking the best element, it picks an element randomly among a restricted candidate list, consisting on the first `rcl` first elements of the Palmer ordering. Then, this solution is improved with a local search heuristic.
#'
#' @param M A matrix object storing the time to process task j (column) in machine i (row).
#' @param rcl the size of the restricted candidate list.
#' @param iter number of iterations.
#' @param op The neighbourhood operator for the local search heuristic. Presently are implemented `swap` (the default) and `insertion` neighbourhoods.
#' @param opt the local search algorithm to implement. Presently are supported `HC` (hill climbing with [HCFS]), `SA` (simulated annealing with [SAFS]) and `TS` (tabu search with [TSFS]).
#'
#' @return sol The obtained solution.
#' @return obj The makespan of the obtained solution.
#'
#' @examples
#' set.seed(2020)
#' instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
#' GRASPFS(M=instance)
GRASPFS <- function(M, rcl=4, iter=100, op="swap", opt="HC", ...){

  params <- list(...)

  if(!opt %in% c("HC", "TS", "SA"))
    return(success==FALSE)

  n <- dim(M)[2]
  bestsol <- numeric(n)
  bestfit <- Inf

  for(t in 1:iter){

    seed_sol <- PalmerGenerator(M, rcl=rcl)

    if(opt=="HC")
      test_sol <- HCFS(M, seed_sol, op=op)

    if(opt=="TS")
      test_sol <- TSFS(M, seed_sol, iter=25, op=op, early = TRUE)

    if(opt=="SA"){
      if(is.null(params$Tmax) & is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, op=op)
      if(!is.null(params$Tmax) & is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, Tmax=params$Tmax, op=op)
      if(is.null(params$Tmax) & !is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, mu=params$mu, op=op)
      if(!is.null(params$Tmax) & !is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, Tmax=params$Tmax, mu=params$mu, op=op)
    }


    if(test_sol$obj < bestfit){
      bestsol <- test_sol$sol
      bestfit <- test_sol$obj
    }

  }

  return(list(sol=bestsol, obj=bestfit))
}

PerturbationInsertion <- function(v, ni=4){
  n <- length(v)
  for(i in 1:ni){
    ch <- sample(1:n, 2)
    v <- insertion(v, ch[1], ch[2])
  }
  return(v)
}

#' Iterated local search for the permutative flowshop
#'
#' @description An iterated local search for the permutative flowshop. At each iteration, the present examined solution obtained at the previous iteration is perturbated performing `ni` insertions. This perturbated solution is improved with a local search heuristic using the swap operator. The starting solution is obtained with the Palmer heuristic.
#'
#' @param M A matrix object storing the time to process task j (column) in machine i (row).
#' @param ni Number of insertions performed when perturbating the solution.
#' @param iter number of iterations.
#' @param opt the local search algorithm to implement. Presently are supported `HC` (hill climbing with [HCFS]), `SA` (simulated annealing with [SAFS]) and `TS` (tabu search with [TSFS]).
#' @param ... we can pass additional parameters for the `SAFS` heuristic if desired (`Tmax` and `mu`).
#'
#' @return sol The obtained solution.
#' @return obj The makespan of the obtained solution.
#'
#' @examples
#' set.seed(2020)
#' instance <- matrix(sample(10:90, 100, replace=TRUE), 5, 20)
#' ILSFS(M=instance, opt="SA", Tmax=10000, mu=1000)
ILSFS <- function(M, ni=4, iter=100, opt="HC", ...){

  params <- list(...)

  if(!opt %in% c("HC", "TS", "SA"))
    return(success==FALSE)

  n <- dim(M)[2]
  bestsol <- numeric(n)
  bestfit <- Inf

  sol <- PalmerTrapezes(M)$pal
  fit <- makespan(M, sol)

  for(i in 1:iter){

    seed_sol <- PerturbationInsertion(sol)

    if(opt=="HC")
      test_sol <- HCFS(M, seed_sol, op="swap")

    if(opt=="TS")
      test_sol <- TSFS(M, seed_sol, iter=25, op="swap", early = TRUE)

    if(opt=="SA"){
      if(is.null(params$Tmax) & is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, op="swap")
      if(!is.null(params$Tmax) & is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, Tmax=params$Tmax, op="swap")
      if(is.null(params$Tmax) & !is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, mu=params$mu, op="swap")
      if(!is.null(params$Tmax) & !is.null(params$mu))
        test_sol <- SAFS(M, seed_sol, Tmax=params$Tmax, mu=params$mu, op="swap")
    }

    if(test_sol$obj < fit){
      sol <- test_sol$sol
      fit <- test_sol$obj
    }

    if(test_sol$obj < bestfit){
      bestsol <- test_sol$sol
      bestfit <- test_sol$obj
    }
  }

  return(list(sol=bestsol, obj=bestfit))

}
