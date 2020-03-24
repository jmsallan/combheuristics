#' Some TSP instances from the TSP-LIB library
#' 
#' @description A list containing some instances of the travelling salesman problem from the TSP-LIB library. For each instance, the matrix distance is available without need of defining a distance calculation function.
#' 
#'  @format Each instance is wrapped in a list, with parameters depending on information availability:
#'  \describe{
#'  \item{size}{The number of nodes of the TSP instance.}
#'  \item{description}{A brief description of the source of data.}
#'  \item{points}{A data frame with the coordinates of each point. Can be presented in (x,y) or in (Lon, Lat) format, depending on the instance.}
#'  \item{D}{A square matrix containing all distances between nodes. At the moment, all the instances included are symmetric.}
#'  \item{opt.tour}{The optimal tour. If it is not availabe a 'Not Available' message is returned.}
#'  }
#'  
#'  @source \url{http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/index.html}
#'  
#'  @examples TSPLI_sample$gr21
#'  
"TSPLIB_sample"