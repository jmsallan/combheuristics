#' J. E. Beasley instances of the set covering problem
#'
#' @description A list containing 79 instances of the set covering problem included in the OR-Library maintained by J. R. Beasley.
#'
#' @format A list containing 79 lists, named as in the original problem set. Each list has the following contents:
#' \describe{
#' \item{m}{Number of sets of elements of the universe.}
#' \item{n}{Numerber of elements of the universe.}
#' \item{costs}{Cost of including a set to the solution.}
#' \item{instance}{A data frame with all the pairs element - subset. Filtering by subset, we obtain the elements obtained in each subset.}
#' }
#'
#' @source \url{http://people.brunel.ac.uk/~mastjjb/jeb/orlib/scpinfo.html}
#'
#' @examples Beasley_SCP$scp41
"Beasley_SCP"
