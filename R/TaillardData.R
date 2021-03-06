#' Taillard flow shop sequencing instances
#'
#' @description A list containing the Taillard flow shop sequencing instances.
#'
#' @format A list containing lists with samples with different numbers of jobs and machines:
#' \describe{
#' \item{tai20.5}{10 instances of 20 jobs and 5 machines.}
#' \item{tai20.10}{10 instances of 20 jobs and 10 machines.}
#' \item{tai20.20}{10 instances of 20 jobs and 20 machines.}
#' \item{tai50.5}{10 instances of 50 jobs and 5 machines.}
#' \item{tai50.10}{10 instances of 50 jobs and 10 machines.}
#' \item{tai50.20}{10 instances of 50 jobs and 20 machines.}
#' \item{tai100.5}{10 instances of 100 jobs and 5 machines.}
#' \item{tai100.10}{10 instances of 100 jobs and 10 machines.}
#' \item{tai100.20}{10 instances of 100 jobs and 20 machines.}
#' \item{tai200.10}{10 instances of 200 jobs and 10 machines.}
#' \item{tai200.20}{10 instances of 200 jobs and 10 machines.}
#' \item{tai500.20}{10 instances of 500 jobs and 20 machines.}
#' }
#'
#' @format Each instance contains the following information:
#' \describe{
#' \item{m}{Number of machines.}
#' \item{n}{Number of jobs.}
#' \item{seed}{The seed of the random number generator}
#' \item{upper}{A upper bound of the makespan.}
#' \item{lower}{A lower bound of the makespan.}
#' \item{tij}{Processing time of task j on machine i.}
#' }
#'
#' @examples Taillard_FS$tai.20.5[[1]]
#'
#'
#' @source \url{http://mistic.heig-vd.ch/taillard/problemes.dir/ordonnancement.dir/ordonnancement.html}
#'
#'
"Taillard_FS"
