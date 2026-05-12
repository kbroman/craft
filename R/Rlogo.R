globalVariables("Rlogo", "craft")
#' R logo data
#'
#' Matrix corresponding to a version of the R logo
#'
#' @docType data
#'
#' @usage data(Rlogo)
#'
#' @details The dataset is a matrix of pixel colors for a reduced-size version of the R logo,
#' with 1=white, 2=gray, and 3=blue.
#'
#' @keywords datasets
#'
#' @source <https://www.r-project.org/logo/">
#'
#' @examples
#' data(Rlogo)
#' image(Rlogo[,ncol(Rlogo):1], col=c("white", "gray", "blue"),
#'       bty="n", xaxt="n", yaxt="n")
"Rlogo"
