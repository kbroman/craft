#' Font set data
#'
#' Font set data used to construct text from blocks within minecraft
#'
#' @docType data
#'
#' @usage data(font_sets)
#'
#' @details The dataset is a list of fonts with each font being represented as a list with
#' - `charset` - a vector of character strings with the characters present in the font
#' - `png` - a matrix of 0's and 1's grabbed from the PNG file for the font using the package
#'   [imager](https://cran.r-project.org/package=imager).
#'
#' @keywords datasets
#'
#' @source \url{http://uzebox.org/wiki/index.php?title=Font_Bitmaps}
#'
#' @examples
#' data(font_sets)
"font_sets"
