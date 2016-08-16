#' Scatterplot Matrices
#'
#' This function produces a scatterplot matrix for integer data
#'
#' @param dat      a dat frame
#' @param type     type of plot, i.e., 'jitter', 'size', 'count', 'sun', and 'identity'
#' @param barplot  logical: if \code{TRUE} barplots are shown in the diagonals.
#' @param curves   logical: if \code{TRUE} lowess smoothing curves are added in the upper diagonal.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.cor}}, \code{\link{seqtest.cor}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @export
#'
#' @examples
#' dat <- round(sim.cor(200, rho = 0.7))
#'
#' # Scatterplot matrix: jitter
#' scatterplot(dat)
#'
#' # Scatterplot matrix: size
#' scatterplot(dat, type = "size")
#'
#' # Scatterplot matrix: count
#' scatterplot(dat, type = "count")
#'
#' # Scatterplot matrix: sun
#' scatterplot(dat, type = "sun")
scatterplot <- function(dat, type = c("jitter", "size", "count", "sun", "identity"),
                        barplot = TRUE, curves = TRUE) {

  #-----------------------------------------------------------------------------------
  # Check input

  if(!all(apply(dat, 2,  function(x) all(x %% 1 == 0)))) {

    stop("This function is desinged to deal with integer data, use the pairs() function instead")

  }

  #-----------------------------------------------------------------------------------
  # Main function

  dat <- na.omit(dat)

  type <- ifelse(all(c("jitter", "size", "count", "sun") %in% type), "jitter", type)

  if (type == "jitter") {

    pairs(apply(dat, 2, jitter),
          upper.panel = if (curves == TRUE) { panel.smooth } else { NULL },
          diag.panel = if (barplot == TRUE) {
            function(x, ...) {
              par(new = TRUE)
              barplot(table(round(x)), names.arg = "", axes = FALSE, col = "grey95")}
          } else { NULL })

  }

  if (type == "size") {

    pairs(dat, panel = internal.sizeplot,
          upper.panel = if (curves == TRUE) { panel.smooth } else { NULL },
          diag.panel = if (barplot == TRUE) {
            function(x, ...) {
              par(new = TRUE)
              barplot(table(round(x)), names.arg = "", axes = FALSE, col = "grey95")}
          } else { NULL })

  }

  if (type == "count") {

    pairs(dat, panel = internal.count.overplot,
          upper.panel = if (curves == TRUE) { panel.smooth } else { NULL },
          diag.panel = if (barplot == TRUE) {
            function(x, ...) {
              par(new = TRUE)
              barplot(table(round(x)), names.arg = "", axes = FALSE, col = "grey95")}
          } else { NULL })

  }

  if (type == "sun") {

    pairs(dat, panel = internal.sunflowerplot,
          upper.panel = if (curves == TRUE) { panel.smooth } else { NULL },
          diag.panel = if (barplot == TRUE) {
            function(x, ...) {
              par(new = TRUE)
              barplot(table(round(x)), names.arg = "", axes = FALSE, col = "grey95")}
          } else { NULL })

  }

  if (type == "identity") {

    pairs(dat,
          upper.panel = if (curves == TRUE) { panel.smooth } else { NULL },
          diag.panel = if (barplot == TRUE) {
            function(x, ...) {
              par(new = TRUE)
              barplot(table(round(x)), names.arg = "", axes = FALSE, col = "grey95")}
          } else { NULL })

  }

}
