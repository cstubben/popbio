#' Matrix definition program for Hudsonia vital rates
#'
#' Creates a projection matrix from \emph{Hudsonia} vital rates (survival,
#' growth, and reproduction). Growth rates are defined as a set of binomial
#' choices as in Table 8.4 B in Morris and Doak (2002).
#'
#' @param vrs Vital rate means in \code{\link{hudvrs}}
#'
#' @return A  projection matrix
#'
#' @references Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
#' biology: Theory and practice of population viability analysis. Sinauer,
#' Sunderland, Massachusetts, USA.
#'
#' @seealso \code{\link{vitalsim}}
#'
#' @author Chris Stubben
#'
#' @examples
#' hudmxdef(hudvrs$mean)
#'
#' @export

hudmxdef <- function(vrs) {
  matrix(c(
    vrs[14], 0, vrs[15], vrs[16], vrs[17], vrs[18],
    vrs[19], 0, vrs[20], vrs[21], vrs[22], vrs[23],
    0, vrs[24], vrs[10] * (1 - vrs[1]), vrs[11] * (1 - vrs[2]), vrs[12] * (1 - vrs[5]), 0,
    0, 0, vrs[10] * vrs[1], vrs[11] * (vrs[2] - vrs[3]), vrs[12] * (vrs[5] - vrs[6]), vrs[13] * (1 - vrs[8]),
    0, 0, 0, vrs[11] * (vrs[3] - vrs[4]), vrs[12] * (vrs[6] - vrs[7]), vrs[13] * (vrs[8] - vrs[9]),
    0, 0, 0, vrs[11] * vrs[4], vrs[12] * vrs[7], vrs[13] * vrs[9]
  ), nrow = 6, byrow = TRUE)
}
