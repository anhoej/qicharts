#' Trellis run charts for multivariate data
#'
#' Run charts for multivariate data in grid (trellis) layout.
#'
#' @export
#' @import lattice
#' @import latticeExtra
#' @param x Formula object to plot. The formula is of the form y ~ x |
#'    g1 + g2 + ..., indicating that plots of y (on the y-axis) versus x
#'    (on the x-axis) should be produced conditional on the variables g1, g2
#' @param xscale Scaling of x-axes: 'same' or 'free'
#' @param yscale Scaling of y-axes: 'same' or 'free'
#' @param dec Number of decimals of median value
#' @param pch Plotting character
#' @param col1 Colour of curve
#' @param col2 Colour of center line if non-random variation is present
#' @param col3 Colour of center line if only random variation is present
#' @param stripcol Colour of strip background
#' @param xpad Number specifying the fraction by which to extend the x-axis in
#'   order to make space for the median label.
#' @param ... Further arguments to xyplot
#' @details
#' This function is a wrapper for \code{\link{xyplot}} from the
#' \code{\link{lattice}} package. Some usefull arguments from
#' \code{\link{xyplot}} are \code{main}, \code{ylab}, \code{xlab}, and
#' \code{layout}.
#' @return Returns an object of class "trellis".
#' @seealso
#' \code{\link{xyplot}}
#' @examples
#' # Trellis run chart on 1 conditioning variable
#' d1 <- data.frame(y = rnorm(96, 12, 3),
#'                  expand.grid(x = 1:24,
#'                              g = LETTERS[1:4]))
#' trc(y ~ x | g, data = d1, main = 'Trellis run chart')
#'
#' # Trellis run chart on 2 conditioning variables
#' d2 <- data.frame(y = rnorm(144, 12, 3),
#'                  expand.grid(x = seq.Date(as.Date('2014-1-1'),
#'                                           by = 'week',
#'                                           length.out = 24),
#'                              g1 = LETTERS[1:3],
#'                              g2 = letters[1:2]))
#' trc(y ~ x | g1 + g2, data = d2, main = 'Trellis run chart')
#'
#' # Introduce a shift in process performance
#' d2$y[132:144] <- d2$y[132:144] * 3
#' trc(y ~ x | g1 + g2, data = d2, main = 'Trellis run chart')
trc <- function(x,
                xscale   = 'same',
                yscale   = 'same',
                dec      = 2,
                xpad     = 0.1,
                pch      = 19,
                col1     = 'steelblue4',
                col2     = 'tomato',
                col3     = 'palegreen4',
                stripcol = 'grey96',
                ...) {
  #   pch      <- pch
  #   col1     <- col1
  #   col2     <- col2
  #   col3     <- col3
  #   stripcol <- stripcol
  strip    <- strip.custom(bg = stripcol)
  scales   <- list(y = list(relation = yscale,
                            alternating = 1,
                            tck = c(1, 0)),
                   x = list(relation = xscale,
                            alternating = 1,
                            tck = c(1, 0)))
  # Add room for median label
  prepanel <- function(x, y, ...) {
    list(xlim = range(min(x), max(extendrange(x, f = xpad))))
  }

  panel <- function(x, y, ...) {
    qic <- qic(y, plot.chart = FALSE, ...)
    signal <- qic$runs.test

    if(signal) {
      col <- col2
      lty <- 2
    } else {
      col <- col3
      lty <- 1
    }

    panel.lines(x, qic$cl, col = col, lty = lty)
    panel.text(x = max(x),
               y = qic$cl,
               labels = round(qic$cl, dec),
               cex = 0.9,
               pos = 4,
               ...)
    panel.xyplot(x, y, ...)
  }

  p <- xyplot(x,
              type = 'o',
              pch = pch,
              scales = scales,
              prepanel = prepanel,
              panel = panel,
              col = col1,
              strip = strip,
              ...)

  # Use outer strips with two conditioning variables
  if(length(dim(p)) == 2) {
    p <- useOuterStrips(p,
                        strip = strip,
                        strip.left = strip)
  }
  return(p)
}
