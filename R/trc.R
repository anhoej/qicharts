#' Trellis run charts for multivariate data
#'
#' Run charts for multivariate data in trellis (grid) layout.
#'
#' @export
#' @import lattice
#' @import latticeExtra
#' @param x Formula object to plot. The formula is of the form y ~ x |
#'    g1 + g2 + ..., indicating that plots of y (on the y-axis) versus x
#'    (on the x-axis) should be produced conditional on the variables g1, g2
#' @param chart Type of chart: 'run' or 'i'
#' @param xscale Scaling of x-axes: 'same' or 'free'
#' @param yscale Scaling of y-axes: 'same' or 'free'
#' @param dec Number of decimals of median value. The default behaviour (smart
#'   rounding to at least two significant digits) should be satisfactory in most
#'   cases.
#' @param pch Plotting character
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
#'
#' # Make I chart
#' trc(y ~ x | g1 + g2, data = d2, main = 'Trellis run chart', chart = 'i')
trc <- function(x,
                chart  = c('run', 'i'),
                xscale = 'same',
                yscale = 'same',
                dec    = NULL,
                xpad   = 0.1,
                pch    = 20,
                ...) {
  col1     <- rgb(093, 165, 218, maxColorValue = 255)
  col2     <- rgb(223, 092, 036, maxColorValue = 255)
  col3     <- rgb(140, 140, 140, maxColorValue = 255)
  axiscol  <- 'grey50'
  stripcol <- 'grey86'
  stripbor <- 'grey80'
  #   stripcol <- 'grey96'
  #   stripbor <- 'grey93'

  chart    <- match.arg(chart)
  strip    <- strip.custom(bg = stripcol)
  par      <- list(axis.line = list(col = 0),
                   strip.border = list(col = stripbor),
                   par.main.text = list(cex = 1))
  scales   <- list(y = list(relation = yscale, col = 1),
                   x = list(relation = xscale, col = 1),
                   col = axiscol,
                   alternating = 1,
                   tck = c(0.5, 0))

  # Scale axes
  prepanel <- function(x, y, ...) {
    qic <- qic(y, chart = chart, plot.chart = FALSE, ...)
    list(xlim = range(min(x), max(extendrange(x, f = xpad))),
         ylim = range(qic$y, qic$lcl, qic$ucl, na.rm = T))
  }

  # Smart rounding for median labels, to at least 2 significant digits
  sround <- function(x) {
    n <- nchar(as.character(floor(x)))
    signif(x, max(2, n))
  }

  # Setup plot
  panel <- function(x, y, ...) {
    qic <- qic(y, chart = chart, plot.chart = FALSE, ...)
    signal <- qic$runs.test

    if(signal) {
      col <- col2
      lty <- 2
    } else {
      col <- col3
      lty <- 1
    }

    rounded_labels <- sapply(qic$cl, sround)
    if (!is.null(dec)) rounded_labels <- round(qic$cl, dec)

    panel.lines(x, qic$cl, col = col, lty = lty, lwd = 1)
    panel.lines(x, qic$ucl, col = col3, lwd = 1)
    panel.lines(x, qic$lcl, col = col3, lwd = 1)
    panel.points(x, y, type = 'o', pch = pch, col = col1, lwd = 2.5, cex = 0.5)
    panel.text(x = max(x), y = qic$cl,
               labels = rounded_labels,
               cex = 0.8,
#                col = axiscol,
               pos = 4)
    panel.xyplot(x, y, ...)
    panel.points(x[qic$signal], y[qic$signal],
                 col = col2,
                 cex = 0.9,
                 pch = pch)
    lims <- current.panel.limits()
    panel.abline(h = lims$ylim[1],
                 v = lims$xlim[1],
                 col = axiscol,
                 lwd = 0.6)
  }

  # Create plot
  p <- xyplot(x,
              type         = 'n',
              scales       = scales,
              prepanel     = prepanel,
              panel        = panel,
              col          = col3,
              strip        = strip,
              par.settings = par,
              between      = list(x = 0.75, y = 0.75),
              ...)

  # Use outer strips with two conditioning variables
  if(length(dim(p)) == 2) {
    p <- useOuterStrips(p,
                        strip = strip,
                        strip.left = strip)
  }

  # Print plot
  return(p)
}
