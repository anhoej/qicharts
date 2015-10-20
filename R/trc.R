#' Trellis run charts for multivariate data
#'
#' Run charts for multivariate data in trellis (grid) layout.
#'
#' @export
#' @import lattice
#' @importFrom utils tail
#' @importFrom latticeExtra useOuterStrips
#' @param x Formula object to plot. The formula is of the form y ~ x | g1 + g2 +
#'   ..., indicating that plots of y (on the y-axis) versus x (on the x-axis)
#'   should be produced conditional on the variables g1, g2.
#' @param chart Type of chart: 'run' or 'i'.
#' @param xscale Scaling of x-axes: 'same' or 'free'.
#' @param yscale Scaling of y-axes: 'same' or 'free'.
#' @param dec Number of decimals of median value. The default behaviour (smart
#'   rounding to at least two significant digits) should be satisfactory in most
#'   cases.
#' @param pch Plotting character.
#' @param cex Number indicating the magnification of plotting character.
#' @param gap Number indicating spacing between panels.
#' @param target Value specifying a target line to plot.
#' @param direction Value indication direction of improvement, 0 (down) or 1
#'   (up).
#' @param xpad Number specifying the fraction by which to extend the x-axis in
#'   order to make space for the median label.
#' @param ... Further arguments to xyplot.
#' @details This function is a wrapper for \code{\link{xyplot}} from the
#' \code{\link{lattice}} package. Some usefull arguments from
#' \code{\link{xyplot}} are \code{main}, \code{ylab}, \code{xlab}, and
#' \code{layout}.
#' @return Returns an object of class "trellis".
#' @seealso \code{\link{xyplot}}, \code{\link{qic}}
#' @examples
#' # Trellis run chart on 1 conditioning variable
#' d1 <- data.frame(y = rnorm(96, 12, 3),
#'                  expand.grid(x = 1:24,
#'                              g = LETTERS[1:4]))
#' trc(y ~ x | g, data = d1, main = 'Trellis run chart')
#'
#' # Add target line
#' trc(y ~ x | g, data = d1, main = 'Trellis run chart', target = 20)
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
                cex    = 0.7,
                gap    = 0.5,
                target = NA,
                direction = NULL,
                ...) {
#   col1     <- rgb(093, 165, 218, maxColorValue = 255)
#   col2     <- rgb(223, 092, 036, maxColorValue = 255)
#   col3     <- rgb(140, 140, 140, maxColorValue = 255)
  col1            <- rgb(093, 165, 218, maxColorValue = 255) # blue
  col2            <- rgb(140, 140, 140, maxColorValue = 255) # grey
  col3            <- rgb(005, 151, 072, maxColorValue = 255) # green
  col4            <- rgb(255, 165, 000, maxColorValue = 255) # yellow
  col5            <- rgb(241, 088, 084, maxColorValue = 255) # red

  axiscol  <- 'grey50'
  stripcol <- 'grey90'
  stripbor <- 'grey90'

  chart    <- match.arg(chart)
  strip    <- strip.custom(bg = stripcol)
  # strip    <- strip.custom(bg = col)
  par      <- list(axis.line = list(col = stripcol),
                   strip.border = list(col = stripbor),
                   par.main.text = list(cex = 1.25,
                                        font = 1,
                                        just = 'left',
                                        x = grid::unit(3, 'lines')))
  scales   <- list(y = list(relation = yscale, col = 1),
                   x = list(relation = xscale, col = 1),
                   col = axiscol,
                   alternating = 1,
                   tck = c(0.5, 0))

  # Scale axes
  prepanel <- function(x, y, ...) {
    # qic <- qic(y, chart = chart, plot.chart = FALSE, ...)
    qic <- qic(y, chart = chart, plot = FALSE, ...)
    list(xlim = range(min(x), max(extendrange(x, f = xpad))),
         ylim = range(qic$y, qic$lcl, qic$ucl, target, na.rm = T))
  }

  # Setup plot
  panel <- function(x, y, ...) {
    # qic <- qic(y, chart = chart, plot.chart = FALSE, ...)
    qic <- qic(y, chart = chart, plot = FALSE, ...)
    signal <- qic$runs.test

    if(signal) {
      col <- col4
      lty <- 5
    } else {
      col <- col2
      lty <- 1
    }

    # colour center line according to type of variation and target (red-amber-green)
    lty <- 1
    col <- col2
    m <- ifelse(direction, 1, -1)
    if(signal) {
      lty <- 5
      col <- col4
    } else if(!is.na(target) & !is.null(direction)) {
      col <- ifelse((target - tail(qic$cl, 1)) * m > 0, col5, col3)
    }

    panel.lines(x, qic$cl, col = col, lty = lty, lwd = 1)
    panel.lines(x, qic$ucl, col = col2, lwd = 1)
    panel.lines(x, qic$lcl, col = col2, lwd = 1)
    panel.lines(x, target, col = col2, lty = 3)
    panel.points(x, y, type = 'o', pch = pch, col = col1, lwd = 2.5, cex = cex)
    panel.text(x = max(x), y = qic$cl,
               labels = sround(qic$cl, dec), #rounded_labels,
               cex = 0.8,
               pos = 4)

    panel.text(x = max(x), y = target,
               labels = target,
               cex = 0.8,
               pos = 4)
    panel.xyplot(x, y, ...)
    panel.points(x[qic$signal], y[qic$signal],
                 col = col4,
                 cex = cex * 1.5,
                 pch = pch)
  }

  # Create plot
  p <- xyplot(x,
              type         = 'n',
              scales       = scales,
              prepanel     = prepanel,
              panel        = panel,
              col          = col2,
              strip        = strip,
              par.settings = par,
              between      = list(x = gap, y = gap),
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
