#' @title Pareto chart
#' @description Creates a pareto chart from a categorical variable
#' @author Jacob Anhoej
#' @export
#' @param x Categorical vector to be plotted
#' @param main Plot title
#' @param ylab Label on first (left) y axis
#' @param xlab Label on x axis
#' @param cumperc.by Grid interval
#' @param cex Number indicating the amount by which text and symbols should be magnified.
#' @param ... Further arguments to plot function
#' @return A table of frequencies and percentages from the pareto analysis
#' @examples
#' x <- rep(LETTERS[1:9], c(200, 100, 50, 25, 12, 6, 3, 2, 1))
#' paretochart(x)

paretochart <- function (x,
                         main,
                         ylab       = 'Frequency',
                         xlab       = '',
                         cumperc.by = 20,
                         cex = 0.9,
                         ...) {
  varname  <- deparse(substitute(x))
  x        <- factor(x)
  w        <- max(strwidth(levels(x), 'inch') + 0.4, na.rm = TRUE)
  x        <- table(x)
  x        <- sort(x, decreasing = TRUE, na.last = TRUE)
  cumsum.x <- cumsum(x)
  cumperc  <- seq(0, 100, by = cumperc.by)
  q        <- quantile(seq(0, max(cumsum.x, na.rm = TRUE)), cumperc/100)
  ylim     <- range(0, cumsum.x)
  mai.add  <- c(w - par('mai')[1], 0, -0.4, 0)
  cex      <- par('cex') * cex

  if (missing(main)) {
    main <- paste('Pareto Chart for', varname)
  }

  if(xlab == '') {
    oma <- c(0.5, 0.3, 0, 0.4)
  } else {
    oma <- c(1.5, 0.3, 0, 0.4)
  }

  opar <- par(mai = par('mai') + mai.add, oma = oma)

  pc <- barplot(x,
                space    = 0.1,
                ylim     = ylim,
                ylab     = ylab,
                col      = 'steelblue4',
                las      = 3,
                cex      = cex,
                cex.axis = cex,
                cex.lab  = cex,
                ...)
  box()
  title(main = main, adj = 0, line = 1, cex.main = cex)
  abline(h = q, col = 'lightgrey', lty = 3)
  lines(pc, cumsum.x, type = 'b', pch = 20)
  axis(4, at = q, labels = paste0(cumperc, '%'), cex.axis = cex, ...)
  mtext(xlab, 1, line = 0, cex = cex, outer = TRUE)
  par(opar)

  tab <- cbind(x, cumsum.x, x/max(cumsum.x, na.rm = TRUE) * 100,
               cumsum.x/max(cumsum.x, na.rm = TRUE) * 100)
  colnames(tab) <- c('Frequency', 'Cumulative Frequency',
                     'Percentage', 'Cumulative Percentage')
  names(dimnames(tab)) <- c('', paste('Pareto analysis for', varname))
  return(as.data.frame(tab))
}
