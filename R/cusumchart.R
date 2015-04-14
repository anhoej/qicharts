#' Cusum status charts
#'
#' Cusum status charts are a graphical display of the tabular cusum and may be
#' used to monitor the mean of a process. They show the one-sided upper and
#' lower cusums.
#'
#' @export
#' @param y Numeric vector of measures to plot. Mandatory.
#' @param target Target value of the process mean.
#' @param sdev Standard deviation of the process.
#'
#' @details Lipsum.
#'
#' @return Nothing.
#'
#' @references Douglas C. Montgomery (2009). Introduction to Statistical Process
#' Control, Sixth Edition, John Wiley & Sons.

cusumchart <- function(y,
                       target,
                       sdev,
                       shift     = NULL,
                       k         = 0.5,
                       h         = 5,
                       headstart = FALSE,
                       ylim,
                       main,
                       cex       = 0.8) {
  if(missing(main))
    main <- 'Cusum status chart'

  if(headstart)
    main <- paste(main, 'with headstart')

  if(missing(ylim))
    ylim <- 0

  n_obs <- length(y)

  if (is.null(shift))
    shift <- sdev

  K <- k * shift
  H <- h * shift

  upper_target <- target + K
  lower_target <- target - k

  c_plus <- rep(0, n_obs + 1)
  c_minus <- rep(0, n_obs + 1)
  n_plus <- rep(0, n_obs + 1)
  n_minus <- rep(0, n_obs + 1)

  if (headstart) {
    c_plus[1] <- 0.5 * H
    c_minus[1] <- 0.5 * H
  }

  for (i in 2:(n_obs + 1)) {
    c_plus[i] <- max(0, y[i - 1] - upper_target + c_plus[i -1], na.rm = T)
    if (c_plus[i] > 0) n_plus[i] <- n_plus[i - 1] + 1
    c_minus[i] <- max(0, lower_target - y[i - 1] + c_minus[i -1], na.rm = T)
    if (c_minus[i] > 0) n_minus[i] <- n_minus[i - 1] + 1
  }

  if (!headstart) {
    c_plus <- c_plus[-1]
    c_minus <- c_minus[-1]
  }
  n_plus <-  n_plus[-1]
  n_minus <- n_minus[-1]

  col1            <- rgb(093, 165, 218, maxColorValue = 255)
  col2            <- rgb(223, 092, 036, maxColorValue = 255)
  col3            <- rgb(140, 140, 140, maxColorValue = 255)
  lwd             <- cex
  cex             <- par('cex') * cex  # Text size adjustment

  x_obs <- 1:n_obs

  if (headstart)
    x_obs <- 0:n_obs

  ylim <- range(c(1.1 * c_plus, 1.1 * c_minus, 1.2 * H, -1.2 * H, ylim))

  mar <- par('mar') + c(-0.5, 0, 0, 0)
  op <- par(mar = mar)

  # setup empty plot area
  plot(x    = x_obs,
       y    = c_minus,
       type = 'n',
       xaxt = 'n',
       yaxt = 'n',
       bty  = 'n',
       ylim = ylim,
       xlab = '',
       ylab = '')

  # add x axis and title to plot
  axis(1,
       #      at = x_obs,
       tcl = -0.2,
       lwd = 0,
       lwd.ticks = lwd,
       cex.axis = cex,
       col = col3)
  axis(2,
       tcl = -0.2,
       lwd = 0,
       lwd.ticks = lwd,
       cex.axis = cex,
       col = col3,
       las = 2)
  box(bty = 'l',
      lwd = lwd,
      col = col3)
  title(main = main,
        adj = 0,
        line = 2.7,
        cex.main = cex * 1.25,
        font.main = 1)
  # title(xlab = xlab, ylab = ylab, cex.lab = cex)

  if (headstart) n_obs <- n_obs + 1

  lines(x_obs, rep(0, n_obs), col = col3, lty = 1, lwd = lwd * 1.5)
  lines(x_obs, rep(H, n_obs), col = col3, lty = 1, lwd = lwd)
  lines(x_obs, rep(-H, n_obs), col = col3, lty = 1, lwd = lwd)
  lines(x_obs, -c_minus, col = col1, lty = 1, lwd = lwd * 3, cex = cex, type = 'o',
        pch = 21, bg = 'white')
  lines(x_obs, c_plus, col = col1, lty = 1, lwd = lwd * 3, cex = cex, type = 'o',
        pch = 19)

  par(op)
}
