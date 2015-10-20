#' Trellis Control Charts
#'
#' Run and control charts for multivariate data i trellis (grid) layout.
#'
#' @param n Numeric vector of counts or measures to plot. Mandatory.
#' @param d Numeric vector of sample sizes. Mandatory for P and U charts.
#' @param x Subgrouping vector used for aggregating data and making x-labels.
#'   Mandatory for Xbar and S charts.
#' @param g1 Grouping vector 1 used for trellis layout (facets).
#' @param g2 Grouping vector 2used for trellis layout (facets).
#' @param breaks Numeric vector of break points. Useful for splitting graph in
#'   two or more sections with separate center line and control limits.
#' @param data Data frame containing variables.
#' @param chart Type of control chart. Possible types are: \itemize{ \item
#'   "run": run chart (default). \item "i": individuals chart. \item "mr":
#'   moving range chart. \item "xbar": sample average chart. \item "s": sample
#'   standard deviation chart. \item "t": time between events chart. \item "p":
#'   proportions chart. \item "c": counts chart. \item "u": rates chart. \item
#'   "g": cases between events chart. }
#' @param multiply Integer indicating a number to multiply y axis by, e.g. 100
#'   for percents rather than proportions.
#' @param freeze Number identifying the last data point to include in
#'   calculations of center and limits (ignored if \code{breaks} argument is
#'   given).
#' @param exclude Numeric vector of data points to exclude from calculations of
#'   center and control lines.
#' @param sum.n Logical value indicating whether the mean (default) or sum of
#'   counts should be plotted. Only relevant for run charts and I charts.
#' @param neg.y Logical value. If TRUE (default), the y axis is allowed to be
#'   negative (only relevant for I and Xbar charts).
#' @param cex Number indicating the amount by which text should be magnified.
#' @param pex Number indicating the amount by which plotting symbols should be
#'   magnified.
#' @param ylim Range of y axis.
#' @param date.format Date format of x axis labels. See \code{?strftime} for
#'   possible date formats.
#' @param prime Logical value, If TRUE (default), control limits incorporate
#'   between-subgroup variation as proposed by Laney (2002). Only relevant for P
#'   and U charts.
#' @param flip Logical. If TRUE rotates the plot 90 degrees.
#' @param dots.only Logical value. If TRUE, data points are not connected by
#'   lines and runs analysis is not performed. Useful for comparison and funnel
#'   plots.
#' @param main Character string specifying the title of the plot.
#' @param xlab Character string specifying the x axis label.
#' @param ylab Character string specifying the y axis label.
#' @param plot Logical. If TRUE (default), plot chart.
#' @param print Logical. if TRUE, prints return value.
#' @param ... Further arguments to ggplot function.
#'
#' @details \code{tcc()} is a wrapper function that uses \code{\link{ggplot2}}
#'   to create multivariate run and control charts. It takes up to two grouping
#'   variables to make one or two dimensional trellis plots.
#'
#' @return A list of of class tcc containing values and parameters of the tcc
#'   plot.
#'
#' @export
#'
#' @examples
#' # Run chart of 24 random vaiables
#' tcc(rnorm(24))
#'
#' # Build data frame for examples
#' d <- data.frame(x = rep(1:24, 4),
#'                 mo = (rep(seq(as.Date('2013-1-1'),
#'                               length.out = 24,
#'                               by = 'month'),
#'                           4)),
#'                 n = rbinom(4 * 24, 100, 0.5),
#'                 d = round(runif(4 * 24, 90, 110)),
#'                 g1 = rep(c('a', 'b'), each = 48),
#'                 g2 = rep(c('A', 'B'), each = 24))
#'
#' # Single, one-dimensional run chart
#' tcc(n, d, mo, data = subset(d, g1 == 'a' & g2 == 'A'))
#'
#' # Run chart with one grouping variable and two groups
#' tcc(n, d, mo, g1 = g2, data = subset(d, g1 == 'a'))
#'
#' # Run chart with two grouping variables
#' tcc(n, d, mo, g1 = g1, g2 = g2, data = d)
#'
#' # I chart
#' tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'i')
#'
#' # P chart
#' tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p')
#'
#' # P chart with baseline fixed to the first 12 data points
#' tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p', freeze = 12)
#'
#' # P chart with two breaks
#' tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p', breaks = c(12, 18))
#'
#' # P chart with two data points excluded from calculations
#' tcc(n, d, mo, g1 = g1, g2 = g2, data = d, chart = 'p', exclude = c(12, 18))

tcc <- function(n, d, x, g1, g2, breaks,
                data,
                chart        = c("run", "i", "mr", "xbar", "s",
                                 "t", "p", "c", "u", "g"),
                multiply    = 1,
                freeze      = NULL,
                exclude,
                sum.n       = FALSE,
                neg.y       = TRUE,
                cex         = 1,
                pex         = 1,
                ylim        = NULL,
                date.format = NULL,
                prime       = TRUE,
                flip        = FALSE,
                dots.only   = FALSE,
                main,
                xlab        = 'Time',
                ylab        = 'Indicator',
                plot        = TRUE,
                print       = FALSE,
                ...) {
  # Get chart type
  type <- match.arg(chart)
  fn <- paste0('tcc.', type)

  # Build chart title
  if(missing(main)) {
    main <- paste(toupper(type), "Chart of", deparse(substitute(n)))
    if(!missing(d)) {
      main <- paste(main, '/', deparse(substitute(d)))
    }
    if(multiply != 1) {
      main <- paste(main, 'x', multiply)
    }
  }

  # Get data
  if(!missing(data)){
    class(data) <- 'data.frame'
    n <- data[,deparse(substitute(n))]
    if(deparse(substitute(d)) %in% colnames(data))
      d <- data[,deparse(substitute(d))]
    if(deparse(substitute(x)) %in% colnames(data))
      x <- data[,deparse(substitute(x))]
    if(deparse(substitute(g1)) %in% colnames(data))
      g1 <- data[,deparse(substitute(g1))]
    if(deparse(substitute(g2)) %in% colnames(data))
      g2 <- data[,deparse(substitute(g2))]
  }

  # Set missing denominator
  no.d <- missing(d)
  if(no.d)
    d <- rep(1, length(n))

  # Set missing subgroups
  if(missing(x))
    x <- 1:length(n)
  if(missing(g1))
    g1 <- rep(1, length(n))
  if(missing(g2))
    g2 <- rep(1, length(n))

  # Fix missing values
  cases <- complete.cases(n, d)
  n[!cases] <- NA
  d[!cases] <- NA
  cases <- complete.cases(n, d)

  # Initialise data frame
  df <- data.frame(n, d, x, g1, g2, cases)
  df <- droplevels(df)

  # Build breaks variable
  if(missing(breaks)) {
    df$breaks <- rep(1, nrow(df))
  } else {
    freeze = NULL
    df <- split(df, list(df$g1, df$g2))
    df <- lapply(df, function(x) {
      if(!all(breaks %in% 2:(nrow(x) - 2))) {
        warning('Invalid \"breaks\" argument')
        x$breaks <- rep(1, nrow(x))
      } else {
        breaks <- c(0, breaks, nrow(x))
        breaks <- sort(breaks)
        breaks <- diff(breaks)
        breaks <- rep(1:length(breaks), breaks)
        x$breaks <- breaks}
      return(x)})
    df <- do.call(rbind, df)
    df <- df[order(df$g1, df$g2, df$breaks, df$x), ]
  }

  # Calculate values to plot
  d1 <- aggregate(cbind(n, d) ~ x + g1 + g2 + breaks,
                  data = df,
                  FUN = sum,
                  na.rm = TRUE,
                  na.action = na.pass)
  d2 <- aggregate(cbind(s = n) ~ x + g1 + g2 + breaks,
                  data = df,
                  FUN = sd,
                  na.rm = TRUE,
                  na.action = na.pass)
  d3 <- aggregate(cbind(n.obs = cases) ~ x + g1 + g2 + breaks,
                  data = df,
                  FUN = sum,
                  na.rm = TRUE)
  df <- merge(d1, d2)
  df <- merge(df, d3)
  df <- df[order(df$x), ]

  # Calculate y variable
  if(sum.n & no.d | type %in% c('c', 't', 'g')) {
    df$y <- df$n
  } else {
    df$y <- df$n / df$d
  }
  df$y[is.nan(df$y)] <- NA

  # Build exclude variable
  df$exclude <- FALSE
  if(!missing(exclude)) {
    df <- split(df, list(df$g1, df$g2))
    df <- lapply(df, function(x) {x$exclude[exclude] <- T; return(x)})
    df <- do.call(rbind, df)
  }

  # Complete data frame
  df                  <- split(df, list(df$g1, df$g2, df$breaks))
  df                  <- lapply(df, fn, freeze = freeze, prime = prime, sum.n)
  df                  <- lapply(df, runs.analysis)
  df                  <- do.call(rbind, df)
  row.names(df)       <- NULL
  num.cols            <- c('n', 'd', 's', 'n.obs', 'cl', 'lcl', 'ucl', 'y')
  mult.cols           <- c('cl', 'lcl', 'ucl', 'y')
  df[num.cols]        <- sapply(df[num.cols], as.numeric)
  df[mult.cols]       <- sapply(df[mult.cols], function(x) x * multiply)
  df$limits.signal    <- df$y < df$lcl | df$y > df$ucl
  x                   <- is.na(df$limits.signal)
  df$limits.signal[x] <- FALSE

  # Prevent negative y axis if negy argument is FALSE
  if(!neg.y & min(df$y, na.rm = TRUE) >= 0)
    df$lcl[df$lcl < 0] <- NA

  # Build return value
  tcc <- list(df       = df,
              main     = main,
              xlab     = xlab,
              ylab     = ylab,
              sum.n    = sum.n,
              multiply = multiply,
              freeze   = freeze,
              neg.y    = neg.y,
              prime    = prime)
  class(tcc) <- 'tcc'

  # Plot and return
  if(plot) {
    plot.tcc(tcc, cex = cex, pex = pex, ylim = ylim, date.format = date.format,
             flip = flip, dots.only = dots.only, ...)
  }

  if(print) {
    return(tcc)
  } else {
    invisible(tcc)
  }
}

tcc.run <- function(df, freeze, ...) {
  l <- nrow(df)
  if(is.null(freeze))
    freeze <- l

  base <- 1:freeze
  base <- base[!df$exclude]
  y    <- df$y
  cl   <- median(y[base], na.rm = TRUE)
  cl   <- rep(cl, l)
  lcl  <- NA
  ucl  <- NA
  df   <- cbind(df, cl, ucl, lcl)

  return(df)
}

tcc.i <- function(df, freeze, ...) {
  l <- nrow(df)
  if(is.null(freeze))
    freeze <- l
  base <- 1:freeze
  base <- base[!df$exclude]

  y    <- df$y

  # Calculate centre line
  cl   <- mean(y[base], na.rm = TRUE)
  cl   <- rep(cl, l)

  # Average moving range
  mr  <- abs(diff(y))
  amr <- mean(mr, na.rm = TRUE)

  # Upper limit for moving ranges
  ulmr <- 3.267 * amr

  # Remove moving ranges greater than ulmr and recalculate amr, Provost p.156
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)

  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128

  # Calculate limits
  lcl <- cl - 3 * stdev
  ucl <- cl + 3 * stdev

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.mr <- function(df, freeze, ...) {
  l <- nrow(df)
  if(is.null(freeze))
    freeze <- l
  base <- 1:freeze
  base <- base[!df$exclude]

  y    <- df$y
  y    <- c(NA, abs(diff(y)))
  df$y <- y

  # Calculate centre line
  cl <- mean(y[base], na.rm = TRUE)
  cl <- rep(cl, l)

  # Calculate upper limit for moving ranges
  lcl <- NA
  ucl <- 3.27 * cl

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.t <- function(df, freeze, ...) {
  if(min(df$y, na.rm = TRUE) < 0) {
    stop('Time between events cannot contain negative values')
  }

  if(min(df$y, na.rm = TRUE) == 0) {
    df$y[df$y == 0] <- 0.1
    warning('Time between events should not contain zero values. Zeros replaced by 0.1')
  }

  d   <- df
  d$y <- d$y^(1 / 3.6)

  d <- tcc.i(d, freeze, ...)

  # Back transform centre line and limits
  # y = d$y^3.6
  cl  <- d$cl^3.6
  ucl <- d$ucl^3.6
  lcl <- d$lcl^3.6
  lcl[lcl < 0 | is.nan(lcl)] <- NA

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.xbar <- function(df, freeze, ...){
  l <- nrow(df)
  if(is.null(freeze))
    freeze <- l
  base <- 1:freeze
  base <- base[!df$exclude]

  y <- df$y
  n <- df$n.obs
  s <- df$s

  # Calculate centre line, Montgomery 6.30
  cl <- sum(n[base] * y[base], na.rm = TRUE) / sum(n[base],
                                                   na.rm = TRUE)
  cl <- rep(cl, l)

  # Calculate standard deviation and control limits, Montgomery 6.31
  stdev <- sqrt(sum(s[base]^2 * (n[base] - 1), na.rm = TRUE) /
                  sum(n[base] - 1, na.rm = TRUE))
  A3    <- a3(n)
  ucl   <- cl + A3 * stdev
  lcl   <- cl - A3 * stdev

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.s <- function(df, freeze, ...){
  l <- nrow(df)
  if(is.null(freeze))
    freeze <- l

  base <- 1:freeze
  base <- base[!df$exclude]
  s    <- df$s
  df$y <- s
  n    <- df$n.obs

  # Calculate centre line, Montgomery 6.31
  sbar <- sqrt(sum(s[base]^2 * (n[base] - 1), na.rm = TRUE) /
                 (sum(n[base], na.rm = TRUE) - l))
  cl   <- rep(sbar, l)
  B3   <- b3(n)
  B4   <- b4(n)
  ucl  <- B4 * sbar
  lcl  <- B3 * sbar

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.p <- function(df, freeze, prime, ...) {
  l <- nrow(df)
  if(is.null(freeze))
    freeze <- l

  base  <- 1:freeze
  base  <- base[!df$exclude]
  n     <- df$n
  d     <- df$d
  y     <- df$y
  cl    <- sum(n[base], na.rm = TRUE) / sum(d[base], na.rm = TRUE)
  cl    <- rep(cl, l)
  stdev <- sqrt(cl * (1 - cl) / d)

  # Calculate standard deviation for Laney's p-prime chart, incorporating
  # between-subgroup variation.
  if(prime) {
    z_i     <- (y[base] - cl[base]) / stdev[base]
    sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
    stdev   <- stdev * sigma_z
  }

  ucl          <- cl + 3 * stdev
  lcl          <- cl - 3 * stdev
  ucl[ucl > 1] <- NA
  lcl[lcl < 0] <- NA

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.c <- function(df, freeze, ...){
  l        <- nrow(df)
  if(is.null(freeze))
    freeze <- l
  base <- 1:freeze
  base <- base[!df$exclude]

  y  <- df$y
  cl <- mean(y[base], na.rm = TRUE)
  cl <- rep(cl, l)

  # Calculate standard deviation, Montgomery 7.17
  stdev <- sqrt(cl)

  # Calculate limits
  ucl          <- cl + 3 * stdev
  lcl          <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.u <- function(df, freeze, prime, ...){
  l        <- nrow(df)
  if(is.null(freeze))
    freeze <- l

  base <- 1:freeze
  base <- base[!df$exclude]
  n    <- df$n
  d    <- df$d
  y    <- n / d
  cl   <- sum(n[base], na.rm = TRUE) / sum(d[base], na.rm = TRUE)
  cl   <- rep(cl, l)

  # Calculate standard deviation, Montgomery 7.19
  stdev <- sqrt(cl / d)

  # Calculate standard deviation for Laney's p-prime chart, incorporating
  # between-subgroup variation.
  if(prime) {
    z_i     <- (y[base] - cl[base]) / stdev[base]
    sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
    stdev   <- stdev * sigma_z
  }

  # Calculate limits
  ucl          <- cl + 3 * stdev
  lcl          <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

tcc.g <- function(df, freeze, ...){
  l        <- nrow(df)
  if(is.null(freeze))
    freeze <- l

  base <- 1:freeze
  base <- base[!df$exclude]
  y    <- df$y

  # Calculate centre line
  cl <- mean(y[base], na.rm = TRUE)
  cl <- rep(cl, l)

  # Calculate standard deviation, Montgomery, p. 319
  stdev <- sqrt(cl * (cl + 1))

  # Calculate limits
  ucl          <- cl + 3 * stdev
  lcl          <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  # Set centre line to theoretical median, Provost (2011) p. 228
  cl <- 0.693 * cl

  # Build and return data frame
  df <- cbind(df, cl, ucl, lcl)
  return(df)
}

runs.analysis <- function(df) {
  y               <- df$y
  cl              <- df$cl
  runs            <- sign(y - cl)
  runs            <- runs[runs != 0 & !is.na(runs)]
  n.useful        <- length(runs)
  run.lengths     <- rle(runs)$lengths
  n.runs          <- length(run.lengths)
  longest.run     <- max(run.lengths)
  longest.run.max <- round(log2(n.useful)) + 3                # Schilling 2012
  n.crossings     <- max(n.runs - 1, 0)
  n.crossings.min <- qbinom(0.05, max(n.useful - 1, 0), 0.5)  # Chen 2010 (7)
  runs.signal     <- longest.run > longest.run.max ||
    n.crossings < n.crossings.min
  runs.signal     <- rep(runs.signal, length(y))
  df              <- cbind(df, runs.signal)

  return(df)
}

a3 <- function(n) {
  n[n == 0]    <- NA
  tbl          <- c(NA,
                    2.659, 1.954, 1.628, 1.427, 1.287, 1.182,
                    1.099, 1.032, 0.975, 0.927, 0.886, 0.850,
                    0.817, 0.789, 0.763, 0.739, 0.718, 0.698,
                    0.680, 0.663, 0.647, 0.633, 0.619, 0.606)
  x            <- 3 / (4 * (n - 1)) * (4 * n - 3) / sqrt(n)
  w            <- which(n <= 25)
  x[w]         <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

b3 <- function(n) {
  n[n == 0]    <- NA
  tbl          <- c(NA,
                    0.000, 0.000, 0.000, 0.000, 0.030, 0.118,
                    0.185, 0.239, 0.284, 0.321, 0.354, 0.382,
                    0.406, 0.428, 0.448, 0.466, 0.482, 0.497,
                    0.510, 0.523, 0.534, 0.545, 0.555, 0.565)
  x            <- 1 - (3 / c4(n) / sqrt(2 * (n - 1)))
  w            <- which(n <= 25)
  x[w]         <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

b4 <- function(n) {
  tbl          <- c(NA,
                    3.267, 2.568, 2.266, 2.089, 1.970, 1.882,
                    1.815, 1.761, 1.716, 1.679, 1.646, 1.618,
                    1.594, 1.572, 1.552, 1.534, 1.518, 1.503,
                    1.490, 1.477, 1.466, 1.455, 1.445, 1.435)
  x            <- 1 + (3 / c4(n) / sqrt(2 * (n - 1)))
  w            <- which(n <= 25)
  x[w]         <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

c4 <- function(n) {
  n[n == 0]   <- NA
  tbl         <- c(NA,
                   0.7979, 0.8862, 0.9213, 0.9400, 0.9515, 0.9594,
                   0.9650, 0.9693, 0.9727, 0.9754, 0.9776, 0.9794,
                   0.9810, 0.9823, 0.9835, 0.9845, 0.9854, 0.9862,
                   0.9869, 0.9876, 0.9882, 0.9887, 0.9892, 0.9896)

  x            <- 4 * (n - 1) / (4 * n - 3)
  w            <- which(n <= 25)
  x[w]         <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

#' Plot trellis control chart
#'
#' Creates a plot of a tcc object
#'
#' @param x List object returned from the tcc() function.
#' @param y Ignored. Included for compatibility with generic plot function.
#' @param cex Number indicating the amount by which text should be magnified.
#' @param pex Number indicating the amount by which plotting symbols should be
#'   magnified.
#' @param ylim Range of y axis.
#' @param date.format Date format of x axis labels. See \code{?strftime} for
#'   date formats.
#' @param flip Logical. If TRUE rotates the plot 90 degrees.
#' @param dots.only Logical value. If TRUE, data points are not connected by
#'   lines and runs analysis is not performed. Useful for comparison and funnel
#'   plots.
#' @param ... Further arguments to plot function.
#'
#' @return Creates a tcc plot.
#'
#' @export
#'
#' @import ggplot2
#' @import scales
#'
#' @examples
#' p <- tcc(rnorm(24))
#' plot(p)
#'
plot.tcc <- function(x,
                     y           = NULL,
                     cex         = 1,
                     pex         = 1,
                     ylim        = NULL,
                     date.format = '%Y-%m-%d',
                     flip        = FALSE,
                     dots.only   = FALSE,
                     ...) {
  #   require(scales)
  #   require(ggplot2)

  df      <- x$df
  main    <- x$main
  ylab    <- x$ylab
  xlab    <- x$xlab
  freeze  <- x$freeze
  col1    <- rgb(093, 165, 218, maxColorValue = 255) # blue
  col2    <- rgb(255, 165, 000, maxColorValue = 255) # amber
  col3    <- rgb(140, 140, 140, maxColorValue = 255) # grey
  col4    <- 'white'
  cols    <- c('col1' = col1,
               'col2' = col2,
               'col3' = col3,
               'col4' = col4)
  df$pcol <- ifelse(df$limits.signal, 'col2', 'col1')
  df$pcol <- ifelse(df$exclude, 'col4', df$pcol)
  df$lcol <- ifelse(df$runs.signal, 'col2', 'col3')

  p <- ggplot(df) +
    theme_bw(base_size = 12 * cex) +
    theme(panel.border     = element_rect(colour = 'grey70', size = 0.1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line        = element_blank(),
          axis.ticks       = element_line(colour = col3),
          axis.title.y     = element_text(vjust = 1.2),
          axis.title.x     = element_text(vjust = -0.2),
          plot.title       = element_text(vjust = 1.2, hjust = 0),
          strip.background = element_rect(fill = 'grey90', colour = 'grey70'))

  p <- p +
    geom_line(aes_string(x = 'x', y = 'cl', colour = 'lcol', group = 'breaks'),
              lwd = 0.3 * cex,
              na.rm = TRUE) +
    geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'),
              colour = col3,
              lwd = 0.3 * cex,
              na.rm = TRUE) +
    geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'),
              colour = col3,
              lwd = 0.3 * cex,
              na.rm = TRUE)

  if(!dots.only) {
    p <- p + geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'),
                       colour = col1,
                       lwd = 1.1 * cex,
                       na.rm = TRUE)
  }

  p <- p + geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'),
                      colour = col1,
                      size = 2.5 * pex * cex,
                      shape = 21,
                      na.rm = TRUE) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    guides(colour = FALSE, fill = FALSE)

  ng1 <- nlevels(droplevels(as.factor(df$g1))) > 1
  ng2 <- nlevels(droplevels(as.factor(df$g2))) > 1

  if(ng1 & ng2) {
    p <- p + facet_grid(g1 ~ g2, ...)
  } else if(ng1) {
    p <- p + facet_wrap(~ g1, ...)
  } else if(ng2) {
    p <- p + facet_wrap(~ g2, ...)
  } else {
    p <- p + theme(panel.border = element_blank(),
                   axis.line = element_line(size = 0.1, colour = col3))
  }

  if(!is.null(freeze)) {
    f <- as.numeric(df$x[freeze])
    f <- f + as.numeric(df$x[freeze + 1] - df$x[freeze]) / 2
    p <- p + geom_vline(xintercept = f, size = 0.5, lty = 3, colour = col3)
  }

  if(inherits(df$x, c('Date')) & !is.null(date.format)) {
    p <- p + scale_x_date(labels = date_format(date.format))
  }

  if(inherits(df$x, c('POSIXct')) & !is.null(date.format)) {
    p <- p + scale_x_datetime(labels = date_format(date.format))
  }

  p <- p +
    labs(title = main, x = xlab, y = ylab) +
    coord_cartesian(ylim = ylim)

  if(flip) {
    p <- p + coord_flip()

  }

  #   p <- p +
  #     geom_text(aes(x = max(x), y = cl, label = round(cl, 2)),
  #               # hjust = 1,
  #               vjust = 0,
  #               size = 3)

  plot(p)
}
