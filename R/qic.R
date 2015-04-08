#' Quality improvement charts
#'
#' Run and control charts for quality improvement and control
#'
#' @export
#' @param y Numeric vector of counts or measures to plot. Mandatory.
#' @param n Numeric vector of sample sizes. Mandatory for P and U charts.
#' @param x Subgrouping vector used for aggregating data and making x-labels.
#'   Mandatory for Xbar and S charts.
#' @param data Data frame containing variables.
#' @param chart Type of control chart. Possible types are: \itemize{ \item
#'   "run": run chart (default). \item "i": individuals chart. \item "mr":
#'   moving range chart. \item "xbar": sample average chart. \item "s": sample
#'   standard deviation chart. \item "t": time between events chart. \item "p":
#'   proportions chart. \item "c": counts chart. \item "u": rates chart. \item
#'   "g": cases between events chart. }
#' @param notes Character vector of notes to be added to individual. data
#'   points.
#' @param cl Value specifying the center line (if known).
#' @param ylim Range of y axis limits.
#' @param target Value specifying a target line to plot.
#' @param freeze Number identifying the last data point to include in
#'   calculations of center and limits (ignored if \code{breaks} argument is
#'   given).
#' @param breaks Numeric vector of break points.
#' @param exclude Numeric vector of data points to exclude from calculations of
#'   center and control lines.
#' @param negy Logical value, if TRUE, the y axis is allowed to be negative
#'   (only relevant for i and xbar charts).
#' @param dots.only Logical value, if TRUE, data points are not connected by
#'   lines and runs analysis is not performed. Useful for comparison and funnel
#'   plots.
#' @param decimals Integer indicating the number of decimals shown for center
#'   and limits on the plot.
#' @param multiply Integer indicating a number to multiply y axis by, e.g. 100
#'   for percents rather than proportions.
#' @param x.format Date format of x axis labels.
#' @param cex Number indicating the amount by which text and symbols should be
#'   magnified.
#' @param main Character string specifying the title of the plot.
#' @param xlab Character string specifying the x axis label.
#' @param ylab Character string specifying the y axis label.
#' @param pre.text Character string labelling pre-freeze period
#' @param post.text Character string labelling post-freeze period
#' @param runvals Logical value, if TRUE, prints statistics from runs analysis
#'   on plot.
#' @param linevals Logical value, if TRUE, prints values for center and control
#'   lines on plot.
#' @param plot.chart Logical value, if TRUE, prints plot.
#' @param prnt Logical value, if TRUE, prints return value
#' @param primed Logical value, if TRUE, control limits incorporate
#'   between-subgroup variation as proposed by Laney (2002). This is recommended
#'   for data involving very large sample sizes \code{n}. Only relevant for P
#'   and U charts.
#' @param standardised Logical value, if TRUE, creates a standardised control
#'   chart, where points are plotted in standard deviation units along with a
#'   center line at zero and control limits at 3 and -3. Only relevant for P
#'   and U charts.
#' @param ... Further arguments to plot function.
#'
#' @details If \code{chart} is not specified, \code{qic} plots a \strong{run
#'   chart}. Non-random variation will be marked by a dashed, red center line
#'   (the median) if either the longest run of data points above or below the
#'   median is longer than predicted or if the graph crosses the median fewer
#'   times than predicted (see references for details).
#'
#'   Only the \code{y} argument giving the count or measure of interest is
#'   mandatory for a run chart. If a denominator argument, \code{n}, is given,
#'   \eqn{y/n} will be plotted. If a subgrouping argument, \code{x}, is given,
#'   \eqn{sum(y)/sum(n)}, within each subgroup will be plotted.
#'
#'   With \strong{controlcharts}, data aggregation by subgroups is handled
#'   according to chart type. For P, U, and I charts, data are aggregated as
#'   described for the run chart. For the C chart, the sum of counts,
#'   \code{sum(y)}, within each subgroups will be plotted.
#'
#'   For Xbar and S charts, the subgrouping argument, \code{x}, is mandatory.
#'   However, the sample size argument, \code{n}, is irrelevant and will be
#'   ignored.
#'
#'   The subgrouping argument, \code{x}, is irrelevant for T and G charts, and,
#'   if given, an error will occur if any subgroup has more than one element.
#'
#'   If more than one \code{note} is present within any subgroup, the first
#'   \code{note} (alphabetically) is chosen.
#'
#'   If both \code{primed} and \code{standardised} are \code{TRUE}, points are
#'   plotted in units corresponding to Laney's modified "standard deviation",
#'   which incorporates the variation between subgroups.
#'
#' @return A list of values and parameters of the qic plot.
#'
#' @references Runs analysis: \itemize{ \item Mark F. Schilling (2012). The
#'   Surprising Predictability of Long Runs. Math. Mag. 85, 141-149. \item
#'   Zhenmin Chen (2010). A note on the runs test. Model Assisted Statistics and
#'   Applications 5, 73-77. } Calculation of control limits: \itemize{ \item
#'   Douglas C. Montgomery (2009). Introduction to Statistical Process Control,
#'   Sixth Edition, John Wiley & Sons. \item James C. Benneyan (2001).
#'   Number-Between g-Type Statistical Quality Control Charts for Monitoring
#'   Adverse Events. Health Care Management Science 4, 305-318. \item Lloyd P.
#'   Provost, Sandra K. Murray (2011). The Health Care Data Guide: Learning from
#'   Data for Improvement. San Fransisco: John Wiley & Sons Inc. \item David B.
#'   Laney (2002). Improved control charts for attributes. Quality Engineering,
#'   14(4), 531-537.}
#'
#' @examples
#' set.seed(1)
#' # Run chart of 24 samples of a random continuous variable
#' # with an approximate mean = 12 and standard deviation = 3.
#' y <- rnorm(24, 12, 3)
#' qic(y)
#'
#' # Add subgroup vector (dates) and a target
#' x <- seq.Date(as.Date('2013-08-04'), by = 'week', length = 24)
#' qic(y, x = x, target = 16)
#'
#' # Individuals control chart
#' qic(y, x = x, chart = 'i')
#'
#' # Xbar control chart, sample size = 5
#' y <- rnorm(5 * 24)
#' x <- rep(x, 5)
#' qic(y, x = x, chart = 'xbar')
#'
#' # Create data frame with counts and sample sizes by week
#' d <- data.frame(week = seq.Date(as.Date('2013-08-04'),
#'                                 by = 'week',
#'                                 length = 36),
#'                 y = c(rbinom(24, 20, 0.5), rbinom(12, 20, 0.8)),
#'                 n = round(rnorm(36, 20, 2)))
#'
#' # Proportions control chart
#' qic(y, n, x = week, data = d[1:24,], chart = 'p')
#'
#' # Introduce change in process performance
#' qic(y, n, x = week, data = d, chart = 'p')
#'
#' # Freeze baseline to first 24 samples
#' qic(y, n, x = week, data = d, chart = 'p', freeze = 24)
#'
#' # Break control chart before and after change
#' qic(y, n, x = week, data = d, chart = 'p', breaks = 24)
#'
#' # Introduce extreme sample value and notes
#' d$a <- ''
#' d$a[30] <- 'Extreme value'
#' d$y[30] <- 1
#' qic(y, n, x = week, data = d, chart = 'p',
#'     breaks = 24,
#'     notes = a)
#'
#' # Exclude value from calculations
#' d$a[30] <- 'Value excluded from calculations'
#' qic(y, n, x = week, data = d, chart = 'p',
#'     breaks = 24,
#'     notes = a,
#'     exclude = 30)

qic <- function(y,
                n,
                x,
                data,
                chart =
                  c('run',
                    'i',
                    'mr',
                    'xbar',
                    's',
                    't',
                    'p',
                    'c',
                    'u',
                    'g'),
                notes        = NULL,
                cl           = NULL,
                ylim         = NULL,
                target       = NULL,
                freeze       = NULL,
                breaks       = NULL,
                exclude      = NULL,
                negy         = TRUE,
                dots.only    = FALSE,
                decimals     = 1,
                multiply     = 1,
                x.format     = '%Y-%m-%d',
                cex          = 0.8,
                main,
                xlab         = 'Subgroup',
                ylab         = 'Indicator',
                pre.text     = 'Before data',
                post.text    = 'After data',
                runvals      = FALSE,
                linevals     = TRUE,
                plot.chart   = TRUE,
                prnt         = FALSE,
                primed       = FALSE,
                standardised = FALSE,
                ...) {

  # Select chart type
  type <- match.arg(chart)
  fn <- paste0('qic.', type)

  # Prepare chart title
  if(missing(main))
    main <- paste(toupper(type), "Chart of", deparse(substitute(y)))

  # Get data, sample sizes, subgroups, and notes
  if(!missing(data)){
    class(data) <- 'data.frame'
    y <- data[,deparse(substitute(y))]
    if(deparse(substitute(n)) %in% colnames(data))
      n <- data[,deparse(substitute(n))]
    if(deparse(substitute(x)) %in% colnames(data))
      x <- data[,deparse(substitute(x))]
    if(deparse(substitute(notes)) %in% colnames(data))
      notes <- data[,deparse(substitute(notes))]
  }

  # Check arguments
  if(missing(y))
    stop('\"y\" argument must be provided')
  if(any(type == c('p', 'u')) & missing(n))
    stop('\"n\" argument must be provided for P and U charts')
  if(any(type == c('xbar', 's')) & missing(x))
    stop('\"x\" argument must be provided for Xbar and S charts')
  if(any(type == c('xbar', 's', 'c', 'g', 't')) & !missing(n)) {
    warning('\"n\" argument is only relevant for P, U, and I control charts and will be ignored')
    n <- rep(1, length(y))
  }
  if(missing(n)) {
    n <- rep(1, length(y))
  } else {
    if(length(n) != length(y))
      stop('\"y\" and \"n\" arguments must have same length')
  }
  if(missing(x)) {
    x <- 1:length(y)
  } else {
    if(length(x) != length(y))
      stop('\"y\" and \"x\" arguments must have same length')
  }

  # Fix missing values
  cases <- complete.cases(y, n)
  y[!cases] <- NA
  n[!cases] <- NA

  # Clear freeze argument if breaks argument is given
  if(!is.null(breaks)){
    freeze <- NULL
    cl <- NULL
  }

  # Create data frame of values to analyse
  d <- data.frame(y.sum  = tapply(y, x, sum, na.rm = T),
                  y.mean = tapply(y, x, mean, na.rm = T),
                  y.sd   = tapply(y, x, sd, na.rm = T),
                  y.n    = tapply(n, x, sum, na.rm = T))

  # Check that subgroups are unique for T and G charts
  if(any(type == c('t', 'g')) & max(d$y.n, na.rm = TRUE) > 1)
    stop('The grouping argument, \"x\", must contain unique values for T and G charts')

  # Replace NaN values with NA (if subgroup is empty)
  d[is.nan(d$y.mean),] <- NA

  # Get number of data points
  n.obs <- nrow(d)

  # Fix notes
  if(missing(notes)) {
    notes <- rep(NA, n.obs)
  } else {
    notes <- as.character(notes)
    notes[notes == ''] <- NA
    notes <- c(notes, rep(NA, length(y) - length(notes)))
    suppressWarnings(notes <- tapply(notes, x, min, na.rm = TRUE))
    dimnames(notes) <- NULL
  }

  # Create indices of chart parts (if breaks argument is given)
  if(is.null(breaks))
    breaks <- n.obs
  breaks   <- breaks[order(breaks)]
  breaks   <- breaks[breaks > 1 & breaks < n.obs - 1]
  start    <- c(1, breaks + 1)
  end      <- c(breaks, n.obs)
  parts    <- list()

  for(i in 1:length(start)) {
    parts[[i]] <- seq(start[i], end[i])
  }

  # Perform data analysis according to chart type
  qic <- list()

  for(p in parts) {
    # Calculate indices of data points to exclude within each chart part
    ex <- exclude[exclude %in% p]
    if(length(ex)) {
      ex <- ex - min(p) + 1
    } else {
      ex <- NULL
    }

    # Build qic object
    y <- do.call(fn, list(d = d[p,],
                          cl = cl,
                          freeze = freeze,
                          exclude = ex,
                          primed = primed,
                          standardised = standardised))
    qic$y   <- c(qic$y, y$y)
    qic$cl  <- c(qic$cl, y$cl)
    qic$lcl <- c(qic$lcl, y$lcl)
    qic$ucl <- c(qic$ucl, y$ucl)
  }

  # Prevent negative y axis if negy argument is FALSE
  if(!negy & min(qic$y, na.rm = TRUE) >= 0)
    qic$lcl[qic$lcl < 0] <- NA

  # Multiply y axis by multiply argument
  qic$y   <- as.vector(qic$y) * multiply
  qic$cl  <- as.vector(qic$cl) * multiply
  qic$lcl <- as.vector(qic$lcl) * multiply
  qic$ucl <- as.vector(qic$ucl) * multiply

  # Create x axis labels
  labels <- row.names(d)

  if(inherits(x, c('Date','POSIXct', 'POSIXt'))) {
    labels <- as.Date(labels)
    labels <- format(labels, format = x.format)
  }

  # Perform runs analysis
  if(is.null(ex)) {
    runs             <- sign(qic$y - qic$cl)
  } else {
    runs             <- sign(qic$y[-ex] - qic$cl[-ex])
  }

  runs               <- runs[runs != 0 & !is.na(runs)]
  n.usefull          <- length(runs)

  if(n.usefull) {
    run.lengths      <- rle(runs)$lengths
    n.runs           <- length(run.lengths)
    longest.run      <- max(run.lengths)
    longest.run.max  <- round(log2(n.usefull)) + 3                # Schilling 2012
    n.crossings      <- max(n.runs - 1, 0)
    n.crossings.min  <- qbinom(0.05, max(n.usefull - 1, 0), 0.5)  # Chen 2010 (7)
    runs.test        <- longest.run > longest.run.max |
      n.crossings < n.crossings.min
  } else {
    longest.run      <- NA
    longest.run.max  <- NA
    n.crossings      <- NA
    n.crossings.min  <- NA
    runs.test        <- FALSE
  }

  signals            <- which(qic$y > qic$ucl | qic$y < qic$lcl)

  # Complete qic object
  qic$n               <- as.vector(d$y.n)
  qic$labels          <- labels
  qic$notes           <- notes
  qic$parts           <- parts
  qic$main            <- main
  qic$xlab            <- xlab
  qic$ylab            <- ylab
  qic$freeze          <- freeze
  qic$exclude         <- exclude
  qic$n.obs           <- n.obs
  qic$target          <- target
  qic$n.usefull       <- n.usefull
  qic$longest.run     <- longest.run
  qic$longest.run.max <- longest.run.max
  qic$n.crossings     <- n.crossings
  qic$n.crossings.min <- n.crossings.min
  qic$runs.test       <- runs.test
  qic$signals         <- signals

  # Plot qic chart
  if(plot.chart)
    plot.qic(qic = qic,
             dots.only = dots.only,
             decimals  = decimals,
             runvals   = runvals,
             linevals  = linevals,
             ylim      = ylim,
             pre.text  = pre.text,
             post.text = post.text,
             cex = cex,
             ...)

  # Return qic object
  if(prnt) {
    return(qic)
  } else {
    invisible(qic)
  }
}

qic.run <- function(d, freeze, cl, exclude, ...){
  # Calcutate indicator to plot

  y <- d$y.sum / d$y.n

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line
  if(is.null(cl))
    cl <- median(y[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate limits
  ucl <- NULL
  lcl <- NULL

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.i <- function(d, freeze, cl, exclude, ...) {

  # Get indicator to plot
  y <- d$y.sum / d$y.n

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- (1:freeze)
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line
  if(is.null(cl))
    cl <- mean(y[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate moving ranges
  mr <- abs(diff(y[base]))
  amr <- mean(mr, na.rm = T)

  # Calculate upper limit for moving ranges
  ulmr <- 3.267 * amr

  # Remove moving ranges bigger than ulmr and recalculate amr, Provost
  mr <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = T)

  # Calculate standard deviation, Montgomery, p. 6.33
  stdev <- amr/1.128
  stdev <- rep(stdev, y.length)

  # Calculate limits
  lcl <- cl - 3 * stdev
  ucl <- cl + 3 * stdev

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.mr <- function(d, freeze, cl, exclude, ...) {

  # Calcutate indicator to plot
  y <- d$y.sum / d$y.n
  y <- c(NA, abs(diff(y)))

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line
  if(is.null(cl))
    cl <- mean(y[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate upper limit for moving ranges
  ucl <- 3.27 * cl

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = 0,
              ucl = ucl))
}

qic.t <- function(d, freeze, cl, exclude, ...) {

  # Get values to plot
  y <- d$y.sum

  if(min(y, na.rm = T) < 0) {
    stop('Time between events cannot contain negative values')
  }

  if(min(y, na.rm = T) == 0) {
    y[y == 0] <- 0.1
    warning('Time between events should not contain zero values. Zeros replaced by 0.1')
  }

  # Transform measures, Montgomery 7.28
  y <- y^(1 / 3.6)
  d$y.sum <- y

  # Calculate center and limits for I chart
  qic <- qic.i(d, freeze, cl, exclude, ...)

  # Back transform center line and limits
  y = qic$y^3.6
  cl = qic$cl^3.6
  ucl = qic$ucl^3.6
  lcl = qic$lcl^3.6
  lcl[lcl < 0 | is.nan(lcl)] <- NA

  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.xbar <- function(d, freeze, cl, exclude, ...){

  # Get values to plot
  y <- d$y.mean
  n <- d$y.n
  s <- d$y.sd
  excl <- which(is.na(s))

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line, Montgomery 6.30
  if(is.null(cl))
    cl <- sum(n[base] * y[base], na.rm = T) / sum(n[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate standard deviation and control limits, Montgomery 6.28
  stdev <- sqrt(sum(s[base]^2 * (n[base] - 1), na.rm = T) /
                  sum(n[base] - 1, na.rm = T))

  #   stdev[excl] <- NA
  A3 <- a3(n)
  ucl <- cl + A3 * stdev
  lcl <- cl - A3 * stdev

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.s <- function(d, freeze = NULL, cl, exclude, ...){

  # Get values to plot
  s <- d$y.sd
  n <- d$y.n
  excl <- which(is.na(s))

  # Get number of subgroups
  y.length <- length(s)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line, Montgomery 6.31
  sbar <- sqrt(sum(s[base]^2 * (n[base] - 1), na.rm = T) /
                 (sum(n[base], na.rm = T) - y.length))
  cl <- rep(sbar, y.length)
  B3 <- b3(n)
  B4 <- b4(n)
  ucl <- B4 * sbar
  lcl <- B3 * sbar

  # Return object to calling function
  return(list(y = s,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.p <- function(d, freeze, cl, exclude, primed, standardised, ...){

  # Calcutate indicator to plot
  n <- d$y.sum
  N <- d$y.n
  y <- n / N

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line, Montgomery 7.7
  if(is.null(cl))
    cl <- sum(n[base], na.rm = T) / sum(N[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate standard deviation, Montgomery 7.8
  stdev <- sqrt(cl * (1 - cl) / N)

  # Calculate standard deviation for Laney's p-primed chart, incorporating
  # between-subgroup variation.
  if(primed) {
    z_i <- (y[base] - cl[base]) / stdev[base]
    sigma_z <- mean(abs(diff(z_i))) / 1.128
    stdev <- stdev * sigma_z
  }

  # Calculate limits
  ucl <- cl + 3 * stdev
  ucl[ucl > 1] <- NA
  lcl <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  # Calculations for standardised control chart, Montgomery 7.14
  if(standardised) {
    y <- (y[base] - cl[base]) / stdev[base]  # "z_i" in Montgomery
    cl <- rep(0, y.length)
    ucl <- rep(3, y.length)
    lcl <- rep(-3, y.length)
  }

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.c <- function(d, freeze, cl, exclude, ...){

  # Calcutate indicator to plot
  y <- d$y.sum

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line
  if(is.null(cl))
    cl <- mean(y[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate standard deviation, Montgomery 7.17
  stdev <- sqrt(cl)

  # Calculate limits
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.u <- function(d, freeze, cl, exclude, primed, standardised, ...){

  # Calcutate indicator to plot
  n <- d$y.sum
  N <- d$y.n

  y <- n / N

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]


  # Calculate center line
  if(is.null(cl))
    cl <- sum(n[base], na.rm = T) / sum(N[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate standard deviation, Montgomery 7.19
  stdev <- sqrt(cl / N)

  # Calculate standard deviation for Laney's p-primed chart, incorporating
  # between-subgroup variation.
  if(primed) {
    z_i <- (y[base] - cl[base]) / stdev[base]
    sigma_z <- mean(abs(diff(z_i))) / 1.128
    stdev <- stdev * sigma_z
  }

  # Calculate limits
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  # Calculations for standardised control chart, Montgomery 7.20
  if(standardised) {
    y <- (y[base] - cl[base]) / stdev[base]  # "u_i" in Montgomery
    cl <- rep(0, y.length)
    ucl <- rep(3, y.length)
    lcl <- rep(-3, y.length)
  }

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

qic.g <- function(d, freeze, cl, exclude, ...){

  # Calcutate indicator to plot
  y <- d$y.sum

  # Get number of subgroups
  y.length <- length(y)

  # Define subgroups to be used in calculations
  if(is.null(freeze))
    freeze <- y.length
  base <- 1:freeze
  if(!is.null(exclude))
    base <- base[-exclude]

  # Calculate center line
  if(is.null(cl))
    cl <- mean(y[base], na.rm = T)
  cl <- rep(cl, y.length)

  # Calculate standard deviation:
  # Benneyan (2001)
  # Montgomery, p. 319
  stdev <- sqrt(cl * (cl + 1))

  # Calculate limits
  ucl <- cl + 3 * stdev
  lcl <- cl - 3 * stdev
  lcl[lcl < 0] <- NA

  #   Set center line to theoretical median, Provost (2011) p. 228
  cl <- 0.693 * cl

  # Return object to calling function
  return(list(y = y,
              cl = cl,
              lcl = lcl,
              ucl = ucl))
}

a3 <- function(n) {
  n[n == 0] <- NA
  tbl <- c(NA,
           2.659, 1.954, 1.628, 1.427, 1.287, 1.182,
           1.099, 1.032, 0.975, 0.927, 0.886, 0.850,
           0.817, 0.789, 0.763, 0.739, 0.718, 0.698,
           0.680, 0.663, 0.647, 0.633, 0.619, 0.606)
  x <- 3 / (4 * (n - 1)) * (4 * n - 3) / sqrt(n)
  w <- which(n <= 25)
  x[w] <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

b3 <- function(n) {
  n[n == 0] <- NA
  tbl <- c(NA,
           0.000, 0.000, 0.000, 0.000, 0.030, 0.118,
           0.185, 0.239, 0.284, 0.321, 0.354, 0.382,
           0.406, 0.428, 0.448, 0.466, 0.482, 0.497,
           0.510, 0.523, 0.534, 0.545, 0.555, 0.565)
  x <- 1 - (3 / c4(n) / sqrt(2 * (n - 1)))
  w <- which(n <= 25)
  x[w] <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

b4 <- function(n) {
  tbl <- c(NA,
           3.267, 2.568, 2.266, 2.089, 1.970, 1.882,
           1.815, 1.761, 1.716, 1.679, 1.646, 1.618,
           1.594, 1.572, 1.552, 1.534, 1.518, 1.503,
           1.490, 1.477, 1.466, 1.455, 1.445, 1.435)
  x <- 1 + (3 / c4(n) / sqrt(2 * (n - 1)))
  w <- which(n <= 25)
  x[w] <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

c4 <- function(n) {
  n[n == 0] <- NA
  tbl <- c(NA,
           0.7979, 0.8862, 0.9213, 0.9400, 0.9515, 0.9594,
           0.9650, 0.9693, 0.9727, 0.9754, 0.9776, 0.9794,
           0.9810, 0.9823, 0.9835, 0.9845, 0.9854, 0.9862,
           0.9869, 0.9876, 0.9882, 0.9887, 0.9892, 0.9896)

  x <- 4 * (n - 1) / (4 * n - 3)
  w <- which(n <= 25)
  x[w] <- tbl[n[w]]
  x[is.nan(x)] <- NA
  return(x)
}

plot.qic <- function(qic,
                     dots.only,
                     decimals,
                     runvals,
                     linevals,
                     ylim,
                     pre.text,
                     post.text,
                     cex,
                     ...) {
  col1            <- rgb(093, 165, 218, maxColorValue = 255)
  col2            <- rgb(223, 092, 036, maxColorValue = 255)
  col3            <- rgb(140, 140, 140, maxColorValue = 255)
#   col3            <- 'grey50'
  lwd             <- cex
  n.obs           <- qic$n.obs
  y               <- qic$y
  x               <- 1:n.obs
  cl              <- qic$cl
  lcl             <- qic$lcl
  ucl             <- qic$ucl
  target          <- qic$target
  signals         <- qic$signals
  runs.test       <- qic$runs.test
  freeze          <- qic$freeze
  parts           <- qic$parts
  exclude         <- qic$exclude
  labels          <- qic$labels
  main            <- qic$main
  xlab            <- qic$xlab
  ylab            <- qic$ylab
  type            <- ifelse(dots.only, 'p', 'o')
  pch             <- ifelse(dots.only, 19, 20)
  notes           <- qic$notes
  n.usefull       <- qic$n.usefull
  longest.run     <- qic$longest.run
  longest.run.max <- qic$longest.run.max
  n.crossings     <- qic$n.crossings
  n.crossings.min <- qic$n.crossings.min
  cex             <- par('cex') * cex  # Text size adjustment
  ylim            <- range(ylim, y, ucl, lcl, cl, target, na.rm = T)

  # Setup plot margins
  mar             <- par('mar') + c(-0.5, 0, 0, 0)
#     if(!linevals)
#       mar           <- mar + c(0, 0, 0, -2)
#     if(main == '')
#       mar           <- mar + c(0, 0, -1.8, 0)
#     if(xlab == '')
#       mar           <- mar + c(-1, 0, 0, 0)
#     if(ylab == '')
#       mar           <- mar + c(0, -1, 0, 0)
  if(runvals & !dots.only)
    mar           <- mar + c(1.5, 0, 0, 0)
  op              <- par(mar = mar)

  # setup empty plot area
  plot(x    = 1:n.obs,
       y    = y,
       type = 'n',
       xaxt = 'n',
       yaxt = 'n',
       bty  = 'n',
       ylim = ylim,
       xlab = '',
       ylab = '',
       ...)

  # add x axis and title to plot
  axis(1,
       at = 1:n.obs,
       labels = labels,
       tcl = -0.2,
       lwd = 0,
       lwd.ticks = lwd,
       cex.axis = cex,
       col = col3,
       ...)
  axis(2,
       tcl = -0.2,
       lwd = 0,
       lwd.ticks = lwd,
       cex.axis = cex,
       col = col3,
       las = 2,
       ...)
  box(bty = 'l',
      lwd = lwd,
      col = col3)
  title(main = main,
        adj = 0,
        line = 2.7,
        cex.main = cex * 1.25,
        font.main = 1)
  title(xlab = xlab, ylab = ylab, cex.lab = cex)

  # Color and dash center line if non random variation is present
  lty <- 1
  col <- col3
  if(runs.test & !dots.only) {
    lty <- 2
    col <- col2
  }

  # Add lines to plot
  for(p in parts) {
    lines(p, cl[p], col = col, lty = lty, lwd = lwd * 1.5)
    lines(p, ucl[p], lty = 1, col = col3, lwd = lwd)
    lines(p, lcl[p], lty = 1, col = col3, lwd = lwd)
    lines(p, y[p], type = type, col = col1, lwd = lwd * 4,
          pch = pch, cex = cex)
  }
  # add target line
  if(!is.null(target))
    lines(1:n.obs, rep(target, n.obs), lty = 3, col = col3,
          lwd = lwd)

  # annotate before and after data if freeze argument is given
  if(!is.null(freeze)) {
    abline(v = freeze + 0.5,
           col = col3,
           lty = 3,
           lwd = lwd)
    mtext(pre.text,
          at = freeze / 2,
          cex = cex * 0.9,
          line = 0.7)
    mtext(post.text,
          at = freeze + (n.obs - freeze) / 2,
          cex = cex * 0.9,
          line = 0.7)
  }

  # color data points outside sigma limits
  points(signals, y[signals], col = col2, pch = pch, cex = cex * 1.5)

  # mark excluded data points
  points(exclude, y[exclude], bg = 0, col = col3, pch = 21, cex = cex * 1.2)

  # add values for center and limits to the plot
  if(linevals) {
    mtext(round(cl[n.obs], decimals),
          side = 4,
          at = cl[n.obs],
          las = 1,
          cex = cex * 0.9)
    mtext(round(ucl[n.obs], decimals),
          side = 4,
          at = ucl[n.obs],
          las = 1,
          cex = cex * 0.9)
    mtext(round(lcl[n.obs], decimals),
          side = 4,
          at = lcl[n.obs],
          las = 1,
          cex = cex * 0.9)
    if(!is.null(target))
      mtext(round(target, decimals),
            side = 4,
            at = target,
            las = 1,
            cex = cex * 0.9)
  }

  # Print statistics from runs analysis to plot
  if(runvals & !dots.only) {
    mtext(paste0('Obs. (usefull) = ', sum(!is.na(y)),
                 ' (', n.usefull, ')'),
          cex = cex * 0.9,
          side = 1,
          line = 4.5,
          adj = 0)
    mtext(paste0('Longest run (max) = ', longest.run,
                 ' (', longest.run.max, ')'),
          cex = cex * 0.9,
          side = 1,
          line = 4.5,
          adj = 0.5,
          col = ifelse(longest.run > longest.run.max, col2, 1))
    mtext(paste0('Crossings (min) = ', n.crossings,
                 ' (', n.crossings.min, ')'),
          cex = cex * 0.9,
          side = 1,
          adj = 1,
          line = 4.5,
          col = ifelse(n.crossings < n.crossings.min, col2, 1))
  }

  # Add notes to plot
  i.ann <- which(!is.na(notes))
  if(length(i.ann)) {
    x.ann <- x[i.ann]
    y.ann <- y[i.ann]
    mtext(notes,
          side = 3,
          at = x,
          adj = 0.5,
          cex = cex * 0.9)
    segments(x.ann,
             max(ylim, na.rm = TRUE) * 1.05,
             y1 = y.ann,
             lty = 3,
             lwd = lwd)
  }
  par(op)
}
