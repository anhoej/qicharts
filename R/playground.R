library(readxl)
d <- read_excel('../qictest/montgomery.xls', 'table 9.1')

# cusumchart <- function(y,
#                        x,
#                        target,
#                        sdev,
#                        shift = NULL,
#                        k     = 0.5,
#                        h     = 5) {
#
# }

y <- d$x
target <- 10
sdev <- 1
k <- 0.5
h <- 5

n_obs <- length(y)

shift <- sdev
K <- k * shift
H <- h * shift

upper_target <- target + K
lower_target <- target - k

c_plus <- rep(0, n_obs + 1)
c_minus <- rep(0, n_obs + 1)
n_plus <- rep(0, n_obs + 1)
n_minus <- rep(0, n_obs + 1)

for (i in 2:(n_obs + 1)) {
  c_plus[i] <- max(0, y[i - 1] - upper_target + c_plus[i -1], na.rm = T)
  if (c_plus[i] > 0) n_plus[i] <- n_plus[i - 1] + 1
  c_minus[i] <- max(0, lower_target - y[i - 1] + c_minus[i -1], na.rm = T)
  if (c_minus[i] > 0) n_minus[i] <- n_minus[i - 1] + 1
}

c_plus <- c_plus[-1]
c_minus <- c_minus[-1]
n_plus <-  n_plus[-1]
n_minus <- n_minus[-1]
