# chapter1
install.packages('installr')
library(installr)
install.Rtools()
install.packages('pkgbuild')
library(pkgbuild)
install.packages("cli")
install.packages("devtools")

library("microbenchmark")
library("profvis")
library("ggplot2")
help.start
vignette()
vignette(package = "dplyr", "dplyr")
microbenchmark::microbenchmark()

library("microbenchmark")
df = data.frame(v = 1:4, name = letters[1:4])
microbenchmark(df[3, 2], df[3, "name"], df$name[3])


x = 1:100 # initiate vector to cumulatively sum

# Method 1: with a for loop (10 lines)
cs_for = function(x) {
  for (i in x) {
    if (i == 1) {
      xc = x[i]
    } else {
      xc = c(xc, sum(x[1:i]))
    }
  }
  xc
}

# Method 2: with apply (3 lines)
cs_apply = function(x) {
  sapply(x, function(x)
    sum(1:x))
}

# Method 3: cumsum (1 line, not shown)
microbenchmark(cs_for(x), cs_apply(x), cumsum(x))



# Test how long it takes to subset the data frame 50,000 times:
system.time(for (i in 1:50000) {
  df[3, 2]
})

devtools::install_github("csgillespie/efficient",
                         build_vignettes = TRUE,
                         dependencies = TRUE)

# chapter2
install.packages('benchmarkme')
library("benchmarkme")

Sys.info()

# Note: uses 2+ GB RAM and several seconds or more depending on hardware
# 1: Create large dataset
X = as.data.frame(matrix(rnorm(1e8), nrow = 1e7))
# 2: Find the median of each column using a single core
r1 = lapply(X, median)
# 3: Find the median of each column using many cores
r2 = parallel::mclapply(X, median)

installr::updateR()

pkgs = c("raster", "leaflet", "rgeos") # package names
install.packages(pkgs)

inst = lapply(pkgs, library, character.only = TRUE) # load them


installr::install.rtools()
update.packages() # update installed CRAN packages

R--vanilla
R.home()
Sys.getenv("HOME")
getwd()

# file.edit("~/.Rprofile") # edit .Rprofile in HOME/
# file.edit(".Rprofile") # edit project specific .Rprofile

user_renviron = path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron) # open with another text editor if this fails

file.path("C:", "DATA", "data.csv")

site_path = R.home(component = "home")
fname = file.path(site_path, "etc", "Rprofile.site")
file.exists(fname)
file.exists("~/.Rprofile")


install.packages("fortunes")
fortunes::fortune(1)

# ht == headtail
# Show the first 6 rows & last 6 rows of a data frame
ht = function(d, n = 6)
  rbind(head(d, n), tail(d, n))
# Show the first 5 rows & first 5 columns of a data frame
hh = function(d)
  d[1:5, 1:5]

rm(list = ls())

download.file(
  "https://www.census.gov/2010census/csv/pop_change.csv",
  "extdata/pop_change.csv"
)

rnbinom()

USArrests$Assault

library("benchmarkme")
get_linear_algebra()

install.packages("atlas")

# chapter 3
library("compiler")
library("memoise")
install.packages("pryr")
library("pryr")
library("microbenchmark")
? microbenchmark

n <- 1000
method1 = function(n) {
  vec = NULL # Or vec = c()
  for (i in seq_len(n))
    vec = c(vec, i)
  vec
}
method2 = function(n) {
  vec = numeric(n)
  for (i in seq_len(n))
    vec[i] = i
  vec
}
method3 = function(n)
  seq_len(n)
microbenchmark(times = 100,
               unit = "s",
               method1(n),
               method2(n),
               method3(n))


monte_carlo = function(N) {
  hits = 0
  for (i in seq_len(N)) {
    u1 = runif(1)
    u2 = runif(1)
    if (u1 ^ 2 > u2)
      hits = hits + 1
  }
  return(hits / N)
}
N = 500000
system.time(monte_carlo(N))
monte_carlo_vec = function(N)
  sum(runif(N) ^ 2 > runif(N)) / N
system.time(monte_carlo_vec(N))
monte_carlo_vec(N)
monte_carlo(N)
runif(N)

library("STAT")
install.packages("STAT")

integrand <- function(x) {
  sqrt(x)
}
integrate(integrand, lower = 0, upper = 1)


# Suppress the error message
good = try(1 + 1, silent = TRUE)
bad = try(1 + "1", silent = TRUE)
bad


regression_plot = function(x, y, ...) {
  # Plot and pass additional arguments to default plot method
  plot(x, y, ...)
  
  # Fit regression model
  model = lm(y ~ x)
  
  # Add line of best fit to the plot
  abline(model)
  invisible(model)
}
out = regression_plot(x, y)

x = 4:6
c(x)
#> [1] 4 5 6
c(factor(x))
#> [1] 1 2 3

m = c("January", "December", "March")
# month.name contains the 12 months
fac_m = factor(m, levels = month.name)
sort(fac_m)

## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 1, mean, trim = .2)
x

install.packages("ggplot2movies")
data(movies, package = "ggplot2movies")
ratings = movies[, 7:16]
ratings
ratings[1,]

popular = apply(ratings, 1, nnet::which.is.max)
plot(table(popular))

two_cols = data.frame(x = 1:5, y = letters[1:5])
zero_cols = data.frame()
sapply(two_cols, class)  # a character vector
sapply(zero_cols, class) # a list
two_cols[, 1:2]          # a data.frame
two_cols[, 1]            # an integer vector

example("apply")
example("factor")


apply(x, 2, function(i)
  mean(i) / sd(x))
sd_x = sd(x)
apply(x, 2, function(i)
  mean(i) / sd_x)

# Argument indicates row to remove
plot_mpg = function(row_to_remove) {
  data(mpg, package = "ggplot2")
  mpg = mpg[-row_to_remove,]
  plot(mpg$cty, mpg$hwy)
  lines(lowess(mpg$cty, mpg$hwy), col = 2)
}
m_plot_mpg = memoise(plot_mpg)
microbenchmark(times = 10,
               unit = "ms",
               m_plot_mpg(10),
               plot_mpg(10))

? closures()

getFunction("mean")

library("compiler")
mean_r = function(x) {
  m = 0
  n = length(x)
  for (i in seq_len(n))
    m = m + x[i] / n
  m
}

cmp_mean_r = cmpfun(mean_r)
x = rnorm(1000)
microbenchmark(times = 10,
               unit = "ms",
               # milliseconds
               mean_r(x),
               cmp_mean_r(x),
               mean(x))

## Windows users will need Rtools
install.packages("ggplot2", type = "source")

# install.packages("ggplot2", type = "source", INSTALL_opts = "--byte-compile")
