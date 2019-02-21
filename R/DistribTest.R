#' Distribution test
#'
#' This function runs and neatly presents distribution tests on a vector
#' @param x Vector of values
#' @keywords distribution tests
#' @export
#' @examples
#' DistribTest(dataframe$vector)


DistribTest <- function(x){ # x is the vector to be tested
  require(goftest)
  require(sn)
  require(modes)
  require(dplyr)
  require(fitdistrplus)
  pois.l <- fitdistr(x, "Poisson")$estimate[[1]]
  geom.p <- fitdistr(x, "geometric")$estimate[[1]]
  t.df <- fitdistr(x, "t")$n[[1]]
  ad.test.pnorm <- ad.test(x, null = "pnorm") # normal
  ad.test.pt <- ad.test(x, null = "pt", df = t.df) # t
  ad.test.psn <- ad.test(x, null = "psn") # skew normal
  ad.test.pst <- ad.test(x, null = "pst") # skew t
  ad.test.plogis <- ad.test(x, null = "plogis") # logistic
  ad.test.ppois <- ad.test(x, null = "ppois", lambda = pois.l) # poisson
  ad.test.pexp <- ad.test(x, null = "pexp") # exponential
  ad.test.pgeom <- ad.test(x, null = "pgeom", prob = geom.p) # geometric
  ad.test.pcauchy <- ad.test(x, null = "pcauchy") # cauchy
  kurt <- kurtosis(x, finite = T) # kurtosis
  skew <- skewness(x) # skewness
  all.tests <- tribble(
    ~x, ~y, ~z,
    "Normal", round(ad.test.pnorm$statistic,3), round(ad.test.pnorm$p.value,3),
    "t", round(ad.test.pt$statistic,3), round(ad.test.pt$p.value,3),
    "Skew-normal", round(ad.test.psn$statistic,3), round(ad.test.psn$p.value,3),
    "Skew-t", round(ad.test.pst$statistic,3), round(ad.test.pst$p.value,3),
    "Logistic", round(ad.test.plogis$statistic,3), round(ad.test.plogis$p.value,3),
    "Poisson", round(ad.test.ppois$statistic,3), round(ad.test.ppois$p.value,3),
    "Exponential", round(ad.test.pexp$statistic,3), round(ad.test.pexp$p.value,3),
    "Geometric", round(ad.test.pgeom$statistic,3), round(ad.test.pgeom$p.value,3),
    "Cauchy", round(ad.test.pcauchy$statistic,3), round(ad.test.pcauchy$p.value,3),
    "Kurtosis", round(kurt,3), "",
    "Skewness", round(skew,3), ""
  )
  all.tests.df <- data.frame(all.tests)
  names(all.tests.df) <- c("test", "statistic", "pval")
  print(all.tests.df, row.names=F)
}


