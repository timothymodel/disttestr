#' Modality test
#'
#' This function runs and neatly presents modality tests on a vector
#' @param x Vector of values
#' @keywords modality tests
#' @export
#' @examples
#' ModalityTest(dataframe$vector)

ModalityTest <- function(x){ #x is a vector for the voter turnour
  require(diptest)
  require(modes)
  require(dplyr)
  DIP <- dip.test(x, simulate.p.value = F) # Dip test
  DIP.d <- c()
  DIP.d <- DIP$statistic # Dip statistic
  DIP.p <- c()
  DIP.p <- DIP$p.value # Dip statistic p value
  BiCoef <- bimodality_coefficient(x, finite = T) # Bimodality coefficient
  BiRatio <- bimodality_ratio(x, list = F) # Bimodality ratio
  Kurt <- kurtosis(x, finite = T) # Kurtosis
  Skew <- skewness(x) # Skewness
  DistribSum <- tibble(
    "Dip Statistic" = round(DIP.d,5),
    "Dip P-Value" = round(DIP.p,5),
    "Bimodality Coef." = round(BiCoef,5),
    "Bimodality Ratio" = round(BiRatio,5),
    "Kurtosis" = round(Kurt,5),
    "Skewness" = round(Skew,5)
  )
  DistribSum <- as.data.frame(cbind(nms = names(DistribSum), t(DistribSum)))
  names(DistribSum)[names(DistribSum) == "nms"] <- "Test"
  names(DistribSum)[names(DistribSum) == "V2"] <- "Result"
  Interpretation <- c("",
                      "",
                      "",
                      "Proportion of bimodality",
                      "",
                      "")
  DistribSum$Interpretation <- Interpretation
  if (DIP.p > 0.05) {
    DistribSum$Interpretation[2] <- "Do not reject null of unimodality"
  }
  else {DistribSum$Interpretation[2] <- "Reject null of unimodality"
  }
  if (BiCoef > 5/9) {
    DistribSum$Interpretation[3] <- "Evidence of bimodality"
  }
  else {DistribSum$Interpretation[3] <- "No evidence of bimodality"
  }
  print(DistribSum, row.names = F)
}
