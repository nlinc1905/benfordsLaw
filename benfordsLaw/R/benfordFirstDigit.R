#' Benford's First Digit Test
#' 
#' This function performs Benford's first digit test, using a z-critical value
#' of 1.96 by default (equivalent to a 95\% confidence interval). The results show each digit's
#' test results, as well as the overall mean absolute deviation (MAD).
#' @param columnToTest The column of a dataframe with the values to apply Benford's Law.
#' @param returnValue The output type returned by the function. Options are dataframe or list.
#' @return Returns a dataframe or list, depending on the argument given
#' @export
#' @examples
#' benfordFirstDigit(iris$Sepal.Width, returnValue='list')

benfordFirstDigit <- function(columnToTest, returnValue='dataframe') {
  #Remove amounts < 1 unit value (Benford's first digit test doesn't apply to decimals)
  columnToTest <- as.numeric(columnToTest)
  dl <- columnToTest[columnToTest >= 1]
  
  #Isolate digits for testing
  b <- data.frame(column_to_test = dl,
                  firstDigit = as.factor(as.numeric(substring(dl,1,1))))
  
  #Expected ratios according to Benford's Law
  firstDigitBenford <- vector()
  for (i in 1:9) {
    firstDigitBenford[i] <- log((1+(1/i)), 10)
  }
  names(firstDigitBenford) <- c(1:9)
  
  #Observed ratios in the data
  rowCount <- length(dl)
  firstDigitFrequency <- summary(b$firstDigit)
  #Insert 0's for missing digits
  for (i in 1:9) {
    if (!(i %in% names(firstDigitFrequency))) {
      firstDigitFrequency <- append(firstDigitFrequency, 0, after=i-1)
    }
  }
  names(firstDigitFrequency) <- c(1:9)
  firstDigitObserved <- firstDigitFrequency/rowCount
  
  #Z-scores for each digit test
  firstDigitZN <- ifelse (1/(2*rowCount) < abs(firstDigitObserved-firstDigitBenford),
                          abs(firstDigitObserved-firstDigitBenford) - 1/(2*rowCount),
                          abs(firstDigitObserved-firstDigitBenford))
  firstDigitZD <- sqrt((firstDigitBenford*(1-firstDigitBenford))/rowCount)
  firstDigitZ <- firstDigitZN/firstDigitZD
  
  #Z Critical value at 95% confidence
  zCrit <- 1.96
  
  #Test for significance: if higher than critical value then abnormal
  firstDigitTests <- abs(firstDigitZ) > zCrit
  
  #Combine the expected vs observed frequencies and ratios of each digit and calculate MAD values
  firstDigitObserved <- firstDigitFrequency
  firstDigitExpected <- firstDigitBenford*rowCount
  indexValue <- as.factor(c('1','2','3','4','5','6','7','8','9'))
  firstDigitResults <- data.frame(indexValue, firstDigitObserved, firstDigitExpected, firstDigitTests)
  colnames(firstDigitResults) <- c('First_Digit', 'Observed_Frequency', 'Expected_Frequency', 'Abnormal_Flag')
  firstDigitResults$Perc_Abnormal <- nrow(firstDigitResults[firstDigitResults$Abnormal_Flag==T,])/nrow(firstDigitResults)
  firstDigitResults$Observed_Ratio <- firstDigitObserved
  firstDigitResults$Expected_Ratio <- firstDigitBenford
  fdMAD <- sum(abs(firstDigitResults$Observed_Ratio - firstDigitResults$Expected_Ratio))/nrow(firstDigitResults)
  firstDigitResults$MAD_Value <- fdMAD
  firstDigitResults$MAD_Conformity <- "Close Conformity"
  if (fdMAD <= 0.004) {
    firstDigitResults$MAD_Conformity <- "Close Conformity"
  } else if (fdMAD > 0.004 & fdMAD <= 0.008) {
    firstDigitResults$MAD_Conformity <- "Acceptable Conformity"
  } else if (fdMAD > 0.008 & fdMAD <= 0.012) {
    firstDigitResults$MAD_Conformity <- "Marginally Acceptable Conformity"
  } else if (fdMAD > 0.012) {
    firstDigitResults$MAD_Conformity <- "Non-Conformity"
  }
  
  if (returnValue=='list') {
    resultsList <- list(MAD_CONFORMITY=unique(firstDigitResults$MAD_Conformity),
                        MAD_VALUE=unique(firstDigitResults$MAD_Value),
                        PERC_ABNORMAL=unique(firstDigitResults$Perc_Abnormal),
                        FIRST_DIGITS=firstDigitResults$First_Digit,
                        OBSERVED_RATIO=firstDigitResults$Observed_Ratio,
                        EXPECTED_RATIO=firstDigitResults$Expected_Ratio,
                        OBSERVED_FREQUENCY=firstDigitResults$Observed_Frequency,
                        EXPECTED_FREQUENCY=firstDigitResults$Expected_Frequency,
                        ABNORMAL_FLAG=firstDigitResults$Abnormal_Flag)
    return(resultsList)
  } else {
    return(firstDigitResults)
  }
}