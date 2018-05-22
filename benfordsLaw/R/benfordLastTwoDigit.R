#' Benford's Last Two Digit Test
#' 
#' This function performs Benford's last two digit test, using a z-critical value
#' of 1.96 by default (equivalent to a 95\% confidence interval). The results show each digit's
#' test results, as well as the overall mean absolute deviation (MAD).
#' @param columnToTest The column of a dataframe with the values to apply Benford's Law.
#' @param returnValue The output type returned by the function. Options are dataframe or list.
#' @return Returns a dataframe or list, depending on the argument given
#' @export
#' @examples
#' benfordLastTwoDigit(iris$Sepal.Width, returnValue='list')

benfordLastTwoDigit <- function(columnToTest, returnValue='dataframe') {
  #Isolate digits for testing
  dl <- as.numeric(columnToTest)
  b <- data.frame(column_to_test = dl,
                  lastTwoDigit = as.factor(as.numeric(substring(dl, nchar(dl)-2+1, nchar(dl)))))
  
  #Expected ratios according to Benford's Law
  lastTwoDigitBenford <- rep(0.01, 100)
  names(lastTwoDigitBenford) <- c(0:99)
  
  #Observed ratios in the data
  rowCount <- length(dl)
  lastTwoDigitFrequency <- summary(b$lastTwoDigit)
  #Insert 0's for missing digits
  for (i in 0:99) {
    if (!(i %in% names(lastTwoDigitFrequency))) {
      lastTwoDigitFrequency <- append(lastTwoDigitFrequency, 0, after=i-1)
    }
  }
  names(lastTwoDigitFrequency) <- c(0:99)
  lastTwoDigitObserved <- lastTwoDigitFrequency/rowCount
  
  #Z-scores for each digit test
  lastTwoDigitZN <- ifelse (1/(2*rowCount) < abs(lastTwoDigitObserved-lastTwoDigitBenford),
                            abs(lastTwoDigitObserved-lastTwoDigitBenford) - 1/(2*rowCount),
                            abs(lastTwoDigitObserved-lastTwoDigitBenford))
  lastTwoDigitZD <- sqrt((lastTwoDigitBenford*(1-lastTwoDigitBenford))/rowCount)
  lastTwoDigitZ <- lastTwoDigitZN/lastTwoDigitZD
  
  #Z Critical value at 95% confidence
  zCrit <- 1.96
  
  #Test for significance: if higher than critical value then abnormal
  lastTwoDigitTests <- abs(lastTwoDigitZ) > zCrit
  
  #Combine the expected vs observed frequencies and ratios of each digit and calculate MAD values
  lastTwoDigitObservedFreq <- lastTwoDigitFrequency
  lastTwoDigitExpectedFreq <- lastTwoDigitBenford*rowCount
  ftindexValue <- as.factor(c(0:99))
  lastTwoDigitResults <- data.frame(ftindexValue, lastTwoDigitObservedFreq, lastTwoDigitExpectedFreq, lastTwoDigitTests)
  colnames(lastTwoDigitResults) <- c('Last_Two_Digits', 'Observed_Frequency', 'Expected_Frequency', 'Abnormal_Flag')
  lastTwoDigitResults$Perc_Abnormal <- nrow(lastTwoDigitResults[lastTwoDigitResults$Abnormal_Flag==T,])/nrow(lastTwoDigitResults)
  lastTwoDigitResults$Observed_Ratio <- lastTwoDigitObserved
  lastTwoDigitResults$Expected_Ratio <- lastTwoDigitBenford
  l2dMAD <- sum(abs(lastTwoDigitResults$Observed_Ratio - lastTwoDigitResults$Expected_Ratio))/nrow(lastTwoDigitResults)
  lastTwoDigitResults$MAD_Value <- l2dMAD
  lastTwoDigitResults$MAD_Conformity <- "Close Conformity"
  if (l2dMAD <= 0.004) {
    lastTwoDigitResults$MAD_Conformity <- "Close Conformity"
  } else if (l2dMAD > 0.004 & l2dMAD <= 0.008) {
    lastTwoDigitResults$MAD_Conformity <- "Acceptable Conformity"
  } else if (l2dMAD > 0.008 & l2dMAD <= 0.012) {
    lastTwoDigitResults$MAD_Conformity <- "Marginally Acceptable Conformity"
  } else if (l2dMAD > 0.012) {
    lastTwoDigitResults$MAD_Conformity <- "Non-Conformity"
  }
  
  if (returnValue=='list') {
    resultsList <- list(MAD_CONFORMITY=unique(lastTwoDigitResults$MAD_Conformity),
                        MAD_VALUE=unique(lastTwoDigitResults$MAD_Value),
                        PERC_ABNORMAL=unique(lastTwoDigitResults$Perc_Abnormal),
                        FIRST_DIGITS=lastTwoDigitResults$Last_Two_Digits,
                        OBSERVED_RATIO=lastTwoDigitResults$Observed_Ratio,
                        EXPECTED_RATIO=lastTwoDigitResults$Expected_Ratio,
                        OBSERVED_FREQUENCY=lastTwoDigitResults$Observed_Frequency,
                        EXPECTED_FREQUENCY=lastTwoDigitResults$Expected_Frequency,
                        ABNORMAL_FLAG=lastTwoDigitResults$Abnormal_Flag)
    return(resultsList)
  } else {
    return(lastTwoDigitResults)
  }
}