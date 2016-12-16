#' Benford's First Two Digit Test
#' 
#' This function performs Benford's first two digit test, using a z-critical value
#' of 1.96 by default (equivalent to a 95\% confidence interval). The results show each digit's
#' test results, as well as the overall mean absolute deviation (MAD).
#' @param columnToTest The column of a dataframe with the values to apply Benford's Law.
#' @param returnValue The output type returned by the function. Options are dataframe or list.
#' @return Returns a dataframe or list, depending on the argument given
#' @export
#' @examples
#' benfordFirstTwoDigit(iris$Sepal.Width, returnValue='list')

benfordFirstTwoDigit <- function(columnToTest, returnValue='dataframe') {
  #Remove amounts < 1 unit value (Benford's first 2 digit test doesn't apply to decimals)
  columnToTest <- as.numeric(columnToTest)
  dl <- columnToTest[columnToTest >= 1]
  
  #Remove amounts < 10 unit value (Benford's first 2 digit test doesn't apply to single digits)
  dl <- dl[as.numeric(substring(dl,1,2))>=10]
  
  #Isolate digits for testing
  b <- data.frame(column_to_test = dl,
                  firstTwoDigit = as.factor(as.numeric(substring(dl,1,2))))
  
  #Expected ratios according to Benford's Law
  firstTwoDigitBenford <- vector()
  for (i in 1:99) {
    firstTwoDigitBenford[i] <- log((1+(10*as.numeric(substring(i, 1, 1))+as.numeric(substring(i, 2, 2)))), 10)
  }
  names(firstTwoDigitBenford) <- c(1:99)

  #Remove digits 1-9 because the first 2 digit test doesn't apply to single digits
  firstTwoDigitBenford <- firstTwoDigitBenford[10:99]
  row.names(firstTwoDigitBenford) <- NULL
  
  #Observed ratios in the data
  rowCount <- length(dl)
  firstTwoDigitFrequency <- summary(b$firstTwoDigit)
  #Insert 0's for missing digits
  for (i in 10:99) {
    if (!(i %in% names(firstTwoDigitFrequency))) {
      firstTwoDigitFrequency <- append(firstTwoDigitFrequency, 0, after=i-1)
    }
  }
  names(firstTwoDigitFrequency) <- c(10:99)
  firstTwoDigitObserved <- firstTwoDigitFrequency/rowCount
  
  #Z-scores for each digit test
  firstTwoDigitZN <- ifelse (1/(2*rowCount) < abs(firstTwoDigitObserved-firstTwoDigitBenford),
                             abs(firstTwoDigitObserved-firstTwoDigitBenford) - 1/(2*rowCount),
                             abs(firstTwoDigitObserved-firstTwoDigitBenford))
  firstTwoDigitZD <- sqrt((firstTwoDigitBenford*(1-firstTwoDigitBenford))/rowCount)
  firstTwoDigitZ <- firstTwoDigitZN/firstTwoDigitZD
  
  #Z Critical value at 95% confidence
  zCrit <- 1.96
  
  #Test for significance: if higher than critical value then abnormal
  firstTwoDigitTests <- abs(firstTwoDigitZ) > zCrit
  
  #Combine the expected vs observed frequencies and ratios of each digit and calculate MAD values
  firstTwoDigitObserved <- firstTwoDigitFrequency
  firstTwoDigitExpected <- firstTwoDigitBenford*rowCount
  ftindexValue <- as.factor(c(10:99))
  firstTwoDigitResults <- data.frame(ftindexValue, firstTwoDigitObserved, firstTwoDigitExpected, firstTwoDigitTests)
  colnames(firstTwoDigitResults) <- c('First_Two_Digits', 'Observed_Frequency', 'Expected_Frequency', 'Abnormal_Flag')
  firstTwoDigitResults$Perc_Abnormal <- nrow(firstTwoDigitResults[firstTwoDigitResults$Abnormal_Flag==T,])/nrow(firstTwoDigitResults)
  firstTwoDigitResults$Observed_Ratio <- firstTwoDigitObserved
  firstTwoDigitResults$Expected_Ratio <- firstTwoDigitBenford
  f2dMAD <- sum(abs(firstTwoDigitResults$Observed_Ratio - firstTwoDigitResults$Expected_Ratio))/nrow(firstTwoDigitResults)
  firstTwoDigitResults$MAD_Value <- f2dMAD
  firstTwoDigitResults$MAD_Conformity <- "Close Conformity"
  if (f2dMAD <= 0.004) {
    firstTwoDigitResults$MAD_Conformity <- "Close Conformity"
  } else if (f2dMAD > 0.004 & f2dMAD <= 0.008) {
    firstTwoDigitResults$MAD_Conformity <- "Acceptable Conformity"
  } else if (f2dMAD > 0.008 & f2dMAD <= 0.012) {
    firstTwoDigitResults$MAD_Conformity <- "Marginally Acceptable Conformity"
  } else if (f2dMAD > 0.012) {
    firstTwoDigitResults$MAD_Conformity <- "Non-Conformity"
  }
  
  if (returnValue=='list') {
    resultsList <- list(MAD_CONFORMITY=unique(firstTwoDigitResults$MAD_Conformity),
                        MAD_VALUE=unique(firstTwoDigitResults$MAD_Value),
                        PERC_ABNORMAL=unique(firstTwoDigitResults$Perc_Abnormal),
                        FIRST_DIGITS=firstTwoDigitResults$First_Two_Digits,
                        OBSERVED_RATIO=firstTwoDigitResults$Observed_Ratio,
                        EXPECTED_RATIO=firstTwoDigitResults$Expected_Ratio,
                        OBSERVED_FREQUENCY=firstTwoDigitResults$Observed_Frequency,
                        EXPECTED_FREQUENCY=firstTwoDigitResults$Expected_Frequency,
                        ABNORMAL_FLAG=firstTwoDigitResults$Abnormal_Flag)
    return(resultsList)
  } else {
    return(firstTwoDigitResults)
  }
}