#' Plot of Benford's First Two Digit Test
#' 
#' This function plots Benford's first two digit test results as expected vs observed
#' digit frequencies.
#' @param firstTwoDigitdf The results of the \code{benfordFirstTwoDigit} function in dataframe format.
#' @return Returns a plot
#' @export
#' @examples
#' plotFD(benfordFirstTwoDigit(iris$Sepal.Width))

plotF2D <- function(firstTwoDigitdf) {
  require(ggplot2)
  f2dPlot <- ggplot(firstTwoDigitdf) +
    geom_bar(aes(x=First_Two_Digits, y=Observed_Frequency), width=0.5, stat="identity", fill="slategray") +
    geom_line(aes(x=as.numeric(First_Two_Digits), y=Expected_Frequency, color="Expected"), size=1.5) +
    scale_colour_manual("", breaks=c("Expected"), values=c("Expected"="indianred")) +
    ggtitle("First Two Digits Observed VS Expected") +
    labs(x="Digit", y="Frequency") +
    scale_y_continuous(expand=c(0.02, 0)) +
    theme(text=element_text(size=20), plot.title=element_text(color="#666666", face="bold", size=20),
          axis.title=element_text(color="#666666", face="bold", size=16))
  return(f2dPlot)
}