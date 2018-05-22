#' Plot of Benford's Last Two Digit Test
#' 
#' This function plots Benford's last two digit test results as expected vs observed
#' digit frequencies.
#' @param lastTwoDigitdf The results of the \code{benfordLastTwoDigit} function in dataframe format.
#' @return Returns a plot
#' @export
#' @examples
#' plotL2D(benfordLastTwoDigit(iris$Sepal.Width))

plotL2D <- function(lastTwoDigitdf) {
  require(ggplot2)
  l2dPlot <- ggplot(lastTwoDigitdf) +
    geom_bar(aes(x=Last_Two_Digits, y=Observed_Frequency), width=0.5, stat="identity", fill="slategray") +
    geom_line(aes(x=as.numeric(Last_Two_Digits), y=Expected_Frequency, color="Expected"), size=1.5) +
    scale_colour_manual("", breaks=c("Expected"), values=c("Expected"="indianred")) +
    ggtitle("Last Two Digits Observed VS Expected") +
    labs(x="Digit", y="Frequency") +
    scale_y_continuous(expand=c(0.02, 0)) +
    theme(text=element_text(size=20), plot.title=element_text(color="#666666", face="bold", size=20),
          axis.title=element_text(color="#666666", face="bold", size=16))
  return(l2dPlot)
}