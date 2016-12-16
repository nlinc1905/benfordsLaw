#' Plot of Benford's First Digit Test
#' 
#' This function plots Benford's first digit test results as expected vs observed
#' digit frequencies.
#' @param firstDigitdf The results of the \code{benfordFirstDigit} function in dataframe format.
#' @return Returns a plot
#' @export
#' @examples
#' plotFD(benfordFirstDigit(iris$Sepal.Width))

plotFD <- function(firstDigitdf) {
  require(ggplot2)
  fdPlot <- ggplot(firstDigitdf) +
    geom_bar(aes(x=First_Digit, y=Observed_Frequency), width=0.5, stat="identity", fill="slategray") +
    geom_line(aes(x=as.numeric(First_Digit), y=Expected_Frequency, color="Expected"), size=1.5) +
    scale_colour_manual("", breaks=c("Expected"), values=c("Expected"="indianred")) +
    ggtitle("First Digit Observed VS Expected") +
    labs(x="Digit", y="Frequency") +
    scale_y_continuous(expand=c(0.02, 0)) +
    theme(text=element_text(size=20), plot.title=element_text(color="#666666", face="bold", size=20),
          axis.title=element_text(color="#666666", face="bold", size=16))
  return(fdPlot)
}