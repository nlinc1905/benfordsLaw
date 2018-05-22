# benfordsLaw

An R package to apply Benford's first digit, first two digit, and last two digit tests.  To install, ensure that the dependency ggplot2 library is installed first, or run the commands below.

```R
install.packages(c('ggplot2', 'devtools')) #Installs dependencies
library(devtools)
setwd("C:/Users/User/Desktop") #Assumes package is unzipped to desktop
install("benfordsLaw")
library(benfordsLaw)
```

The package has 6 functions: one for Benford's first digit test, another for Benford's first two digit test, another for Benford's last two digit test, and a plotting function for each test.

```R
?benfordFirstDigit
?benfordFirstTwoDigit
?benfordLastTwoDigit
?plotFD
?plotF2D
?plotL2D
```