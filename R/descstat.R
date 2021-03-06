#' Generates and displays both graphical summaries and numerical summaries for numeric data
#' @export
#' @param x is the vector containing the random sample to be used


descstat <- function(x)
{


  # Graphical summaries
  par(mfrow=c(2,2))


  # Horizontal boxplot
  boxplot(x, horizontal = TRUE)


  # Vertical boxplot
  boxplot(x, horizontal = FALSE)


  # Histogram
  hist(x, col=rainbow(9), prob=T)
  lines(density(x), lwd=3, col='red')
  rug(x)


  # QQ plot
  qqnorm(x); qqline(x, col = 2)
  
  par(mfrow=c(1,1))
  
  # Stem and leaf plot
  stem(x)
  
  # Test of normality
  shapiro.test(x)
}
