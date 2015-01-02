regression_outliers <- function(n = 50, mu = 100, sigma = 15, interval = 2.5, frames =20) { 

  ani.options(interval = interval, outlier.slope.max = frames)

  #1 The effect of an outlier on the regression line when the outlier is far from the mean.
  x = rnorm(n, mu, sigma)
  y = 3*x + 5 + rnorm(n)
  outx = 130
  plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
  abline(lm(y~x), col = "red")
  
  for (outlier.slope in 3:ani.options("outlier.slope.max")) {
    ol.slope = 2.8 + outlier.slope/5	
    outy =  ol.slope*outx + 5
    xplot = c(x, outx)
    yplot = c(y, outy)
    plot(x,y,  xlim = c(min(xplot), max(xplot)),ylim = c(100,900))
    abline(lm(y~x), col = "red")
    points(xplot,yplot, ylim = c(100,900), xlim = c(min(xplot), max(xplot)))
    abline(lm(yplot ~ xplot))	
  }
  
  #2 The effect of an outlier on the regression line when the outlier is not far from the mean.
  x = rnorm(n, mu, sigma)
  y = 3*x + 5 + rnorm(n)
  outx = 101
  plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
  abline(lm(y~x), col = "red")

  for (outlier.slope in 3:ani.options("outlier.slope.max")) {
    ol.slope = 2.8 + outlier.slope/5	
    outy =  ol.slope*outx + 5
    xplot = c(x, outx)
    yplot = c(y, outy)
    plot(x,y,  xlim = c(min(xplot), max(xplot)),ylim = c(100,900))
    abline(lm(y~x), col = "red")
    points(xplot,yplot, ylim = c(100,900), xlim = c(min(xplot), max(xplot)))
    abline(lm(yplot ~ xplot))	
  }
	
}