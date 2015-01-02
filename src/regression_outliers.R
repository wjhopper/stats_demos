library(animation)
saveHTML({ 
	# The effect of an outlier on the regression line when the outlier is far from the mean.
n = 50
x = rnorm(n, 100, 15)
y = 3*x + 5 + rnorm(n, 0, 1)
outx = 130
plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
abline(lm(y~x), col = "red")
ani.options(interval = 1, outlier.slope.max = 20)
## use a loop to create images one by one
for (outlier.slope in 3:ani.options("outlier.slope.max")) {
ol.slope = 2.8 + outlier.slope/5	
outy =  ol.slope*outx + 5
xplot = c(x, outx)
yplot = c(y, outy)
text(105,600, "Notice how the one outlier")
text(105,580, "pulls the regression line away from")
text(105,560, "the rest of the data as it moves")
text(105,540, "away.")
plot(x,y,  xlim = c(min(xplot), max(xplot)),ylim = c(100,900))
abline(lm(y~x), col = "red")
text(105,600, "Notice how the one outlier")
text(105,580, "pulls the regression line away from")
text(105,560, "the rest of the data as it moves")
text(105,540, "away.")
points(xplot,yplot, ylim = c(100,900), xlim = c(min(xplot), max(xplot)))
abline(lm(yplot ~ xplot))	
ani.pause(1)
}

# The effect of an outlier on the regression line when the outlier is not far from the mean.

x = rnorm(n, 100, 15)
y = 3*x + 5 + rnorm(n, 0, 1)
outx = 101
plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
abline(lm(y~x), col = "red")
oopt = ani.options(interval = 1, outlier.slope.max = 20)
## use a loop to create images one by one
for (outlier.slope in 3:ani.options("outlier.slope.max")) {
ol.slope = 2.8 + outlier.slope/5	
outy =  ol.slope*outx + 5
xplot = c(x, outx)
yplot = c(y, outy)
text(5 ,800, "Notice how the outlier does not")
text(95,780, "pull the regression line away from")
text(95,760, "the rest of the data as much when")
text(95,740, "it moves away.")
plot(x,y,  xlim = c(min(xplot), max(xplot)),ylim = c(100,900))
abline(lm(y~x), col = "red")
text(95,800, "Notice how the outlier does not")
text(95,780, "pull the regression line away from")
text(95,760, "the rest of the data as much when")
text(95,740, "it moves away.")
points(xplot,yplot, ylim = c(100,900), xlim = c(min(xplot), max(xplot)))
abline(lm(yplot ~ xplot))	
ani.pause(1)
}
	
 }, img.name = "Regression_outliers", imgdir = "Regression_outliers", htmlfile = "Regression_outliers.html", 
    autobrowse = FALSE, title = "Effect_of_outliers", 
  description = "The effect of outliers on the regression line")