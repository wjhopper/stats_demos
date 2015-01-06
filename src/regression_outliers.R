regression_outliers <- function(n = 51, mu = 100, sigma = 15, interval = 2.5, frames =20) { 
  library(ggplot2)
  library(gridExtra)
  ani.options(interval = interval, nmax = frames)

  #1 The effect of an outlier on the regression line when the outlier is far from the mean.
  x = rnorm(n, mu, sigma)
  y = .66*x + 5 + rnorm(n,0,5)
  df <- data.frame(xv = x, yv = y)
  max_x <- max(df$xv)
  median_x <- median(df$x)
  max_y <- max(df$yv) + frames
  
    
#   plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
#   abline(lm(y~x), col = "red")
  
  for (residual in seq(2,2+frames)) {
      mod_data <- df
      mod_data_median <- df
      mod_data[mod_data$xv ==max_x,'yv'] <- mod_data[mod_data$xv ==max_x,'yv'] + residual
#       outlier = mod_data[mod_data$xv==max_x,]
      mod_data_median[mod_data_median$xv ==median_x,'yv'] <- mod_data_median[mod_data_median$xv ==median_x,'yv'] + residual
      lm.normal <- lm(yv~xv,data=df)
      lm.far <- lm(yv~xv,data=mod_data)
      lm.near <- lm(yv~xv,data=mod_data_median)
      delta_far <- lm.far$coefficients["xv"] - lm.normal$coefficients["xv"]
      delta_near <- lm.near$coefficients["xv"] - lm.normal$coefficients["xv"]

      p <- ggplot(data=df, mapping = aes(x=xv,y=yv)) + 
        geom_point(size=4) +
        geom_smooth(method="lm", se=F,color='black',size = 1.25) + 
          scale_x_continuous("Numeric Depedent Variable  #1") +
          scale_y_continuous("Numeric Depedent Variable  #2", limits=c(min(df$yv), max_y)) +
          theme_bw() + 
          theme(plot.title = element_text(size=18),
                panel.grid.major = element_blank())

      p1 <- p + geom_smooth(data=mod_data,mapping=aes(x=xv,y=yv),
                            method="lm",se=F,color='red',size = 1.25) +
            geom_point(data= mod_data[mod_data$xv==max_x,],
                       mapping=aes(x=xv, y=yv),color='red',size=4) +
            geom_text(data = data.frame(x=mean(mod_data$xv), y  =100,d = round(delta_far,3)),
                      mapping=aes(label = paste("Change in Slope =",d),x=x,y=y), size=6) + 
            ggtitle("Outlier far from Mean of X")  

      p2 <- p + geom_smooth(data=mod_data_median,mapping=aes(x=xv,y=yv),
                         method="lm",se=F,color='red',size = 1.25) +
            geom_point(data= mod_data_median[mod_data_median$xv==median_x,],
                       mapping=aes(x=xv, y=yv),color='red',size=4) + 
            geom_text(data = data.frame(x=mean(mod_data_median$xv), y  =100, d = round(delta_near,3)),
                      mapping=aes(label = paste("Change in Slope =",d),x=x,y=y), size=6) + 
            theme(axis.title.y= element_blank()) + 
            ggtitle("Outlier near Mean of X")

      grid.arrange(p1,p2,ncol=2,nrow=1)
  }

#     ol.slope = 2.8 + outlier.slope/5  
#     outy =  ol.slope*outx + 5
#     xplot = c(x, outx)
#     yplot = c(y, outy)
#     plot(x,y,  xlim = c(min(xplot), max(xplot)),ylim = c(100,900))
#     abline(lm(y~x), col = "red")
#     points(xplot,yplot, ylim = c(100,900), xlim = c(min(xplot), max(xplot)))
#     abline(lm(yplot ~ xplot))	
#   }
#   
#   #2 The effect of an outlier on the regression line when the outlier is not far from the mean.
#   x = rnorm(n, mu, sigma)
#   y = 3*x + 5 + rnorm(n)
#   outx = 101
#   plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
#   abline(lm(y~x), col = "red")
#   
#   for (outlier.slope in 3:ani.options("outlier.slope.max")) {
#     ol.slope = 2.8 + outlier.slope/5	
#     outy =  ol.slope*outx + 5
#     xplot = c(x, outx)
#     yplot = c(y, outy)
#     plot(x,y,  xlim = c(min(xplot), max(xplot)),ylim = c(100,900))
#     abline(lm(y~x), col = "red")
#     points(xplot,yplot, ylim = c(100,900), xlim = c(min(xplot), max(xplot)))
#     abline(lm(yplot ~ xplot))	
}