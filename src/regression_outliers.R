regression_outliers <- function(n = 50, mu = 100, sigma = 15, interval = .05, frames =20) { 
  library(ggplot2)
  library(gridExtra)
  ani.options(interval = interval, nmax = frames + 1)
  offset = 1 

  n = round(n)
#   if (n %% 2 != 0) {
#     stop("Number of observations must be an even integer")
#   }
  
  #1 The effect of an outlier on the regression line when the outlier is far from the mean.
  x = rnorm(n, mu, sigma)
  y = .4*x + 5 + rnorm(n,0,5)
  # not really random, but make sure most extreme x has most extreme y =)
  max_x <- max(x)
  y[x==max_x] <- max_y <- max(y) + offset

  # find the closest point to the mean of x
  middle_x_pos <- which.min(abs(x-mean(x)))


  df <- data.frame(xv = x, yv = y)  
  # copy this df for modification later 
  m_df_mid <- df
  m_df <- df

  lm.normal <- lm(yv~xv,data=df)

  # Get some good lims
  min_ylim <- min(y) - (ani.options("nmax") - (abs((min(y) - y[middle_x_pos]))))
  max_ylim <- max(y) + ani.options("nmax")
  min_xlim <- min(x) - 5
  max_xlim <- max(x)+ 5

  for (residual in seq(offset-1,offset-1+frames)) {

    m_df[m_df$xv == max_x,'yv'] <- df[df$xv == max_x,'yv'] + abs(residual)
    # always move the middle point in the direction of the sign of the residual
    # check not neccesary for extreme point, as max(y) residual will always be pos. 
    if (lm.normal$residuals[middle_x_pos] < 0) {
      residual = -residual
    }
    
    m_df_mid[middle_x_pos,'yv'] <- df[middle_x_pos,'yv'] + residual
    lm.far <- lm(yv~xv,data=m_df)
    lm.near <- lm(yv~xv,data=m_df_mid)
    delta_far <- lm.far$coefficients["xv"] - lm.normal$coefficients["xv"]
    delta_near <- lm.near$coefficients["xv"] - lm.normal$coefficients["xv"]

    p <- ggplot(data=df, mapping = aes(x=xv,y=yv)) + 
      geom_point(size=4) +
      geom_smooth(method="lm", se=F,color='black',size = 1.25) + 
      scale_x_continuous("Numeric Depedent Variable  #1", limits=c(min_xlim, max_xlim)) +
      scale_y_continuous("Numeric Depedent Variable  #2", limits=c(min_ylim, max_ylim)) +
      theme_bw() + 
      theme(plot.title = element_text(size=18),
            panel.grid.major = element_blank())

    p1 <- p + geom_smooth(data=m_df,mapping=aes(x=xv,y=yv),
                          method="lm",se=F,color='red',size = 1.25) +
          geom_point(data= m_df[m_df$xv==max_x,],
                     mapping=aes(x=xv, y=yv),color='red',size=4) +
          geom_text(data = data.frame(x=mean(m_df$xv), y  = max_ylim -5  , d = round(delta_far,3)),
                    mapping=aes(label = paste("Change in Slope =",d),x=x,y=y), size=6) + 
          ggtitle("Outlier far from Mean of X")  
    p2 <- p + geom_smooth(data=m_df_mid,mapping=aes(x=xv,y=yv),
                          method="lm",se=F,color='red',size = 1.25) +
          geom_point(data= m_df_mid[middle_x_pos,],
                     mapping=aes(x=xv, y=yv),color='red',size=4) + 
          geom_text(data = data.frame(x=mean(m_df_mid$xv), y  = max_ylim -5, d = round(delta_near,3)),
                    mapping=aes(label = paste("Change in Slope =",d),x=x,y=y), size=6) + 
          theme(axis.title.y= element_blank()) + 
          ggtitle("Outlier near Mean of X")

    grid.arrange(p1,p2,ncol=2,nrow=1)
  }

#     plot(x,y,  xlim =c(70, 140),ylim = c(100,900), type = "n")
#     abline(lm(y~x), col = "red")
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