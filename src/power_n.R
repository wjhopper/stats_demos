power_n <- function(meanh0 = 100,  effect = 3, sdh0 = 15, interval=2.5, frames = 30){ 
	
  library(ggplot2)
  library(animation)
	# POWER INCREASES AS N, THE SAMPLE SIZE, INCREASES
  
  opts <- ani.options()
  ani.options(interval = interval, nmax = frames)
  meanh1 <- meanh0+effect
  alpha <- .05
  
  ## use a loop to create images one by one
  for (N in 1:ani.options("nmax")) {
    N=N*5
    se <- sdh0/sqrt(N)
    cutoff = qnorm(1-alpha,meanh0,se)
    power <- (power.t.test(n=N,delta = effect, sd = sdh0,sig.level = alpha, 
                           type = "one.sample", alternative = "one.sided")$power)
    Beta <- round((1-power),2)
    
    # We create a vector of x values over the range of values, stick in data frame
    df <- data.frame(xv = seq(meanh0 - 4*se, meanh1 + 4*se, 0.01))
    df$yv_null <-dnorm(df$xv,meanh0,se)
    df$yv_alt <-dnorm(df$xv,meanh1,se)
    
    p1 <- ggplot(data =df, aes(x=xv)) + 
      geom_line(aes(y=yv_null),size=1) +
      geom_line(aes(y=yv_alt),size=1) +
      # Beta 
      geom_polygon(data = data.frame(x = c(min(df$xv), df[df$xv <= cutoff,'xv'], cutoff),
                                     y = c(0, df[df$xv <= cutoff,'yv_alt'], 0)),
                   aes(x=x,y=y), fill='blue',color='black',alpha = .5) + 
      # power
      geom_polygon(data = data.frame(x = c(cutoff, df[df$xv >= cutoff,'xv'], max(df$xv)),
                                     y = c(0, df[df$xv >= cutoff,'yv_alt'], 0)),
                   aes(x=x,y=y), fill='green',color='black',alpha = .5) + 
      # alpha
      geom_polygon(data = data.frame(x = c(cutoff, df[df$xv >= cutoff,'xv'], max(df$xv)),
                                     y = c(0, df[df$xv >= cutoff,'yv_null'], 0)),
                   aes(x=x,y=y), fill='red',color='black',alpha=.6) + 
      geom_text(data=data.frame( x = cutoff+2, y = .101*max(df$yv_null), alpha = alpha), 
                mapping = aes(label = paste("alpha ==",alpha), x=x, y=y),
                parse=TRUE, size = 3) + 
      geom_text(data=data.frame( x = cutoff-2, y = .1*max(df$yv_null), Beta = Beta), 
                mapping = aes(label = paste("beta ==",Beta), x=x, y=y),
                parse=TRUE, size = 3) +
      geom_text(data=data.frame( x =max(meanh0,cutoff+3), y = .5*max(df$yv_null), power=power), 
                mapping = aes(label = paste("power ==",round(power,2)), x=x, y=y),
                parse=TRUE, size = 3) + 
      geom_text(data=data.frame( x = meanh0, y = max(df$yv_null)+.01),
                mapping = aes(label = "H[0]", x=x, y=y), parse=TRUE, size = 4) + 
      geom_text(data=data.frame( x = meanh1, y = max(df$yv_alt)+.01),
                mapping = aes(label = "H[1]", x=x, y=y), parse=TRUE, size = 4) + 
      scale_x_continuous("Numeric Dependent Variable Values", limits=c(80,120)) + 
      scale_y_continuous("Normal Distribution") +
      ggtitle("Power as a function of sample size") + theme_bw() +
      theme(plot.title = element_text(size=10),
            panel.grid.major = element_blank(),
            axis.title.y = element_text(size=8),
            axis.title.x = element_text(size=8),
            plot.margin = unit(c(0,.25,.25,.25),units="lines"))
    print(p1)
    
  }
  #     xv<-seq(meanh0 - 20*sdh0/sqrt(N),meanh0 + 20*sdh0/sqrt(N),0.01)  # We create a vector of x values over the range of values
  #     yv<-dnorm(xv,0,1)  # We create the corresponding vector of normal densities
  #     curve(dnorm(x, meanh0, sdh0/sqrt(N)), from =  meanh0 - 10*sdh0/sqrt(N), to = meanh0 + 10*sdh0/sqrt(N), 0.01, n = 200, ylim = c(0,.4), xlim=c(80,120));  # We draw the normal curve
  #     
  #     #power = round( pnorm(meanh0 - 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)) + 1 - pnorm(meanh0 + 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)),3)
  #     text(115, .15, paste("N = ", as.character(N)))
  #     #text(meanh0 + effect + 5*sdh0/sqrt(N), .12, paste("Power = ", as.character(power)))
  #     
  #     polygon(        	# We draw the left tail alpha/2
  #     c(meanh0 - 6*sdh0/sqrt(N), 
  #     xv[(xv<=meanh0 - 1.96*sdh0/sqrt(N))&(xv>=meanh0 - 6*sdh0/sqrt(N))],  
  #     meanh0 - 1.96*sdh0/sqrt(N) ), 
  #     c(0,  dnorm(xv[(xv<=meanh0 - 1.96*sdh0/sqrt(N))&(xv>=meanh0 - 6*sdh0/sqrt(N))] ,meanh0,sdh0/sqrt(N)),  0) , col="red")  
  #     polygon(			# We draw the right tail alpha/2
  #     c(meanh0 + 1.96*sdh0/sqrt(N), 
  #     xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + 6*sdh0/sqrt(N))],  
  #     meanh0 + 6*sdh0/sqrt(N) ), 
  #     c(0,  dnorm(xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + 6*sdh0/sqrt(N))] ,meanh0,sdh0/sqrt(N)),  0) , col="red")  
  #     
  #     polygon(			# We draw the left tail power
  #     c(meanh0 - 6*sdh0/sqrt(N), 
  #     xv[(xv>=meanh0 - 6*sdh0/sqrt(N))&(xv<=meanh0 - 1.96*sdh0/sqrt(N))],  
  #     meanh0 - 1.96*sdh0/sqrt(N) ), 
  #     c(0,  dnorm(xv[(xv>=meanh0 - 6*sdh0/sqrt(N))&(xv<=meanh0 - 1.96*sdh0/sqrt(N))] ,meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,0,1,.5))  
  #     
  #     polygon(			# We draw the right tail power
  #     c(meanh0 + 1.96*sdh0/sqrt(N), 
  #     xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + effect + 20*sdh0/sqrt(N))],  
  #     meanh0+ effect + 20*sdh0/sqrt(N) ), 
  #     c(0,  dnorm(xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + effect + 20*sdh0/sqrt(N))], meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,0,1,.5))  
  #     
  #     polygon(			# We draw the beta
  #     c(meanh0 - 1.96*sdh0/sqrt(N), 
  #     xv[(xv<=meanh0 + 1.96*sdh0/sqrt(N))&(xv>=meanh0  - 1.96*sdh0/sqrt(N))],  
  #     meanh0+ 1.96*sdh0/sqrt(N) ), 
  #     c(0,  dnorm(xv[(xv<=meanh0 + 1.96*sdh0/sqrt(N))&(xv>=meanh0  - 1.96*sdh0/sqrt(N))], meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,1,0,.5))  
}
