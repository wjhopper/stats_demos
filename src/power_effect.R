power_effect <- function(meanh0 = 100,  effect = 3, sdh0 = 15, N =15, interval=2.5, frames = 15){ 
  
  # POWER INCREASES AS THE EFFECT SIZE, THE DIFFERENCE BETWEEN THE NULL HYPOTHESIS MEAN AND THE TRUE MEAN,  INCREASES

  oopt =  ani.options()
  ani.options(interval = interval, effectmax = frames)
  
  ## use a loop to create images one by one
  for (effect in 1: ani.options("effectmax")) {
    
    xv<-seq(meanh0 - 20*sdh0/sqrt(N),meanh0 + 20*sdh0/sqrt(N),0.01)  # We create a vector of x values over the range of values
    yv<-dnorm(xv,0,1)  # We create the corresponding vector of normal densities
    curve(dnorm(x, meanh0, sdh0/sqrt(N)), from =  meanh0 - 10*sdh0/sqrt(N), to = meanh0 + 10*sdh0/sqrt(N), 0.01, n = 200, ylim = c(0,.2));  # We draw the normal curve
    
    power = round( pnorm(meanh0 - 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)) + 1 - pnorm(meanh0 + 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)),3)
    text(meanh0 + effect + 3*sdh0/sqrt(N), .13, paste("effect = ", as.character(effect)), col = "blue", font = 2)
    text(meanh0 + effect + 3*sdh0/sqrt(N), .12, paste("Power = ", as.character(power)), col = "blue", font = 2)
        
    polygon(      		# We draw the left tail alpha/2
    c(meanh0 - 6*sdh0/sqrt(N), 
    xv[(xv<=meanh0 - 1.96*sdh0/sqrt(N))&(xv>=meanh0 - 6*sdh0/sqrt(N))],  
    meanh0 - 1.96*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv<=meanh0 - 1.96*sdh0/sqrt(N))&(xv>=meanh0 - 6*sdh0/sqrt(N))] ,meanh0,sdh0/sqrt(N)),  0) , col="red")  
    polygon(			# We draw the right tail alpha/2
    c(meanh0 + 1.96*sdh0/sqrt(N), 
    xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + 6*sdh0/sqrt(N))],  
    meanh0 + 6*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + 6*sdh0/sqrt(N))] ,meanh0,sdh0/sqrt(N)),  0) , col="red")  
    
    polygon(			# We draw the left tail power
    c(meanh0 - 6*sdh0/sqrt(N), 
    xv[(xv>=meanh0 - 6*sdh0/sqrt(N))&(xv<=meanh0 - 1.96*sdh0/sqrt(N))],  
    meanh0 - 1.96*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv>=meanh0 - 6*sdh0/sqrt(N))&(xv<=meanh0 - 1.96*sdh0/sqrt(N))] ,meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,0,1,.5))  
    
    polygon(			# We draw the right tail power
    c(meanh0 + 1.96*sdh0/sqrt(N), 
    xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + effect + 20*sdh0/sqrt(N))],  
    meanh0+ effect + 20*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv>=meanh0 + 1.96*sdh0/sqrt(N))&(xv<=meanh0 + effect + 20*sdh0/sqrt(N))], meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,0,1,.5))  
    
    polygon(			# We draw the beta
    c(meanh0 - 1.96*sdh0/sqrt(N), 
    xv[(xv<=meanh0 + 1.96*sdh0/sqrt(N))&(xv>=meanh0  - 1.96*sdh0/sqrt(N))],  
    meanh0+ 1.96*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv<=meanh0 + 1.96*sdh0/sqrt(N))&(xv>=meanh0  - 1.96*sdh0/sqrt(N))], meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,1,0,.5))  

  }	
  	
}
