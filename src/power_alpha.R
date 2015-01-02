power_alpha <- function(meanh0 = 100, effect =3, N = 15, sdh0 = 15, interval = 2.5, frames = 10) {
# POWER GOES DOWN AS ALPHA, THE SIGNIFICANCE LEVEL, DECREASES


  oopt = ani.options(interval = interval, nmax = frames)
  ## use a loop to create images one by one
  for (alpha in ani.options("nmax"):1) {
    alpha = alpha/100;
    cutoff = qnorm(1-alpha)
    xv<-seq(meanh0 - 20*sdh0/sqrt(N),meanh0 + 20*sdh0/sqrt(N),0.01)  # We create a vector of x values over the range of values
    yv<-dnorm(xv,0,1)  # We create the corresponding vector of normal densities
    curve(dnorm(x, meanh0, sdh0/sqrt(N)), from =  meanh0 - 10*sdh0/sqrt(N), to = meanh0 + 10*sdh0/sqrt(N), 0.01, n = 200, ylim = c(0,.15));  # We draw the normal curve
    
    text(100, .15, "As alpha decreases the cutoffs", col = "blue", font = 2)
    text(100, .14,  "move outward so that power,", col = "blue", font = 2)
    text(100, .13, "also decreases because the ", col = "blue", font = 2)
    text(100, .12, "rejection region moves away from", col = "blue", font = 2) 
    text(100, .11, "the true mean", col = "blue", font = 2) 
    
    power = round( pnorm(meanh0 - cutoff*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)) + 1 - pnorm(meanh0 + cutoff*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)),3)
    text(meanh0 + effect + 5*sdh0/sqrt(N), .10, paste("alpha = ", as.character(alpha)))
    text(meanh0 + effect + 5*sdh0/sqrt(N), .08, paste("Power = ", as.character(power)))
    
    polygon(      		# We draw the left tail alpha/2
    c(meanh0 - 6*sdh0/sqrt(N), 
    xv[(xv<=meanh0 - cutoff*sdh0/sqrt(N))&(xv>=meanh0 - 6*sdh0/sqrt(N))],  
    meanh0 - cutoff*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv<=meanh0 - cutoff*sdh0/sqrt(N))&(xv>=meanh0 - 6*sdh0/sqrt(N))] ,meanh0,sdh0/sqrt(N)),  0) , col="red")  
    polygon(			# We draw the right tail alpha/2
    c(meanh0 + cutoff*sdh0/sqrt(N), 
    xv[(xv>=meanh0 + cutoff*sdh0/sqrt(N))&(xv<=meanh0 + 6*sdh0/sqrt(N))],  
    meanh0 + 6*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv>=meanh0 + cutoff*sdh0/sqrt(N))&(xv<=meanh0 + 6*sdh0/sqrt(N))] ,meanh0,sdh0/sqrt(N)),  0) , col="red")  
    
    polygon(			# We draw the left tail power
    c(meanh0 - 6*sdh0/sqrt(N), 
    xv[(xv>=meanh0 - 6*sdh0/sqrt(N))&(xv<=meanh0 - cutoff*sdh0/sqrt(N))],  
    meanh0 - cutoff*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv>=meanh0 - 6*sdh0/sqrt(N))&(xv<=meanh0 - cutoff*sdh0/sqrt(N))] ,meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,0,1,.5))  
    
    polygon(			# We draw the right tail power
    c(meanh0 + cutoff*sdh0/sqrt(N), 
    xv[(xv>=meanh0 + cutoff*sdh0/sqrt(N))&(xv<=meanh0 + effect + 20*sdh0/sqrt(N))],  
    meanh0+ effect + 20*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv>=meanh0 + cutoff*sdh0/sqrt(N))&(xv<=meanh0 + effect + 20*sdh0/sqrt(N))], meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,0,1,.5))  
    
    polygon(			# We draw the beta
    c(meanh0 - cutoff*sdh0/sqrt(N), 
    xv[(xv<=meanh0 + cutoff*sdh0/sqrt(N))&(xv>=meanh0  - cutoff*sdh0/sqrt(N))],  
    meanh0+ cutoff*sdh0/sqrt(N) ), 
    c(0,  dnorm(xv[(xv<=meanh0 + cutoff*sdh0/sqrt(N))&(xv>=meanh0  - cutoff*sdh0/sqrt(N))], meanh0+ effect,sdh0/sqrt(N)),  0) , col=rgb(0,1,0,.5))  
  }		
  	
   }, img.name = "Power_alpha", imgdir = "Power_alpha", htmlfile = "Power.alpha.html", 
       autobrowse = FALSE, title = "Power as a function of alpha", 
      description = "Power decreases as alpha decreases")
}