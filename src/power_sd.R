saveHTML({ 
	
# POWER INCREASES AS SIGMA, THE POPULATION STANDARD DEVIATION DECREASES
 N = 15
ani.options(interval = 1, nmax = 15)
## use a loop to create images one by one
for (sdh0 in ani.options("nmax"):3) {

meanh0 = 100; effect = 3

xv<-seq(meanh0 - 20*sdh0/sqrt(N),meanh0 + 20*sdh0/sqrt(N),0.01)  # We create a vector of x values over the range of values
yv<-dnorm(xv,0,1)  # We create the corresponding vector of normal densities
curve(dnorm(x, meanh0, sdh0/sqrt(N)), from =  meanh0 - 10*sdh0/sqrt(N), to = meanh0 + 10*sdh0/sqrt(N), 0.01, n = 200, ylim = c(0,.6));  # We draw the normal curve

power = round( pnorm(meanh0 - 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)) + 1 - pnorm(meanh0 + 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)),3)
text(meanh0 + effect + 5*sdh0/sqrt(N), .15, paste("sigma = ", as.character(sdh0)))
text(meanh0 + effect + 5*sdh0/sqrt(N), .12, paste("Power = ", as.character(power)))

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
ani.pause(1)
}
	
}, img.name = "Power_sigma", imgdir = "Power_sigma", htmlfile = "Power.sigma.html", 
   autobrowse = FALSE, title = "Power as a function of sigma", description = "Power increases as population standaard deviation decreases")
