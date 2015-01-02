saveHTML({ 
	
# POWER INCREASES AS THE EFFECT SIZE, THE DIFFERENCE BETWEEN THE NULL HYPOTHESIS MEAN AND THE TRUE MEAN,  INCREASES

 N = 15; meanh0 = 100; sdh0 = 15;
oopt = ani.options(interval = 1, effectmax = 15)
## use a loop to create images one by one
for (effect in 1: ani.options("effectmax")) {

xv<-seq(meanh0 - 20*sdh0/sqrt(N),meanh0 + 20*sdh0/sqrt(N),0.01)  # We create a vector of x values over the range of values
yv<-dnorm(xv,0,1)  # We create the corresponding vector of normal densities
curve(dnorm(x, meanh0, sdh0/sqrt(N)), from =  meanh0 - 10*sdh0/sqrt(N), to = meanh0 + 10*sdh0/sqrt(N), 0.01, n = 200, ylim = c(0,.2));  # We draw the normal curve

power = round( pnorm(meanh0 - 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)) + 1 - pnorm(meanh0 + 1.96*sdh0/sqrt(N), meanh0 + effect, sdh0/sqrt(N)),3)
text(meanh0 + effect + 3*sdh0/sqrt(N), .13, paste("effect = ", as.character(effect)), col = "blue", font = 2)
text(meanh0 + effect + 3*sdh0/sqrt(N), .12, paste("Power = ", as.character(power)), col = "blue", font = 2)

text(100, .20, "Notice how the two sampling distributions ", col = "blue", font = 2)
text(100, .19,  "of the mean pull apart as the effect size increases,", col = "blue", font = 2)
text(100, .18, "making it increasingly likely that a sample mean ", col = "blue", font = 2)
text(100, .17, "will fall in the rejection region. They pull apart because as", col = "blue", font = 2)  
text(100, .16, "the effect size gets bigger, the means are farther apart.", col = "blue", font = 2)  

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
	
 }, img.name = "Power_EffectSize", imgdir = "Power_EffectSize", htmlfile = "Power.EffectSize.html", 
    autobrowse = FALSE, title = "Power as a function of Effect Size", 
  description = "Power increases as Effect Size increases")
