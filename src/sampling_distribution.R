sampling_distribution <- function(samps = NULL, n = NULL, rate = NULL,interval =1) { 
# The sampling distribution of the sample mean for scores sampled from an exponential distribution as the number of samples increases
  library(dplyr)
  
  mu <- 1/rate
  sigma <- 1/rate
  #draw samples from exp
  samples <- as.data.frame(replicate(samps,rexp(n, rate)))
  # calculate sample means
  means <- data.frame(avg=colMeans(samples))
  
  # dummy plots just to get best looking max ylims before drawing the animations 
  dummy <-  hist(means$avg, breaks = 50,  plot=F)
  ymax_counts <- max(dummy$counts)
  ymax_freq <- max(dummy$density)

  ## use a loop to create images one by one

  for (N in 2:ani.options("nmax")) {
    if (N <= 10) {ani.pause(2)}
    if (N <= 0) {ani.pause(.5)}
    p1 <- ggplot(select(samples, N),aes_string(x = paste('V',N,sep=''))) + 
      geom_histogram(aes(y=..density..),fill="white",color="red",binwidth=5) + 
      stat_function(fun = dexp, args = list(rate=rate)) + 
      scale_y_continuous(limits=c(0,.045)) + 
      scale_x_continuous(limits=c(0,max(samples))) +
      ggtitle("Sample Distribution") +  
      xlab(paste("Sample #", N,sep='')) + 
      theme_bw() +
      theme(plot.title = element_text(size=10),
            panel.grid.major = element_blank(),
            axis.title.y = element_text(size=8),
            axis.title.x = element_text(size=8),
            plot.margin = unit(c(0,.25,.25,.25),units="lines"))
    
    p2 <- ggplot(data = data.frame(avg=means$avg[1:N]),mapping=aes(x=avg)) +
      geom_histogram(binwidth=.5,fill="#999999") + 
      geom_text(mapping = aes(label = paste("Number of samples drawn = ", N),
                              x= mu ,y=ymax_counts + 2.5), 
                data = data.frame(mu = mu, ymax_counts = ymax_counts,N = N ),
                size = 2.5, color = "red") +
      scale_y_continuous(limits=c(0,ymax_counts+5))  +
      scale_x_continuous("Sample Mean Value",limits=c(min(means$avg)-3,max(means$avg)+3)) +
      ggtitle("Distibution of Means from All Samples") + 
      theme_bw() + 
      theme(plot.title = element_text(size=10),
            panel.grid.major = element_blank(),
            axis.title.y = element_text(size=8),
            axis.title.x = element_text(size=8),
            plot.margin = unit(c(0,.25,.25,.25),units="lines"))
          
      # figure out x axis location of newest point!
      new_data <- ggplot_build(p2)$data[[1]]
      if (N >2) {
        new_item_loc <- new_data$count != old_data$count 
        x_loc <- new_data[new_item_loc,'x']
        y_loc <- new_data[new_item_loc,'y']
        new_rect_data <- data.frame(x1 = x_loc-.25, x2 = x_loc+.25, 
                                    y1 = y_loc-1, y2 = y_loc)
        p2 <- p2 + geom_rect(mapping=aes(x=NULL,xmin =x1,xmax=x2,ymin=y1,ymax=y2), 
                             data=new_rect_data,
                             color='red',fill='red')
      }
      
      d_est <- data.frame(density(means$avg[1:N], adjust=2)[c(1,2)])
      d_est$y <- ((d_est$y*N)*.5) #.5 is binwidth in histogram
      p2 <- p2 + geom_line(data = d_est, mapping = aes(x=x,y=y),
                           color="darkgreen",linetype = 2 )
      grid.arrange(p1,p2,nrow=1,ncol=2)
      old_data <- new_data
  }
  d_est <- data.frame(density(means$avg, adjust=2)[c(1,2)])
  p3 <- ggplot(data = means) +
    geom_histogram(aes(x=avg,y = ..density..),binwidth=.5,fill="#999999") + 
    stat_function(fun=dnorm, args = list(mean=mu,sd=sigma/sqrt(n)), color='red') + 
    geom_text(aes(label = paste("Number of samples drawn = ", N), 
                  x=mu,y=ymax_freq+.0075), 
              data = data.frame(mu = mu, ymax_freq=ymax_freq,N = N ),
              size = 2.5, color = "red") +
    geom_line(data = d_est, mapping = aes(x=x,y=y),
              color="darkgreen",linetype = 2 ) + 
    scale_y_continuous(limits=c(0,ymax_freq+.01)) + 
    scale_x_continuous("Sample Mean Value",limits=c(min(means$avg)-3,max(means$avg)+3)) +
    ggtitle("Distribution of Means from All Samples") + 
    theme_bw()
  print(p3)

  
}

#   out2 <- hist(samples$avg, breaks = 50, 
#               xlim = c(mu-3*sigma/sqrt(n),mu + 3*sigma/sqrt(n)),
#               ylim = c(0,ymax_freq+.015), freq=F,main = NULL)
#   text(1/rate, ymax_freq+.0075, paste("Number of means = ", as.character(N)), col = "red", cex = 2)
#   lines(density(samples$avgs, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
#   curve(dnorm(x, mu, sigma/sqrt(n)), from = mu-3*sigma/sqrt(n), to = mu + 3*sigma/sqrt(n), add = T, col = "red")
# 
# out1 <-  hist(samples$avgs, breaks = 50, xlim = c(mu-3*sigma/sqrt(n),mu + 3*sigma/sqrt(n)),
#      main = NULL)
# text(1/rate, ymax_freq+.05, paste("Number of means = ", as.character(N)), col = "red", cex = 2)
# d_est <- density(samples$avgs, adjust=2)
# d_est$y <- ((d_est$y*1000)*diff(out1$breaks)[1])
# lines(d_est, lty="dotted", col="darkgreen", lwd=2) 
# out2 <- hist(samples$avgs, breaks = 50, xlim = c(mu-3*sigma/sqrt(n),mu + 3*sigma/sqrt(n)),
#      main = NULL,prob = T)
# lines(density(samples$avgs, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
# curve(dnorm(x, mu, sigma/sqrt(n))*1000, from = mu-3*sigma/sqrt(n), to = mu + 3*sigma/sqrt(n), add = T, col = "red")
