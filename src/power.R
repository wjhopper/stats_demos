power<- function(meanh0 = 100, effect =NULL, N = NULL, sdh0 = NULL, alpha = NULL, interval = 2.5, frames = 10) {

  draw_plot <- function(x_range=NULL,y_range=NULL) { 
    power <- (power.t.test(n=N,delta = effect, sd = sdh0, sig.level = alpha, power= NULL, 
                           type = "one.sample", alternative = "one.sided")$power)
    Beta <- round((1-power),2)
    
    # We create a vector of x values over the range of values, stick in data frame
    df <- data.frame(xv = seq(meanh0 - 4*se, meanh1 + 4*se, 0.01))
    df$yv_null <-dnorm(df$xv,meanh0,se)
    df$yv_alt <-dnorm(df$xv,meanh1,se)
    
    
    p1 <- ggplot(data =df, aes(x=xv)) + 
      geom_line(aes(y=yv_null)) +
      geom_line(aes(y=yv_alt)) +
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
      geom_text(data=data.frame( x = cutoff+max(((2/3)*se),1.5), y = -.0225, alpha = alpha), 
                mapping = aes(label = paste("alpha ==",alpha), x=x, y=y),
                parse=TRUE, size = 3) + 
      geom_text(data=data.frame( x = cutoff-max(((2/3)*se),1.5), y = -.0225, Beta = Beta), 
                mapping = aes(label = paste("beta ==",Beta), x=x, y=y),
                parse=TRUE, size = 3) +
      geom_text(data=data.frame( x = max(meanh0,cutoff+3), y = .5*max(df$yv_null), power=power), 
                mapping = aes(label = paste("power ==",round(power,2)), x=x, y=y),
                parse=TRUE, size = 3) + 
      geom_text(data=data.frame( x = meanh0, y = max(df$yv_null)+.01),
                mapping = aes(label = "H[0]", x=x, y=y), parse=TRUE, size = 4) + 
      geom_text(data=data.frame( x = meanh1, y = max(df$yv_alt)+.01),
                mapping = aes(label = "H[1]", x=x, y=y), parse=TRUE, size = 4) + 
      scale_x_continuous("Numeric Dependent Variable Values", limits = x_range) + 
      scale_y_continuous("Normal Distribution",limits=c(y_range[1],y_range[2]+.03))  +
      ggtitle(sprintf("Effect Size = %1.0f, Sample SD = %1.2f, Sample Size = %1.0f, Alpha Level = %0.2f", 
                      round(effect,2), round(sdh0,2), round(N,2),round(alpha,2))) +
      theme_bw() +
      theme(plot.title = element_text(size=10),
            panel.grid.major = element_blank(),
            axis.title.y = element_text(size=8),
            axis.title.x = element_text(size=8),
            plot.margin = unit(c(0,.25,.25,.25),units="lines"))
    print(p1)
  
  }

  args_list <- list(N, effect, sdh0, alpha)
  if (sum(sapply(list(N, effect, sdh0, alpha), is.null)) != 1) { 
    stop("exactly one of 'n', 'effect', 'sdh0', and 'alpha' must be NULL")
  }
  
  if (!is.null(alpha) && !is.numeric(alpha) || any(0 > alpha | alpha > 1)) { 
    stop("'sig.level' must be numeric in [0, 1]")
  }
  
  opts <- ani.options()
  ani.options(interval = interval, nmax = frames)
  
  if (is.null(N)) {

    meanh1 <- meanh0+effect
    start_se <- sdh0/sqrt(5)
    end_se <- sdh0/sqrt(frames*5)
    for (N in seq(5,frames*5,by = 5)) {
      se <- sdh0/sqrt(N)
      cutoff = qnorm(1-alpha,meanh0,se)
      draw_plot(x_range= c(meanh0 - 4*start_se,meanh1 + 4*start_se),
                y_range = c(-.025, dnorm(meanh0, meanh0, end_se))) 
    } 
  } else if (is.null(sdh0))  { 
    
    meanh1 <- meanh0+effect
    start_se <- 15/sqrt(N)
    end_se <- 3/sqrt(N)
    for (sdh0 in seq(15,3,length.out=frames)) {
      se <- sdh0/sqrt(N)
      cutoff = qnorm(1-alpha,meanh0,se)
      draw_plot(x_range= c(meanh0 - 4*start_se,meanh1 + 4*start_se),
                y_range = c(-.025, dnorm(meanh0, meanh0, end_se)) )
    }
  } else if (is.null(effect)) { 
    
    se <- sdh0/sqrt(N)
    cutoff = qnorm(1-alpha,meanh0,se)
    max_meanh1 <- meanh0+frames
    for (effect in seq(1,frames,by=1)) {
      meanh1 <- meanh0+effect
      draw_plot(x_range= c(meanh0 - 4*se,max_meanh1 + 4*se),
                y_range = c(-.025, dnorm(meanh0, meanh0,se)))
    }
  } else if (is.null(alpha)) { 
    
    se <- sdh0/sqrt(N)
    meanh1 <- meanh0+effect
    for ( alpha in seq(.2,.01, by=-.2/frames)) {
      cutoff = qnorm(1-alpha,meanh0,se)
      draw_plot(x_range= c(meanh0 - 4*se,meanh1 + 4*se),
                y_range = c(-.025, dnorm(meanh0, meanh0,se)) )
    }
  } else { 
    stop("internal error herp")
  }
}