confidence <- function(meanh0 = 100, N = NULL, s =NULL, conf= NULL, replicants = 100, interval =.5, frames = 100) {
  
  draw_plot <- function(df=NULL, wrap_var  = NULL, x_range=NULL,y_range=NULL) { 
    # We create a vector of x values over the range of values, stick in data frame
    for (i in 1:nrow(df)){
      p1 <- ggplot(data =  df[1:i,], aes(x=sample_means, y = count)) + 
        geom_point(color = 'red',size=.5) +
        geom_segment(aes(x = lower, xend= upper, y=count,yend=count, color=caught)) +  
        facet_wrap(as.formula(paste("~", wrap_var)), ncol=3) +
        scale_x_continuous("Sample Mean",expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 5)) +
        scale_color_manual("Mean in CI?",labels =c("out","In"),values = c("red","black")) +
        theme_bw() + 
        theme(strip.text = element_text(size=16)) +
        ggtitle(paste( as.character(conf*100),"% Confidence Intervals by Sample Size",sep=''))
      print(p1)
    }
    final_df <- df %>% group_by_(.dots=as.symbol(wrap_var)) %>% summarize(prop = (mean(as.numeric(caught))-1)*100)
    final_df$prop <- paste(round(final_df$prop,1),"%",sep='')
    p1 <- p1 +
      geom_text(aes(label= prop), data = final_df, x = meanh0, y = 102.5)
    print(p1)
    
  }
  
  args_list <- list(N, conf, sd)
  if (sum(sapply(args_list, is.null)) != 1) { 
    stop("exactly one of 'N', 'confidence' and 'sd' must be NULL")
  }
  
  if (!is.null(conf) && !is.numeric(conf) || any(conf <= 0 | conf >= 1)) { 
    stop("'conf' must be numeric in (0, 1)")
  }
  
  if (is.null(N)) {
    wrap_var <- 'N'
    N <- rep(c(10,100,1000), each=replicants)
    se <- s/sqrt(N)
    
    sample_means <- rep(0,length(N))
    for (i in 1:length(N)) {
      sample_means[i] <- mean(rnorm(N[i], meanh0, s))
    }
    crit_t <- qt(conf + ((1-conf)/2),N)
    upper <- sample_means + (crit_t * se) 
    lower <- sample_means - (crit_t * se) 
    caught <- ifelse((meanh0 <= upper & meanh0 >= lower),1,0)
    data <- data.frame(count = rep(1:replicants, 3), N, se, sample_means, upper, lower, caught = factor(caught) )
    draw_plot(data, wrap_var, x_range= c(min(data$lower)-.05, max(data$upper)+.05),
              y_range = c(0,201)) 


  } else if (is.null(conf)) { 
    wrap_var <- 'conf'
    conf <- rep(c(.85,.95,.99), each=replicants)
    se <- s/sqrt(N)
    sample_means <- replicate(replicants*3, mean(rnorm(N, meanh0, s)))
    crit_t <- qt(conf + ((1-conf)/2),N)
    upper <- sample_means + (crit_t * se) 
    lower <- sample_means - (crit_t * se) 
    caught <- ifelse((meanh0 <= upper & meanh0 >= lower),1,0)
    data <- data.frame(count = rep(1:replicants, 3), conf, se, sample_means, upper, lower, caught = factor(caught) )
    draw_plot(data, wrap_var,x_range= c(min(data$lower)-.05, max(data$upper)+.05),
              y_range = c(0,201)) 

  } else { 
    stop("internal error herp derp cherp")
  }
}