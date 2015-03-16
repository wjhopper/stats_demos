CDF_v_PDF <- function(interval =1) {   
  # length(seq(-3,3,by=.025))
  x <- seq(-3,3,length.out=ani.options("nmax"))
  y_cum  <- pnorm(x)
  y_dens <- dnorm(x)
  CDF_data <- data.frame(x,y_cum)
  CDF <- ggplot() + 
    scale_y_continuous('Cumulative Probability',limits=c(0,1)) + 
    scale_x_continuous(limits = c(-3,3),breaks = seq(-3,3,by =1)) + theme_bw() + 
    ggtitle("Gaussian Cumulative Density Function (CDF")
  
  PDF <- ggplot(data = data.frame(x,y_dens), aes(x=x,y=y_dens)) + 
    geom_line() + 
    ylab('Normal Density') + 
    scale_x_continuous(limits = c(-3,3),breaks = seq(-3,3,by =1)) + theme_bw() +
    ggtitle("Gaussian Probability Density Function (PDF)")
  for (N in 1:ani.options("nmax")) {
      df <- CDF_data[1:N,]
      CDF_2 <- CDF + 
        geom_line(data=df,aes(x=x,y=y_cum) ) + 
        geom_segment(data=df,x= df[N,'x'], y = 0, xend = df[N,'x'], yend = y_cum[N]) + 
        geom_segment(data=df,x=-3,y= y_cum[N], xend = x[N], yend = y_cum[N])
      
      PDF_2 <- PDF + 
        geom_polygon(data = data.frame(x = c(min(x), x[1:N], x[N]),
                                       y = c(0, y_dens[1:N], 0)),
                     aes(x=x,y=y), fill='blue',color='black')
      grid.arrange(CDF_2,PDF_2,ncol=1,nrow=2)
  }

}