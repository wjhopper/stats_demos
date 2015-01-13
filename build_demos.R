build_demos <- function(docs=c("power_alpha","power_n", "power_sd", "power_effect", "regression_outliers",
                               "sampling_distribution"), param_set=NULL, anim_only = FALSE, anim_dir = "anim_output", html_dir = "html_output") { 
  library(animation)
  library(ggplot2)
  library(gridExtra)
  start_dir = getwd()
  if (anim_only) {
    dir.create(anim_dir)
  } else {
    dir.create(html_dir)
  }
  
  # function table to look up right function call for demo
  fun_table <- list(power_alpha = list(name="power"),power_n = list(name="power"), power_sd=list(name = "power"),
                    power_effect= list(name="power"), regression_outliers =list(name="regression_outliers"),
                    sampling_distribution = list(name="sampling_distribution"))

  args_list <- list(power_alpha = list(meanh0 = 100, sdh0 = 15, effect =5, N = 25, interval = 2.5, frames = 10),
       power_effect = list(meanh0 = 100, sdh0 = 15, alpha=.05,  N =25, interval=2.5, frames = 10),
       power_n = list(meanh0 = 100, sdh0 = 15, effect = 3,  alpha=.05 , interval=2.5, frames = 30),
       power_sd = list(meanh0 = 100, effect = 3,  alpha=.05 , N =25, interval=2.5, frames = 15),
       regression_outliers = list(n = 20, range=c(70,130), interval = 1, frames =20),
       sampling_distribution = list(samps = 30, n = 50, rate = .03, interval = .1,frames =30 )
      )
  # prune table based on input args
  fun_table <- fun_table[docs]

  for (i in 1:length(fun_table)) { 
    # add the file path and arguments on there 
    fun_table[[i]]$fun <- source(file.path("src",paste(fun_table[[i]]$name,'.R',sep=''))) # to do = throw custom error about changing directory
    fun_table[[i]]$args <- args_list[names(fun_table[i])][[1]]
    filt <- setdiff(names(fun_table[[i]]$args[]),c('interval','frames'))
    ani.options(nmax = fun_table[[i]]$args[['frames']], interval = fun_table[[i]]$args[['interval']])

  if (anim_only) {    
    dir.create(anim_dir, mode = "0775")
    setwd("anim_output")
    saveHTML({do.call(fun_table[[i]]$fun$value,fun_table[[i]]$args[filt])}, img.name = names(fun_table[i]), 
             imgdir =  "img",  htmlfile = paste(names(fun_table[i]),".html", sep=''),
             autobrowse = FALSE, verbose=FALSE, autoplay =FALSE, navigator = FALSE,
             title = names(fun_table[i]),ani.width =1000, ani.height=500)
    setwd(start_dir)
  } else {
      library(rmarkdown)
      library(knitr)
      opts_knit$set(animation.fun=hook_scianimator)
      
      if (file.exists(html_dir)) {
        unlink(html_dir, recursive=TRUE)
      }
      dir.create(html_dir)
      render(file.path("Rmd",paste(names(fun_table[i]),".Rmd",sep="")),
               output_dir = file.path('..',html_dir),
               envir = environment())
      setwd(start_dir)
  }
      file.copy(file.path("Rmd","depends","custom.css"),file.path(html_dir, "assets","custom.css"))
      file.copy(file.path("Rmd","depends","demos.css"),file.path(html_dir, "assets","demos.css"))      
      render(file.path("Rmd",'demos.Rmd'),output_dir = file.path('..',html_dir),
             intermediates_dir = file.path('..',html_dir),
             envir = parent.env(environment()) )
  }
}