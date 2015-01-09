build_demos <- function(docs=c("power_alpha","power_n", "power_sd", "power_effect", "regression_outliers",
                               "sampling_distribution"), param_set=NULL, anim_only = FALSE, anim_dir = "anim_output", html_dir = "html_output") { 
  library(animation)
  library(ggplot2)
  library(gridExtra)
  start_dir = getwd()
  
  # function table to look up right function call for demo
  fun_table <- list(power_alpha = list(name="power"),power_n = list(name="power"), power_sd=list(name = "power"),
                    power_effect= list(name="power"), regression_outliers =list(name="regression_outliers"),
                    sampling_distribution = list(name="sampling_distribution"))

  args_list <- list(power_alpha = list(meanh0 = 100, sdh0 = 15, effect =5, N = 25, interval = 2.5, frames = 10),
       power_effect = list(meanh0 = 100, sdh0 = 15, alpha=.05,  N =25, interval=2.5, frames = 10),
       power_n = list(meanh0 = 100, sdh0 = 15, effect = 3,  alpha=.05 , interval=2.5, frames = 30),
       power_sd = list(meanh0 = 100, effect = 3,  alpha=.05 , N =25, interval=2.5, frames = 30),
       regression_outliers = list(n = 20, range=c(70,130), interval = .75, frames =20),
       sampling_Distribution = list(samps = 10, n = 50, rate = .03)
      )
  # prune table based on input args
  fun_table <- fun_table[docs]
  
  # add the file path and arguments on there 
  for (i in 1:length(fun_table)) { 
#     fun_table[[i]]$name <- file.path("src",fun_table[[i]]$name)
    fun_table[[i]]$fun <- source(file.path("src",paste(fun_table[[i]]$name,'.R',sep=''))) # to do = throw custom error about changing directory
    fun_table[[i]]$args <- args_list[names(fun_table[i])][[1]]
  }

  if (anim_only) {    
    dir.create(anim_dir, mode = "0775")
    for (i in 1:length(fun_table)) {
      setwd("anim_output")
      saveHTML({do.call(fun_table[[i]]$fun$value,fun_table[[i]]$args)}, img.name = fun_table[[i]]$name, 
               imgdir =  "img",  htmlfile = paste(fun_table[[i]]$name,".html", sep=''),
               autobrowse = FALSE, verbose=FALSE, autoplay =FALSE, navigator = FALSE,
               title = fun_table[[i]]$name,ani.width = 900, ani.height=500)
      setwd(start_dir)
    }
  } else {
      library(rmarkdown)
      library(knitr)

      
      opts_knit$set(animation.fun=hook_scianimator)
      start_dir = getwd()
      if (file.exists(html_dir)) {
        unlink(html_dir, recursive=TRUE)
      }
      dir.create(html_dir)
      rmd_files <- sub('.R','.Rmd', files)
      rmd_files <- sub('src','Rmd',rmd_files)
      
      for (i in rmd_files) {
        render(i,output_dir = file.path('..',html_dir),envir = environment())
      }
      
      setwd(start_dir)
      file.copy(file.path("Rmd","depends","custom.css"),file.path(html_dir, "assets","custom.css"))
      file.copy(file.path("Rmd","depends","demos.css"),file.path(html_dir, "assets","demos.css"))      
      render(file.path("Rmd",'demos.Rmd'),output_dir = file.path('..',html_dir),
             intermediates_dir = file.path('..',html_dir),
             envir = parent.env(environment()) )
  }
}