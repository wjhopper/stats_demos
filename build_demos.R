build_demos <- function(docs=c("power_alpha","power_n", "power_sd", "power_effect", "regression_outliers",
                               "sampling_distribution","CDF_v_PDF","confidence_conf", "confidence_n"), 
                        param_set=NULL, anim_only = FALSE, anim_dir = "anim_output", html_dir = "html_output") { 
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
  fun_table_big <- list(power_alpha = list(name="power", title = "Power and Alpha Level"),
                    power_n = list(name="power", title = "Power and Effect Size"), 
                    power_sd=list(name = "power", title = "Power and Sample Standard Deviation"),
                    power_effect= list(name="power", title = "Power and Effect Size"),
                    regression_outliers =list(name="regression_outliers", title = "Regression and Outliers"),
                    sampling_distribution = list(name="sampling_distribution", title = "Sampling Distributions"),
                    CDF_v_PDF  = list(name="CDF_v_PDF", title = "Relationship between PDF and CDF"),
                    confidence_conf = list(name="confidence", title = "Confidence Interval and desired Confidence Level"),
                    confidence_n = list(name="confidence", title = "Confidence Interval and sample size")
  )

  args_list <- list(power_alpha = list(meanh0 = 100, sdh0 = 15, effect =5, N = 25, interval = .5, frames = 20),
       power_effect = list(meanh0 = 100, sdh0 = 15, alpha=.05,  N =25, interval=.5, frames = 20),
       power_n = list(meanh0 = 100, sdh0 = 15, effect = 3,  alpha=.05 , interval=.5, frames = 30),
       power_sd = list(meanh0 = 100, effect = 3,  alpha=.05 , N =25, interval=.5, frames = 15),
       regression_outliers = list(n = 20, range=c(70,130), interval = 1, frames =20),
       sampling_distribution = list(samps = 1000, n = 50, rate = .03, interval = .1,frames =1000),
       CDF_v_PDF = list(interval = .15,frames = length(seq(-3,3,by=.025))),
       confidence_conf = list(meanh0 = 100,  N = 100, s =15, replicants = 100, interval =.5, frames = 100),
       confidence_n =list(meanh0 = 100,  conf=.95, s =15, replicants = 100, interval =.5, frames = 100)
      )
  # prune table based on input args
  fun_table <- fun_table_big[docs]

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

      if (file.exists(file.path('Rmd',paste(names(fun_table[i]),"_files",sep='')))) {
        unlink(file.path('Rmd',paste(names(fun_table[i]),"_files",sep=''), recursive=TRUE))
        file.remove(file.path('Rmd',paste(names(fun_table[i]),".html",sep='')))
      }
      render(file.path("Rmd",paste(names(fun_table[i]),".Rmd",sep="")),
             envir = environment())
      setwd(start_dir)
    }
  }
  render(file.path("Rmd",'demos.Rmd'), envir = environment())

  if (!anim_only) {
      # get html files 
      f <- list.files(path = "Rmd/", pattern="*.html",full.names = TRUE)
      # get directories (images + css + js)
      d<- list.dirs(path = "Rmd/", recursive=F)
      # filter out ones that we haven't built on this round. Shouldn't be any, but just in case
      f <- f[ f %in% paste('Rmd/',docs,'.html',sep='')]
      d <- c(d[sub('_files','',basename(d)) %in% docs],'depends')
      success <- file.copy(f, html_dir,recursive = T)
      if (all(success)) {
        file.remove(f)
      } else {
        stop(paste(f[!(success)], 'could not be copied to', html_dir))
      }
      success <- file.copy(d, html_dir, recursive = T)
      if (all(success)) {
        unlink(d[!grepl('depends',d,fixed=TRUE)], recursive=TRUE)
#         file.remove(d[!grepl('depends',d,fixed=TRUE)])
      } else {
        stop(paste(d[!(success)], 'could not be copied to', html_dir))
      }
  }

}