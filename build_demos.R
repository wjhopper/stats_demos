build_demos <- function(docs="all",anim_only = FALSE) { 
  library(animation)
  library(ggplot2)
  library(gridExtra)
  
  files <- list.files(path = "./src",pattern="*.R",full.names=TRUE)
  if (docs != "all") {
    files = intersect(files,docs)
  }
  fun_list <- lapply(files,source)
  bnames <- sub('.R','',basename(files))
  
  if (anim_only) {
    start_dir = getwd()
    dir.create("anim_output", mode = "0775")
    for (i in 1:length(fun_list)) {
#       dir.create(file.path("anim_output",bnames[i]), mode = "0775")
#       setwd(file.path("anim_output",bnames[i]))
      setwd("anim_output")
      saveHTML({fun_list[[i]]$value()}, img.name = bnames[i], 
               imgdir =  "img",  htmlfile = paste(bnames[i],".html", sep=''),
               autobrowse = FALSE, verbose=FALSE, autoplay =FALSE, navigator = FALSE,
               title = bnames[i],ani.width = 1000, ani.height=800)
      setwd(start_dir)
    }
  } else {
    # knit things!
  }
}