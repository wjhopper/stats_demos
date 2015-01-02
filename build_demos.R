build_demos <- function(docs="all",anim_only = FALSE, anim_dir = "anim_output", html_dir = "html_output") { 

  files <- list.files(path = "src",pattern="*.R",full.names=TRUE)
  if (docs != "all") {
    files = files[sub('.R', '', basename(files)) == docs]
  }
  bnames <- sub('.R','',basename(files))

  
  if (anim_only) {
        
    library(animation)
    files <- as.list(files)
    names(files) <- bnames
    fun_list <- lapply(files,source)
    
    start_dir = getwd()
    dir.create(anim_dir, mode = "0775")
    for (i in 1:length(fun_list)) {
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