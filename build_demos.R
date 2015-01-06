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
      library(rmarkdown)
      start_dir = getwd()
      if (file.exists(html_dir)) {
        file.remove(html_dir)
      }
      dir.create(html_dir)
      rmd_files <- sub('.R','.Rmd', files)
      rmd_files <- sub('src','Rmd',rmd_files)
      
      for (i in rmd_files) {
        render(i,output_dir = file.path('..',html_dir))
      }
      setwd(start_dir)
      file.copy(file.path("Rmd","depends","custom.css"),file.path(html_dir, "assets","custom.css"))
      file.copy(file.path("Rmd","depends","demos.css"),file.path(html_dir, "assets","demos.css"))      
      render(file.path("Rmd",'demos.Rmd'),output_dir = file.path('..',html_dir),intermediates_dir = file.path('..',html_dir) )
  }
}