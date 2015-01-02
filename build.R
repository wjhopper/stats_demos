build <- function(docs="all",anim_only = FALSE) { 
  files <- list.files(path = "./src",pattern="*.R")
  if (docs != "all") {
    files = intersect(files,docs)
  }
  fun_list <- lapply(files,source)
  
  if (anim_only) {
    for (i in funlist){
      save_html({fun_list[i]},s img.name = "Power_alpha", imgdir = "Power_alpha", htmlfile = "Power.alpha.html", 
                autobrowse = FALSE, title = "Power as a function of alpha", 
                description = "Power decreases as alpha decreases"))
    }
  } else {
    # knit things!
  }
}

  