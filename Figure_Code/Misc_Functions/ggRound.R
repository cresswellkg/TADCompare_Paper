ggRound = function(curr_plot, digits = 2) {
  q = ggplot_build(curr_plot)
  
  #Run through data and edit any annotation to match digits and scipen
  
  q$data = lapply(q$data, function(x) {
    if ("annotation" %in% colnames(x)) {
      x$annotation = gsub("p <", "", x$annotation)
      x$annotation = format(as.numeric(as.character(x$annotation)), scientific = TRUE, digits = digits)
      x$annotation[x$annotation == 2.2e-16] = paste0("<", 2.2e-16)
        
    }
    return(x)
  })
  
  
  q = ggplot_gtable(q)
  return(q)
}
