coordCols = function(x, resolution) {
  colnames(x) = seq(0, (nrow(x)-1)*resolution, resolution )
  return(x)
}