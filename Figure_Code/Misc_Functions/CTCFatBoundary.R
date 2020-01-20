CTCFatBoundary = function(chr_bed, bed_file, marker, distance) {
  require(dplyr)
  
  CTCF_Res = marker %>% filter(chr == chr_bed) 
  bed = bed_file %>% group_by(end) %>% summarise(dist_end = sum(abs(end-CTCF_Res$end)<distance))
  return(mean(bed$dist_end))
   
   }

