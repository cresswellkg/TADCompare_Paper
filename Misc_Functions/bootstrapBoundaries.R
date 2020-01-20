bootstrapBoundaries = function(chr_bed, bed_file, marker, distance, res, permutations, expression = TRUE) {
  set.seed(123)
  require(dplyr)
  print(chr_bed)

  #marker_Res = marker %>% dplyr::filter(chr == chr_bed)  %>% dplyr:: distinct() %>% dplyr::group_by(end) %>% dplyr::dplyr::summarise(close_dist = min(abs(end-bed_file$end))) %>% dplyr::filter(close_dist <= distance)
  
  marker_curr = marker %>% dplyr::filter(chr == chr_bed)
  
  num_within = bed_file %>% filter(chr == chr_bed) %>% dplyr::group_by(end) %>% dplyr::mutate(num_within = length(which (abs(end-marker_curr$end) < distance )))
  
  #We filter areas with zero marker sites because using peaks as criteria are irrelevant
  
  #Generate random endings based on the bed file
  poss_ends = seq(min(num_within$end), max(num_within$end), by = res) 
  poss_ends = data.frame(end = poss_ends)
  
  num_within = num_within %>% dplyr::filter(num_within > 0)


  #Repeat marker finding process
  
  num_within_tot = poss_ends %>% dplyr::group_by(end) %>% dplyr::mutate(num_within = sum(abs(end-marker_curr$end) < distance )) %>% dplyr::filter(num_within != 0)
  
  if (nrow(num_within) == nrow(num_within_tot)) {
    type = "neither"
    perm_p = 0
    return(cbind(perm_p, type))
  } 
  
  #Run the bootstrap t test
  mean_diffs = c()
  
  #Mean of real data
  
  real_mean = mean(num_within$num_within)
  
  if (is.na(real_mean)) {
    perm_p = 0
    type = "neither"
    
  } else {
    
  num_within_other = num_within_tot[-which(num_within_tot$end %in% num_within$end),]
  real_diff = real_mean-mean(num_within_other$num_within)
  
  for (i in 1:permutations) {
    rand_samp1 = sample(num_within_tot$num_within, nrow(num_within), replace = F)
    rand_samp2 = sample(num_within_tot$num_within, nrow(num_within_other), replace = F)
    
    mean_diff_curr = abs(mean(rand_samp1)-mean(rand_samp2))
    mean_diffs = c(mean_diffs, mean_diff_curr)
    }
  
  if (expression) {
  if (is.na(real_diff)) {
    type = "neither"
    perm_p = 0
  } else if (real_diff>0) {
    type = "enrichment"
    perm_p = (sum(mean_diffs>=abs(real_diff))+1)/(permutations+1)
    mean_comp = mean(mean_diffs)
    mean_rand = mean(real_diff)
    
  } else if (real_diff<0) {
    type = "depletion"
    perm_p = (sum(mean_diffs>=abs(real_diff))+1)/(permutations+1)
    mean_comp = mean(real_mean)
    mean_rand = mean(real_diff)
    
  } else {
    type = "neither"
    perm_p = 0
    mean_comp = mean(real_mean)
    mean_rand = mean(real_diff)
  }
  
  } else {
  perm_p = (sum(mean_diffs<=abs(real_diff))+1)/(permutations+1)
  mean_comp = mean(real_mean)
  mean_rand = mean(real_diff)
  }
  }  
  return(cbind(perm_p, type,  mean_comp, mean(num_within_other$num_within), mean_comp))
}

