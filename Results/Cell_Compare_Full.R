# Script to run SpectralTAD on schmitt 2016 data

# set up

dataDir <- '/home/sequencing/juicer/schmitt_2016/contact_maps/RAW/primary_cohort/'
source("TADCompare.R")
library(dplyr)
library(readr)


require(SpectralTAD)

# get file list
files <- list.files(dataDir)
#files <- files[grepl("\\.chr22.mat", files)]
#file_combs <- expand.grid(files, files)
#file_combs <- file_combs %>% filter(Var1 != Var2)

chr_list = paste0('\\.chr', 22:1, '.mat')
# run spectal TAD
tad_list <- rbind()
for (j in chr_list) {
files_new <- files[grepl(j, files)]
cell_locs = c(5,6:8,10, 15:23, 33:35)
files_new = files_new[cell_locs]
file_combs <- expand.grid(files_new, files_new)
file_combs <- file_combs %>% filter(Var1 != Var2)
for(i in 1:nrow(file_combs)) {
  mat1 <- read_table2(paste0(dataDir, file_combs[i,1]), col_names = FALSE)
  mat2 <- read_table2(paste0(dataDir, file_combs[i,2]), col_names = FALSE) 
  # set column names
  colnames(mat1) <- seq(0, (ncol(mat1)-1)*40000, 40000)
  colnames(mat2) <- seq(0, (ncol(mat2)-1)*40000, 40000)
  chr <- sub(".*\\.*.\\.*\\.chr", "chr", file_combs[i,2])
  chr <- sub("\\.mat", "", chr)
  result <- TADCompare(mat1, mat2, resolution = 40000, 
window_size = 25, z_thresh = 2, pre_TADs = FALSE)$TAD_Frame
  file1 = rep(file_combs[i,1], nrow(result))
  file2 = rep(file_combs[i,2], nrow(result))
  curr_frame = cbind(chr,file1, file2, result)
  tad_list <- rbind(tad_list, curr_frame)
  saveRDS(tad_list, "All_TADs_Cells.rds")

}
}

