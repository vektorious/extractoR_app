##################################
# Alexander Kutschera 2020/04/21 #
# alexander.kutschera@gmail.com  #
#     §§  CC-BY-SA 4.0  §§       #
         Version = 0.8          
##################################

# Settings 
         
## Files, folder and data sources
file = "data/test.dat" # only required if read_folder is "FALSE", reads single file
read_folder = FALSE # scans folder and analyses all files
folder_path = "/Volumes/GNASTICK" # path to the folder which should be scanned
filter_files = "_filteredpcameasurements.dat" # pattern in the files which should be analysed
folder_input = "input"
folder_results = "output_results" # write results into new folder
folder_plots = "output_plots" # write plots into new folder
move_all_file = FALSE # move all analyzed files into new folder
move_to = "old_input" # specifiy in which folder all analyzed filed should be moved
remove_old = TRUE # should the old files be removed from the original folder?

## Analysis and data preparation
thresh = 0.1  # set and change the thresh
remove_Cy5 = TRUE # remove the Cy5 channel
use_samplename = TRUE # use sample names to get info about the treatment
#### Naming convention! ####
# separate infos with underscores: ID_concentration_treatment1_treatment2
# e. g. DH12345_100µl_old-protocol_95°C-10min
# for now the script extracts the ID by using everything until the first underscore.
# If you want to record concentrations in the format of 10^-3 write 10e-3 rather than 10-3!
# Keep this in mind when naming your samples

## Plot aesthetics
plot_colors = c("#33a02c", "#e31a1c", "#1f78b4", "#b2df8a", "#ff7f00", "#fb9a99", "#fdbf6f", "#a6cee3")

# libraries
library(dplyr)
library(lubridate)
library(reshape)
library(ggsci)
library(gridExtra)
library(grid)
library(ggplot2)
library(ggpubr)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# functions
source("fun_extracto.R") # carful! This file contains all the important functions 
                         # It needs to be in the same folder as this script 

# Script #############################

if (read_folder == TRUE) {
  all_filenames <- list.files(paste0(folder_path, "/", folder_input), pattern = filter_files)
  for (filename in all_filenames) { # analyze all files in the folder
    print(paste0("analysing ", paste0(folder_path, "/", folder_input, "/", filename), " ..."))
    data <- read_dat_neo(paste0(folder_path, "/", folder_input, "/", filename)) # read the data file
    
    results <- extract_info(data$no_temp, thresh, filename, use_samplename, remove_Cy5) # extract and analyse data
    
    plot <- create.plot(data, results, filename, thresh, remove_Cy5 = TRUE) # plot the data
    write.pdf(results, plot, filename, into_folder = paste0(folder_path, "/", folder_plots)) # write results to pdf
    
    dir.create(paste0(folder_path, "/", folder_results))
    write.csv2(results, paste0(folder_path, "/", folder_results, "/", gsub(".dat", ".csv", filename))) # save results as .csv
  }
} else {
  data <- read_dat_neo(file) # read the data file
  results <- extract_info(data$no_temp, thresh, file, use_samplename, remove_Cy5) # extract and analyse data
  write.csv2(results, gsub(".dat", ".csv", file)) # save results as .csv
  plot <- create.plot(data, results, file, thresh, remove_Cy5 = TRUE) # plot the data
  write.pdf(results, plot, file) # write results to pdf
}

# (re)moving files after analysis
if (move_all_file == TRUE) {
  print(paste0("removing files from ", folder_input, " ..."))
  dir.create(paste0(folder_path, "/", move_to))
  filenames <- list.files(paste0(folder_path, "/", folder_input))
  for (filename in filenames) {
    file.copy(paste0(folder_path, "/", folder_input, "/", filename), paste0(folder_path, "/", move_to))
    if (remove_old == TRUE) {
      file.remove(paste0(folder_path, "/", folder_input, "/", filename), paste0(folder_path, "/", move_to))
    }
  }
}


