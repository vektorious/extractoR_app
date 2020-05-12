##################################
# Alexander Kutschera 2020/04/21 #
# alexander.kutschera@gmail.com  #
#     §§  CC-BY-SA 4.0  §§       #
##################################

# extractor.R functions
## Pre Settings
fun_version <- 0.8
plot_colors = c("#33a02c", "#e31a1c", "#1f78b4", "#b2df8a", "#ff7f00", "#fb9a99", "#fdbf6f", "#a6cee3")

read_dat_neo <- function(filename){
  status <- FALSE
  i = 1 # skip first 1 rows to save data
  while (isFALSE(status)) {
    canRead <- tryCatch(read.table(filename, sep = "\t", dec = ",", header = FALSE,
                                   fileEncoding = "UTF-16le", skip = i, row.names = NULL,
                                   check.names = FALSE, nrows = 1, stringsAsFactors = FALSE),
                        error = function(e) return(FALSE))
    i <- i + 1
    if (!isFALSE(canRead)) {
      if (ncol(canRead) > 8) {
        status <- TRUE
      }
    } else if (i == 200) {
      break
    }
  }
  
  if (status) {
    cn <- canRead
    raw_data <- read.table(filename, sep = "\t", dec = ",", skip = (i + 2), header = FALSE, fileEncoding = "UTF-16le", row.names = NULL, check.names = FALSE)
    colnames(raw_data) <- cn[1,]
    data <- raw_data[1:ncol(raw_data) - 1] # removes the last column
    
    # For analysis and plotting we do not need the Voltage/BlockT/LidT column
    
    cdata <- data[-c(2:4)]
    mdata <- melt(cdata, id = c("TIME[sec.]"))
  } else {
    ix = 1
    canRead <- tryCatch(raw_data <- read.table(filename, sep = "\t",
                                               dec = ",", skip = ix, header = TRUE,
                                               row.names = NULL, check.names = FALSE),
                        error = function(e) return(FALSE))
    while (isFALSE(canRead)) {
      ix <- ix + 1
      canRead <- tryCatch(raw_data <- read.table(filename, sep = "\t",
                                                 dec = ",", skip = ix, header = TRUE,
                                                 row.names = NULL, check.names = FALSE),
                          error = function(e) return(FALSE))
      if (ix == 200) {
        print("data is in the wrong format")
        break
      }
    }
    
    raw_data <- canRead
    colnames(raw_data) <- gsub("-Cy5", "-CY5", colnames(raw_data))
    new_colnames <- colnames(raw_data)[-1] # removes the first value "row.names"
    data <- raw_data[1:ncol(raw_data)-1] # removes the last column
    colnames(data) <- new_colnames # adjust column names
    data$TIME <- as.numeric(gsub(",", ".", data$TIME)) # replace "," with "." and convert col $TIME to numeric
    colnames(data)[1] <- "TIME[sec.]"
    # For analysis and plotting we do not need the Voltage/BlockT/LidT column
    cdata <- subset(data, select= -c(Voltage, BlockT, LidT))
    
    mdata <- melt(cdata, id = c("TIME[sec.]"))
  }
  
  
  output <- list("all" = data,
                 "no_temp" = cdata,
                 "melted" = mdata)
  return(output)
}

ms <- function(t){
  paste(formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
        ,formatC(t %% 60, width = 2, format = "d", flag = "0")
        ,sep = ":"
  )
  
}

get_ttt <- function(column, time, threshold, seconds = TRUE){
  
  if (seconds == FALSE){
    seconds_to_period(time[min(which (column > threshold))])
  } else {
    time[min(which (column > threshold))]
  }
}

extract_info <- function(data, threshold, filename, extract_from_samplename, remove_Cy5){
  
  ttt <- apply(data, 2, get_ttt, time = data$`TIME[sec.]`, threshold = threshold) # get time to threshold
  max_f <- apply(data, 2, max)
  results <- data.frame(ttt, max_f)
  results <- results[-1, ] # remove "TIME" row
  
  if (extract_from_samplename == TRUE) {
    results$sample_id <- sapply(str_split(rownames(results),"_",n=2), `[`, 1)
    results$treatment <- gsub("-FAM", "", sapply(str_split(rownames(results),"_",n=2), `[`, 2))
    results$MIC_Cq <- NA
    
    results$channel <- NA
    results$channel[grepl(paste0("CY5", collapse = "|"), rownames(results))] <- "CY5"
    results$channel[grepl(paste0("FAM", collapse = "|"), rownames(results))] <- "FAM"
    
    results$treatment <- na_if(results$treatment, "FAM")
    results$treatment <- na_if(results$treatment, "CY5")
  }
  
  if (remove_Cy5 == TRUE) {
    results <- results[!grepl(paste0("CY5", collapse = "|"), rownames(results)),]
  }
  
  results$filename <- unlist(strsplit(filename, "/"))[length(unlist(strsplit(filename, "/")))]
  
  return(results)
}

create.plot <- function(data, results, filename, threshold, remove_Cy5){
  raw_mdata <- data$melted
  if (remove_Cy5 == TRUE){
    filtered_mdata <- raw_mdata[grepl(paste0("-FAM", collapse = "|"), raw_mdata$variable),] # filter for FAM channel
    filtered_mdata$variable <- gsub("-FAM", "", filtered_mdata$variable)
  } else {
    filtered_mdata <- raw_mdata
  }
  colnames(filtered_mdata) <- c("TIME [s]", "Sample", "Fluorescence [V]")
  
  plot <- ggplot() +
    scale_color_manual(values=plot_colors) +
    #scale_color_d3() +
    theme_bw() +
    ggtitle(filename) + xlab("Time [s]") + ylab("Fluorescence [V]") +
    geom_hline(yintercept=thresh, linetype="dashed", color = "grey", alpha = 0.7) +
    theme(legend.position="bottom") +
    guides(col = guide_legend(nrow = 4))
  
  dot_data <- results
  if (!all(is.na(dot_data$ttt))) { # add dot plots only if there are some values which crossed the threshold
    dot_data <- dot_data[!is.na(dot_data$ttt),] # remove samples which did not reach the treshold
    plot <- plot + geom_point(data = dot_data, aes(ttt, y = thresh), color = "grey", alpha = 0.7)
  }
  
  plot <- plot + geom_line(data = filtered_mdata, aes(x = `TIME [s]`, y = `Fluorescence [V]`, color = Sample) , size=0.5)
  
  return(plot)
}

create.table <- function(results) {
  results$MIC_Cq <- NULL
  results$filename <- NULL
  results$sample_id <- gsub("-FAM", "", results$sample_id)
  results$sample_id <- gsub("-CY5", "", results$sample_id)
  results$sample_id <- gsub("CC", "Cell culture supernatant", results$sample_id)
  results$treatment <- gsub("_", ", ", results$treatment)
  results$treatment <- gsub("-", " ", results$treatment)
  results$ttt <- ms(results$ttt)
  results$ttt <- gsub("NA:NA", " - ", results$ttt)
  results[is.na(results)] <- " - "
  colnames(results) <- c("time to threshold [m:s]", "max. fluorescence", "sample ID", "treatment", "channel")
  return(results)
}

write.pdf <- function(results, plot, filename, into_folder = FALSE, exclude_MIC_Cq = TRUE){
  table <- create.table(results)
  
  ggtable <- ggtexttable(table, rows = NULL, 
                         theme = ttheme("mCyan"))
  
  if (into_folder == FALSE) {
    pdf(gsub(".dat", ".pdf", filename), width = 21.0, height = 29.7, paper = "a4")
    
    fig <- ggarrange(plot, ggtable, ncol = 1, nrow = 2)
    
    fig <- annotate_figure(fig, right = fig,bottom = paste0("Script version ", Version, ", Alexander Kutschera"))
    print(fig)
    dev.off()
    print(paste0("writing file to ", gsub(".dat", ".pdf", filename)))
  } else {
    dir.create(into_folder)
    pdf(paste0(into_folder, "/", gsub(".dat", ".pdf", filename)), width = 21.0, height =  29.7, paper = "a4")
    
    fig <- ggarrange(plot, ggtable, ncol = 1, nrow = 2)
    
    fig <- annotate_figure(fig, right = fig,bottom = paste0("Script version ", Version, ", Alexander Kutschera"))
    print(fig)
    dev.off()
    print(paste0("writing file to ", into_folder, "/", gsub(".dat", ".pdf", filename)))
  }
}
