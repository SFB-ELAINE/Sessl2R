#' @title getData
#' @description Read csv files from SESSL experiments
#' @details Load results from SESSL and save for every directory a RData 
#' file. The input should be a directory that contains directories with
#' the config directories and experiment results.
#' @aliases getdata
#' @author Kai Budde
#' @export getData
#' @import utils
#' @param input_dir A character (directory that contains all Sessl results)
#' @param lines_to_delete A vector containing all lines that should be
#' deleted
#' @param columns_as_num A vector containing the names (strings) of the
#' columns to be saven as numeric
#' @examples
#' \dontrun{
#' }
#'

# Created:     06/05/2019
# Last edited: 06/05/2019


# TODO: Instead of loading a csv-file and appending it, it might be better
# to first load all csv files and afterwards put all data.frames togehter.

getData <- function(input_dir, lines_to_delete = NA, columns_as_num = NA) {
  
  # Load libraries and set options #########################################
  .old.options <- options()
  on.exit(options(.old.options))
  options(stringsAsFactors = FALSE, warn=-1)
  
  # Check whether there is data as input
  if(is.null(input_dir)){
    data_available <- FALSE
  }else{
    # Check whether directory contains files or other directories
    if(length(list.dirs(input_dir)) > 0){
      data_available <- TRUE
    }else{
      print("Please provide a valid directory name.")
      data_available <- FALSE
    }
  }
  # Data acquisition #######################################################
  
  if(data_available == FALSE){
    return(0)
  }
  
  results_directories <- list.dirs(input_dir, recursive = FALSE)
  number_of_directories <- length(results_directories)
  initial_directory <- getwd()
  
  fun <- function(file_name, directory_id, config_id, lines_to_delete){
    df <- data.table::fread(file_name)
    
    if(!is.na(lines_to_delete)){
      df <- df[-lines_to_delete,]
    }
    # Append information of directory
    df$directory <- directory_id
    
    # Append information of config
    df$config <- config_id
    
    # Append information of replication run
    df$run <- as.numeric(gsub(pattern = "run-(.+).csv", "\\1", file_name))

    return(df)
  }
  

  ## Go through each directory and append data
  
  for(i in 1:number_of_directories){
    setwd(results_directories[i])
    
    ## Go through every config-directory
    if(length(list.dirs(path = ".", recursive = FALSE)) > 0){
      results_config_directories <- list.dirs(path = ".", recursive = FALSE)
    }else{
      results_config_directories <- results_directories[i]
    }
    
    number_of_config_directories <- length(results_config_directories)
    

    for(j in 1:number_of_config_directories){
      df_config <- read.csv(paste(results_config_directories[j],"/config.csv",sep=""))
      
      ## Read in all csv files ##
      
      # Only take the resulting csv files and not the config file
      results_cvs_files <- list.files(results_config_directories[j])
      results_cvs_files <- results_cvs_files[grepl("run", results_cvs_files)]
      number_of_files <- length(results_cvs_files)
      
      
      
      
      # New data frame for the first time
      if(j == 1){
        df <- dplyr::bind_rows(lapply(results_cvs_files,
                                      fun, directory_id = i,
                                      config_id = j,
                                      lines_to_delete = lines_to_delete))
        
        # Append information of config files
        names_of_columns <- df_config$var
        if(length(names_of_columns) > 0){
          for(l in 1:length(names_of_columns)){
            df[[names_of_columns[l]]] <- df_config$value[l]
          }
        }
      }else{
        
        
        df_dummy <- dplyr::bind_rows(
          lapply(results_cvs_files, fun(directory_id = i,
                                        config_idf = j,
                                        lines_to_delete = lines_to_delete)))
        
        # Append information of config files
        names_of_columns <- df_config$var
        if(length(names_of_columns) > 0){
          for(l in 1:length(names_of_columns)){
            df_dummy[[names_of_columns[l]]] <- df_config$value[l]
          }
        }
        
        df <- dplyr::bind_rows(df, df_dummy)
      }
      
    }
    
    df <- as.data.frame(df)
    df <- dplyr::arrange(df, run)
    rownames(df) <- NULL
    
    # Save specific colums as numeric
    if(!is.na(columns_as_num)){
      for(l in 1:length(columns_as_num)){
        df[[columns_as_num[l]]] <- as.numeric(df[[columns_as_num[l]]])
      }
    }
    
    
    # Save the data frame of every sub-directory into rda file
    name_of_df <- paste("df",i, sep="")
    assign(name_of_df, df) # Copies the data frame into a newly named one
    save(list=name_of_df, file = paste(input_dir, "/", name_of_df,".rda", sep=""))
    rm(df, list=name_of_df)
    
  }
  
  setwd(initial_directory)
}
