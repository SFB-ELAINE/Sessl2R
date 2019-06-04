#' @title getData
#' @description Read csv files from SESSL experiments
#' @details The input should be a directory that contains (directories other)
#' csv files. The output is a dataframe.
#' @aliases getdata
#' @author Kai Budde
#' @export getData
#' @param input_dir A character (directory that contains all images)
#' @examples
#' \dontrun{
#' }
#'

getData <- function(input_dir) {
  
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
  if(data_available == TRUE){
    
    # List of all directories with experiment results
    list.dirs(input_dir)
    
    df <- read.csv(input_dir)
    return(df)
  }else{
    return(0)
  }
}
