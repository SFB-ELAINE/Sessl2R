#' @title plotData
#' @description Load data (Rda) of a Sessl experiment and plot it.
#' @details TODO
#' @aliases plotdata
#' @author Kai Budde
#' @export plotData
#' @import ggplot2
#' @param input_file A character (Rda file that contains all Sessl results)
#' @param output_name A character (name of the file that is to be saved)
#' @param x_axis_name A character
#' @param y_axis_name A character
#' @param y_log A boolean
#' @examples
#' \dontrun{
#' }
#'

# Created:     06/05/2019
# Last edited: 06/05/2019

plotData <- function(input_file, output_name, x_axis_name = NA, y_axis_name = NA, y_log = FALSE) {
  
  # Load libraries and set options #########################################
  .old.options <- options()
  on.exit(options(.old.options))
  options(stringsAsFactors = FALSE, warn=-1)

  initial_directory <- getwd()
  
  # Check whether there is data as input
  if(is.null(input_file)){
    data_available <- FALSE
    return(0)
  }
  
  # Load data frame ########################################################
  load(input_file)
  output_directory <- gsub("[[:alnum:]]+.rda$", "", input_file)
  setwd(output_directory)
  
  loaded_dataframe <- gsub("\\.rda", "", input_file)
  
  loaded_dataframe <- gsub("/.*/", "", loaded_dataframe)
  #TODO: Also add if the file was given by \\
  
  df <- get(loaded_dataframe)
  col_names <- colnames(df)
  
  # Plot #####################################################################
  p <- ggplot(df, aes(x=get(col_names[1]), y=get(col_names[2]))) +
    geom_point() +
    theme_bw()
  
  if(y_log == TRUE){
    p <- p + scale_y_log10()
  }
  
  if(!is.na(x_axis_name)){
    p <- p + xlab(x_axis_name)
  }
  
  if(!is.na(y_axis_name)){
    p <- p + ylab(y_axis_name)
  }
  ggsave(paste(output_name, ".pdf", sep = ""), plot = p, width = 60, height = 36, units = "cm")
  
  # Return to starting condition #############################################
  
  setwd(initial_directory)
  
  
}
