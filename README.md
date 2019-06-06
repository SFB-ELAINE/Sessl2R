# Sessl2R
R package to work with output files from SESSL

## How to build the package and use it

```R

require(devtools)
check()
install()
require(Sessl2R)

#   OR:
#   Check Package:             'Ctrl + Shift + E'
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Test Package:              'Ctrl + Shift + T'


input_dir <- "~/results-2019-06-06-11-35-06-014"
getData(input_dir = input_dir)

input_file <- paste(input_dir, "/df1.rda", sep="")
plotData(input_file = input_file, output_name = "HybridResults",
         x_axis_name = "t [h]", y_axis_name = "# of GFP", y_log = FALSE)

```