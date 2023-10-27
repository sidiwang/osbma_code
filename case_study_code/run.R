
### get the directory of the current file
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  script_path <- rstudioapi::getSourceEditorContext()$path
  script_directory <- dirname(script_path)
  print(script_directory)
} else {
  print("Not running in RStudio or rstudioapi not available.")
}

### set working directory
setwd(script_directory)

### import data
dataset <- read.csv("dummy_ospred.csv", header = TRUE)
dataset = dataset[!is.na(dataset$EVL1N),]

total_death <- length(unique(dataset$SID[which(dataset$CS_OS == 0)])) 

args <- commandArgs(trailingOnly = TRUE)

# test if there is at least one imput argument: if not, return an error
print(args)
if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).n", call. = FALSE)
} else {
  longest_survival <- as.numeric(args[[1]])
  cutoff_i <- as.numeric(args[[2]])
  
}

source("datacutoff_4JM.R")
save.image(file = paste0("result", longest_survival, "_", cutoff_i, ".RData"))

