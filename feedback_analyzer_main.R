library('rmarkdown')
library('knitr')

## Locate and read the file
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript feedback_analyzer_main.R spreadsheetName outputFileName addLikedWords addImprovedWords \n
       Enter additional words as a single string with each word separated by a comma. \n
       Only single words supported at this time. Enter NULL if either addLikedWords or addImprovedWords is not desired.")
}
spreadsheetName <- args[1]
output <- args[2]

if (args[3] == "NULL") {
  addLikedWords <- NULL
} else {
  addLikedWords <- strsplit(args[3], ",")[[1]]
}

if(args[4] == "NULL") {
  addImprovedWords <- NULL
} else {
  addImprovedWords <- strsplit(args[4], ",")[[1]]
}

print(addLikedWords)
print(addImprovedWords)

cat("Analysing", spreadsheetName)
render('feedback_analyzer.R', output_file = output)