#' ---
#' title: Analysis of survey results for `r spreadsheetName`
#' output:
#'    html_document:
#'       toc: true
#' ---       

#+ echo=F, eval=T, message=F, warning=F
library(tm)
library(RWeka)
source("helpers.R")
options(mc.cores = 1)
options(java.parameters = "-Xmx2048m")

## Reading the text
message("Reading and extracting text")
spreadsheet <- read.csv(spreadsheetName, na.strings = "", stringsAsFactors = FALSE)
spreadsheet <- spreadsheet[!(spreadsheet$Timestamp %in% c("AVG", "MED", "SDEV")),]
liked <- spreadsheet$What.did.you.like.most.about.the.lab.
improvement <- spreadsheet$How.do.you.think.the.lab.could.be.best.improved.

## Remove the non-comment items
liked <- liked[!is.na(liked)]
improvement <- improvement[!is.na(improvement)]

## Build a vector corpus and process
liked_corpus <- Corpus(VectorSource(liked))
improvement_corpus <- Corpus(VectorSource(improvement))
message("Processing text")
liked_corpus <- processCorpus(liked_corpus)
improvement_corpus <- processCorpus(improvement_corpus)

message("Building Term Document Matricies")
## Build Term Document Matrix
liked_td <- TermDocumentMatrix(liked_corpus)
improved_td <- TermDocumentMatrix(improvement_corpus)
liked_ngrams <- TermDocumentMatrix(liked_corpus, control =list(tokenizer = NGramTokenizer))
improved_ngrams <- TermDocumentMatrix(improvement_corpus, control =list(tokenizer = NGramTokenizer))

## Find and report longest comments
message("Finding longest comments")
liked_lengths <- sapply(liked, function (x) length(strsplit(x, " ")[[1]]))
improvement_lengths <- sapply(improvement, function (x) length(strsplit(x, " ")[[1]]))
quant_liked <- quantile(liked_lengths, 0.95)
quant_improve <- quantile(improvement_lengths, 0.95)

#' ## Longest Comments
#' 
#' 95% of comments for liked items have length <= `r quant_liked`.
#' Longest comment has length `r max(liked_lengths)`.

long_liked <- liked_lengths[which(liked_lengths >= quant_liked)]
comments <- names(long_liked)
for (comment in comments) {
  cat(comment, '\n')
  cat('\n')
}

#' 95% of comments for items to be improved have length <= `r quant_improve`
#' Longest comment has length `r max(improvement_lengths)`.

long_improvement <- improvement_lengths[which(improvement_lengths >= quant_improve)]
comments <- names(long_improvement)
for (comment in comments) {
  cat(comment, '\n')
  cat('\n')
}


## Find and report import n-grams
message("Performing association analysis")
liked_words <- c("like", "learn", "interest", addLikedWords)
improve_words <- c("improve", "more", "less", addImprovedWords)
cor <- 0.25
liked_p <- computePValue(cor, liked_td$ncol)
improved_p <- computePValue(cor, improved_td$ncol)

#' ## Association Analysis
#' ### Metadata
#' Found associations have correlation > 0.25 
#' 
#' From `r liked_td$ncol` responses for liked items
#' and `r improved_td$ncol` responses for items to be improved
#' 
#' P-value for associations with liked items: `r liked_p`
#' 
#' P-value for associations with items to be improved: `r improved_p`
#' 
#' All input words for associations with liked items: `r liked_words`
#' 
#' All input words for associations with items to be improved: `r improve_words`
#' 
#' ### Single World Association Analysis
#' 
#' #### Single Word Associations- Liked Items

findAllAssocs(liked_td, liked_words, cor)

#' #### Single World Associations - Items for Improvement
findAllAssocs(improved_td, improve_words, cor)

#' ### N-Gram Analysis
#' 
#' Note that n-grams generated, by default, were up to 3 words long.
#'  
#' #### N-Gram Associations- Liked Items
findAllAssocs(liked_ngrams, liked_words, cor)

#' #### N-Gram Associations- Items for Improvement
findAllAssocs(improved_ngrams, improve_words, cor)
