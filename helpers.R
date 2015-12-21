## Helper functions

## Process the text
## corpus: corpus object produced by tm
processCorpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"),  c("nothing", "na", "none", "yes", "no", "everything", "hey")))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, stripWhitespace)
  
  ## Combine together words with similar meanings
  for (j in seq(corpus)) {
    corpus[[j]] <- gsub("magic box", "magic_box", corpus[[j]])
    corpus[[j]] <- gsub("teaching assistant", "teaching_assistant", corpus[[j]])
    corpus[[j]] <- gsub("regular express", "regular_expression ", corpus[[j]])
    corpus[[j]] <- gsub("regex", "regular_expression ", corpus[[j]])
  }
  corpus <- tm_map(corpus, PlainTextDocument)
  return(corpus)
}

## Statistical functions for t-tests

## Compute correlation needed to achieve statistic significance for
## p at sample size n
## p: p-value
## n: sample size
computeSigCor <- function(p, n) {
  t <- qt(1 - p, n)
  cor <- sqrt(t^2 / (n-2 + t^2))
  return(cor)
}

## Compute p-value for particular correlation
## r: correlation coefficient
## n: sample size
computePValue <- function(r, n) {
  t <- r * sqrt((n-2) / (1-r^2))
  return(dt(t, n - 2))
}

## Find associations
## td: term document matrix
## wordList: word list
findAllAssocs <- function(td, wordList, cor) {
  for (w in wordList) {
    cat("Words associated with", w, "\n")
    print(findAssocs(td, w, cor))
    cat("\n")
  }
}