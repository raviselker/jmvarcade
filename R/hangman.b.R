
# This file is a generated template, your changes will not be overwritten

#' @rdname jamovi
#' @export
hangmanClass <- R6::R6Class(
    "hangmanClass",
    inherit = hangmanBase,
    private = list(
        .init = function() {
            
            word <- jmvcore::Preformatted$new(self$options, 'word', visible = FALSE, clearWith = NULL)
            self$results$add(word)
            
        },
        .run = function() {
            
            results <- self$results$get('text')
            
            path <- system.file("text", package = "jmvgames")

            stages <- read.csv(paste0(path, "/stages.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            words <- read.csv(paste0(path, "/words.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            win <- read.csv(paste0(path, "/win.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            lost <- read.csv(paste0(path, "/lost.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            
            var <- self$options$letters
            
            wordTemp <- self$results$get('word')
            
            if (wordTemp$content == "")
                wordTemp$content <- sample(words, 1)
            
            word <- wordTemp$content
            
            intro1 <- scrollify("To play this game you first need to drag an empty variable to the \"Letters\" box", 40)
            intro2 <- scrollify(paste0("Good job! Now, it\'s time to go back to the spreadheet editor and start typing letters in column \"", var ,"\". \n\nHave fun!"), 40)
            
            stage0 <- unlist(strsplit(stages[1], split = "\n"))
            word0 <- paste0(rep("_", nchar(word)), collapse = " ")
            
            if (is.null(var)) {
                
                intro <- unlist(strsplit(intro1, split = "\n"))
                results$content <- combine(list(c(stage0, "", word0), intro), spaces = 10)
                
            } else if (all(is.na(self$data[[var]]))) {
                
                intro <- unlist(strsplit(intro2, split = "\n"))
                results$content <- combine(list(c(stage0, "", word0), intro), spaces = 10)
                
            } else {
                
                lettersGuessed <- na.omit(self$data[[var]])
                
                lettersWord <- unlist(strsplit(x = word, split = NULL))
                lettersWord <- casefold(lettersWord, upper = TRUE)
                
                lettersGuessed <- casefold(lettersGuessed, upper = TRUE)
                
                WIN <- FALSE
                if (all (lettersWord %in% lettersGuessed))
                    WIN <- TRUE
                
                mistakes <- sum(! lettersGuessed %in% lettersWord)
                
                LOST <- FALSE
                if (mistakes > 6)
                    LOST <- TRUE
                
                lettersWord[ ! lettersWord %in% lettersGuessed] <- "_"
                wordShown <- paste(lettersWord, collapse = " ")
                
                correct <- unique(lettersGuessed[lettersGuessed %in% lettersWord])
                incorrect <- unique(lettersGuessed[! lettersGuessed %in% lettersWord])
                score <- paste0("Correct guesses: ", paste0(correct, collapse = " "), "\n\nIncorrect guesses: ", paste0(incorrect, collapse = " "))
                
                if (LOST) {
                    stage <- unlist(strsplit(stages[8], split = "\n"))
                    lostText <- unlist(strsplit(lost, split = "\n"))
                    
                    results$content <- combine(list(c(stage, "", wordShown), lostText), spaces = 10)
                    
                } else if (WIN) {
                    for (i in 1:12) {
                        stage <- unlist(strsplit(stages[1], split = "\n"))
                        winText <- unlist(strsplit(win[(i %% 2) + 1], split = "\n"))
                        
                        results$content <- combine(list(c(stage, "", wordShown), winText), spaces = 10)
                        private$.checkpoint()
                        Sys.sleep(0.4)
                    }
                } else {
                    stage <- unlist(strsplit(stages[mistakes + 1], split = "\n"))
                    scoreScroll <- unlist(strsplit(scrollify(score, 40), split = "\n"))
                    
                    results$content <- combine(list(c(stage, "", wordShown), scoreScroll), spaces = 10)
                }
            }
        }),
    public=list(
        asSource=function() {
            
            paste0("This module does not support syntax mode.")
            
        }) 
)
