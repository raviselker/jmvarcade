
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
            
            path <- system.file("text/", package = "jmvgames")
            
            stages <- read.csv(paste0(path, "stages.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            words <- read.csv(paste0(path, "words.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            win <- read.csv(paste0(path, "win.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            lost <- read.csv(paste0(path, "lost.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            
            var <- self$options$letters
            
            wordTemp <- self$results$get('word')
            
            if (wordTemp$content == "")
                wordTemp$content <- sample(words, 1)
            
            word <- wordTemp$content
            
            intro1 <- private$.scrollify("To play this game you first need to drag an empty variable to the \"Letters\" box", 40)
            intro2 <- private$.scrollify(paste0("Good job! Now, it\'s time to go back to the spreadheet editor and start typing letters in column \"", var ,"\". \n\nHave fun!"), 40)
            
            stage0 <- unlist(strsplit(stages[1], split = "\n"))
            word0 <- paste0(rep("_", nchar(word)), collapse = " ")
            
            if (is.null(var)) {
                
                intro <- unlist(strsplit(intro1, split = "\n"))
                results$content <- private$.combine(list(c(stage0, "", word0), intro), spaces = 10)
                
            } else if (all(is.na(self$data[[var]]))) {
                
                intro <- unlist(strsplit(intro2, split = "\n"))
                results$content <- private$.combine(list(c(stage0, "", word0), intro), spaces = 10)
                
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
                    
                    results$content <- private$.combine(list(c(stage, "", wordShown), lostText), spaces = 10)
                    
                } else if (WIN) {
                    for (i in 1:12) {
                        stage <- unlist(strsplit(stages[1], split = "\n"))
                        winText <- unlist(strsplit(win[(i %% 2) + 1], split = "\n"))
                        
                        results$content <- private$.combine(list(c(stage, "", wordShown), winText), spaces = 10)
                        private$.checkpoint()
                        Sys.sleep(0.4)
                    }
                } else {
                    stage <- unlist(strsplit(stages[mistakes + 1], split = "\n"))
                    scoreScroll <- unlist(strsplit(private$.scrollify(score, 40), split = "\n"))
                    
                    results$content <- private$.combine(list(c(stage, "", wordShown), scoreScroll), spaces = 10)
                }
            }
        },
        .scrollify = function (text, sentenceLength) {
            
            sentences <- strwrap(text, sentenceLength)
            maxSentence <- max(nchar(sentences))
            
            sentences <-  sprintf(paste0("%-", sentenceLength,"s"), sentences)
            
            scroll <- c()
            
            scroll[1] <- paste(" ", paste0(rep("_", sentenceLength + 5), collapse = ""), collapse = "")
            scroll[2] <- paste("/\\", paste0(rep(" ", sentenceLength + 3), collapse = ""), "\\", collapse = "")
            scroll[3] <- paste("\\_|", paste0(rep(" ", sentenceLength + 2), collapse = ""), "|", collapse = "")
            
            for (i in 1:length(sentences))
                scroll[length(scroll) + 1] <- paste0("  |  ", sentences[[i]], "  |", collapse = "")
            
            scroll[length(scroll) + 1] <- paste0("  |  ", paste(rep(" ", sentenceLength), collapse = ""), "  |", collapse = "")
            scroll[length(scroll) + 1] <- paste0("  |  ", paste(rep("_", sentenceLength + 2), collapse = ""), "|_", collapse = "")
            scroll[length(scroll) + 1] <- paste0("  \\_/", paste(rep("_", sentenceLength + 4), collapse = ""), "/", collapse = "")
            
            return(paste0(scroll, collapse="\n"))
        },
        .combine = function (texts, spaces) {
            
            lengths <- sapply(texts, length)
            
            for (i in 1:length(texts)) {
                texts[[i]] <- sprintf(paste0("%-", max(nchar(texts[[i]])),"s"), texts[[i]])
                texts[[i]][(1:max(lengths))[-(1:lengths[i])]] <- paste0(rep(" ", max(nchar(texts[[i]]))), collapse = "")
            }
            
            lines <- c()
            for (i in 1:max(lengths))
                lines[i] <- paste0(sapply(texts, function (x) return(x[[i]])), collapse = paste0(rep(" ", spaces), collapse = ""))
            
            return(paste0(lines, collapse="\n"))
        })
)
