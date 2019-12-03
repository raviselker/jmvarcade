
# This file is a generated template, your changes will not be overwritten

blackjackClass <- R6::R6Class(
    "blackjackClass",
    inherit = blackjackBase,
    private = list(
        .run = function() {
            
            player <- self$results$player
            dealer <- self$results$dealer
            instructions <- self$results$instructions
            
            path <- system.file("text", package = "jmvarcade")
            loadedCards <- read.csv(file.path(path, "cards.csv"), header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
            templateCard <- loadedCards[1,1]
            flippedCard <- unlist(strsplit(loadedCards[2,1], "\n"))
            
            win <- read.csv(file.path(path, "win.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            lost <- read.csv(file.path(path, "lost.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            tie <- read.csv(file.path(path, "tie.csv"), header = FALSE, stringsAsFactors = FALSE)[,1]
            
            cmdsName <- self$options$cmds
            
            if ( ! is.null(cmdsName))
                cmds <- na.omit(self$data[[cmdsName]])
            
            if (is.null(cmdsName)) {
                
                instructions$content <- scrollify("To play this game you first need to drag an empty variable to the \"Commands\" box", 40)
                player$content <- combine(list(flippedCard, flippedCard), spaces=0)
                dealer$content <- combine(list(flippedCard, flippedCard), spaces=0)
            
            } else if (length(cmds) == 0 || tolower(cmds[1]) != "start") {
                
                instructions$content <- scrollify(paste0("Good job! Now, it\'s time to go back to the spreadheet editor. Type \"start\" in the first row of  \"", cmdsName ,"\" to start the game."), 40)
                player$content <- combine(list(flippedCard, flippedCard), spaces=0)
                dealer$content <- combine(list(flippedCard, flippedCard), spaces=0)
                
            } else {
                
                instructions$content <- scrollify(paste0("Type \"hit\" in the next row to receive another card or \"stand\" to stand down"), 40)
                
                if ( ! is.null(player$state)) {
                    
                    state <- player$state
                    deck <- state$deck
                    cPlayer <- state$cPlayer
                    cDealer <- state$cDealer
                    
                } else {
                    
                    deck <- private$.initDeck()$deck
                    c <- sample(1:52)
                    cPlayer <- c[1:26]
                    cDealer <- c[27:52]
                    player$setState(list(deck=deck, cPlayer=cPlayer, cDealer=cDealer))
                    
                }
                
                hits <- sum(cmds == "hit")
                stand <- sum(cmds == "stand") > 0
                
                playerCards <- list()
                scoresPlayer <- NULL
                for (i in 1:(hits + 2)) {
                    card <- cPlayer[i]
                    space <- ifelse(nchar(deck[card,1]) == 1, " ", "")
                    playerCards[[i]] <- unlist(strsplit(jmvcore::format(templateCard, deck[card,1], space, deck[card,2], space ,deck[card,1]), "\n"))
                    
                    scoresPlayer <-  c(scoresPlayer, deck$value[card])
                    scorePlayer <- private$.scores(scoresPlayer)
                    if (scorePlayer >= 21)
                        break()
                }
                
                pointsContent <- paste0("Points: ", scorePlayer)
                if (scorePlayer > 21)
                    pointsContent <- paste0(pointsContent, ", BUSTED\n")
                else
                    pointsContent <- paste0(pointsContent, "\n")

                cardsContent <- combine(playerCards,spaces=0)
                player$content <- paste0(pointsContent, cardsContent)
                
                dealerCards <- list()
                
                if (stand || scorePlayer >= 21) {
                    
                    i <- 1
                    GO <- TRUE
                    scoresDealer <- 0
                    while(GO) {
                        
                        card <- cDealer[i]
                        space <- ifelse(nchar(deck[card,1]) == 1, " ", "")
                        dealerCards[[i]] <- unlist(strsplit(jmvcore::format(templateCard, deck[card,1], space, deck[card,2], space ,deck[card,1]), "\n"))
                        
                        scoresDealer <-  c(scoresDealer, deck$value[card])
                        scoreDealer <- private$.scores(scoresDealer)
                        if (scoreDealer > 16)
                            GO <- FALSE
                        
                        pointsContent <- paste0("Points: ", scoreDealer)
                        if (scoreDealer > 21)
                            pointsContent <- paste0(pointsContent, ", BUSTED\n")
                        else
                            pointsContent <- paste0(pointsContent, "\n")
                            
                        cardsContent <- combine(dealerCards,spaces=0)
                        dealer$content <- paste0(pointsContent, cardsContent)

                        if (i > 1) {
                            private$.checkpoint()
                            Sys.sleep(1)
                        }
                        
                        if ( ! GO) {
                            
                            if ((scorePlayer > scoreDealer && scorePlayer <= 21) || (scorePlayer <= 21 && scoreDealer > 21)) {
                                
                                for (i in 1:12) {
                                    winText <- unlist(strsplit(win[(i %% 2) + 1], split = "\n"))
                                    
                                    instructions$content <- winText
                                    private$.checkpoint()
                                    Sys.sleep(0.4)
                                }
                            } else if ((scoreDealer > scorePlayer && scoreDealer <= 21) || (scoreDealer <= 21 && scorePlayer > 21)) {
                                
                                winText <- unlist(strsplit(lost[1], split = "\n"))
                                instructions$content <- winText
                                private$.checkpoint()
                                
                            } else {
                                
                                winText <- unlist(strsplit(tie[1], split = "\n"))
                                instructions$content <- winText
                                private$.checkpoint()
                                
                            }
                        }
                        
                        i <- i + 1
                    }
                    
                } else {
                    
                    card <- cDealer[1]
                    space <- ifelse(nchar(deck[card,1]) == 1, " ", "")
                    dealerCards[[1]] <- unlist(strsplit(jmvcore::format(templateCard, deck[card,1], space, deck[card,2], space, deck[card,1]), "\n"))
                    dealerCards[[2]] <- flippedCard
                    
                    scoreDealer <- ifelse(deck$value[card] == 1, 11, deck$value[card])
                    
                    pointsContent <- paste0("Points: ", scoreDealer, "\n")
                    cardsContent <- combine(dealerCards,spaces=0)
                    dealer$content <- paste0(pointsContent, cardsContent)
                }
            }
        },
        .initDeck = function() {
            
            # initialize card deck
            suits <- c("\u2666","\u2665","\u2660","\u2663")
            cards <- c("A", "2", "3", "4","5", "6", "7", "8", "9", "10", "J", "Q", "K")
            values <- c(1, 2:9, rep(10, 4))
            deck <- expand.grid(cards=cards, suits=suits, stringsAsFactors = FALSE)
            deck$value <- values
            
            nCards <- dim(deck)[1]
            
            return(list(deck=deck, nCards=nCards))
            
        },
        .scores = function(scores) {
            
            aces <- which(scores == 1)
            
            if (length(aces) > 0) {
                scoresTemp <- scores
                scoresTemp[aces[1]] <- 11
                
                if (sum(scoresTemp) <= 21)
                    scores <- scoresTemp
            }
            
            return(sum(scores))
        }),
    public=list(
        asSource=function() {
            
            paste0("This module does not support syntax mode.")
            
        })    
)
