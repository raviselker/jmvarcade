scrollify = function (text, sentenceLength) {
    
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
}

combine = function (texts, spaces) {
    
    lengths <- sapply(texts, length)
    
    for (i in 1:length(texts)) {
        texts[[i]] <- sprintf(paste0("%-", max(nchar(texts[[i]])),"s"), texts[[i]])
        texts[[i]][(1:max(lengths))[-(1:lengths[i])]] <- paste0(rep(" ", max(nchar(texts[[i]]))), collapse = "")
    }
    
    lines <- c()
    for (i in 1:max(lengths))
        lines[i] <- paste0(sapply(texts, function (x) return(x[[i]])), collapse = paste0(rep(" ", spaces), collapse = ""))
    
    return(paste0(lines, collapse="\n"))
}