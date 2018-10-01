inIntervall <- function(sampleVector, borderVectors){
    dimension <- length(borderVectors[[1]])
    borderMatrix <- do.call("rbind",borderVectors)
    if(nrow(unique(borderMatrix))<dimension){
        stop("Not suitable vector")
    }
    minVec <- apply(borderMatrix, 2, min)
    maxVec <- apply(borderMatrix, 2, max)
    return(as.logical(prod(sampleVector>=minVec)*prod(sampleVector<=maxVec)))
}
