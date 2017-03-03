load('coefList')
load('muList')
load('sigmaList')

logistic_predict_HQ <- function(onesam, flag='liantong', NAm=2){
        
        flags <- c('liantong','yidong','idcard')
        
        k <- ifelse(flag %in% flags, which(flag==flags), 3)
        if(NAm == 1) onesam[is.na(onesam)] <- 0
        if(NAm == 2){
                subs <- which(is.na(onesam))
                for(u in subs) onesam[u] <- rnorm(1, muList[[k]][u], sigmaList[[k]][u])
        }
        
        alp <- sum(coefList[[k]] * c(1,as.numeric(onesam)))
        
        s <- 1/(1+exp(-alp))       

        s
}


getOperator <- function(phoneNum){
        
        
}
