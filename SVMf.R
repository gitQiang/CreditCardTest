source('D:/code/CreditCardTest/Credit_v0_1.R')

sample.cross <- function(nsample,K){
        
        train_sample <- list()
        pred_sample <- list()
        
        nk <- floor(nsample/K)
        sam <- sample(nsample)
        #subsK <- matrix(K-1,nk)
        #subK <- sam[((K-1)*nk+1):nsample]
        
        for(i in 1:(K-1)){
                pred_sample[[i]] <- sam[((i-1)*nk+1):(i*nk)]
                train_sample[[i]] <- setdiff(sam,pred_sample[[i]])
        }
        pred_sample[[K]] <- sam[((K-1)*nk+1):nsample]
        train_sample[[K]] <- setdiff(sam,pred_sample[[K]])
        
        
        list(train=train_sample,pred=pred_sample)
}

### SVM
library(e1071)
library(zoo)

n.bad=100
n.sam=504
load("outputIndex504")
mode(aa) <- "numeric"
valM <- aa[c(4:8,10:14,16:20), ]
valM <- t(valM)
valM <- cbind(valM,c(rep(0,n.bad),rep(1,n.sam-n.bad)))
colnames(valM)[1:15] <- paste(colnames(valM)[1:15],1:15,sep="_")
colnames(valM)[16] <- "±êÇ©"
valM <- as.matrix(valM)

for(k in 1:ncol(valM)){
        valM[,k] <- na.approx(valM[,k],na.rm=FALSE)
        valM[is.na(valM[,k]), k] <-  mean(valM[!is.na(valM[,k]), k])
}

# tmp <- valM
# library(RMySQL)
# library(DBI)
# con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
# #dbRemoveTable(con,"zhonghang_index15")
# dbWriteTable(con, "zhonghang_index15_fill", as.data.frame(tmp))
# dbDisconnect(con)

K <- 4
cvlist <- sample.cross(n.sam,K)
pV <- 1:n.sam

for(i in 1){ #:K){
        trainsub <- cvlist$train[[i]]
        x <- valM[trainsub,1:15]
        y <- valM[trainsub,16]
        model <- svm(x,y)    
        
        # compute decision values and probabilities:
        #pred <- predict(model, x, decision.values = TRUE)
        #print(attr(pred, "decision.values")[1:4,])
        
        # test:
        predisub <- cvlist$pred[[i]]
        newdata <- valM[predisub,1:15]
        preModel <- predict (model, newdata)
        pV[predisub] <- preModel
}

#KS_curves(pV[(n.bad+1):n.sam], pV[1:n.bad],main="",plot=TRUE)

y1 <- valM[predisub,16]
KS_curves(preModel[y1==1], preModel[y1==0],main="",plot=TRUE)
