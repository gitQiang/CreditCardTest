## author: Huang Qiang 2017/2/28
setwd("D:/data/中行个人征信/赵旭/程序")
## missing values and feature selection ?????

# main process
source("DataClean.R", encoding = 'UTF-8')
source("logistic_predict_HQ.R")

# read input data  
dt.liantong <- read.csv('data/liantong_label.csv', fileEncoding='UTF-8',header=TRUE, check.names = FALSE)
dt.yidong <- read.csv('data/yidong_label.csv', fileEncoding='UTF-8', header=TRUE, check.names = FALSE)
dt.idcard <- read.csv('data/basic_label.csv', fileEncoding='UTF-8',header=TRUE, check.names = FALSE)

dt.liantong <- dt.liantong[,4:ncol(dt.liantong)]
dt.yidong <- dt.yidong[,4:ncol(dt.yidong)]
dt.idcard <- dt.idcard[(!dt.idcard$身份证号 %in% dt.liantong$身份证号) & (!dt.idcard$身份证号 %in% dt.yidong$身份证号), 4:ncol(dt.idcard)]

dt_liantong <- DataClean_Train(dt.liantong, flag="liantong")
dt_yidong <- DataClean_Train(dt.yidong, flag="yidong")
dt_idcard <- DataClean_Train(dt.idcard, flag="idcard")

dtList <- list()
dtList[[1]] <- dt_liantong
dtList[[2]] <- dt_yidong
dtList[[3]] <- dt_idcard[sample(nrow(dt_idcard),1000), ]


### density functions for different labels
plot=FALSE
xx <- seq(0,1,0.000001)
pv1 <- dlnorm(1/(1-xx) - 1,0,3)  #
pv2 <- dnorm(xx,0.5,0.1)
pv3 <- dlnorm(1/xx - 1,0,3)

if(plot){
        plot(xx,a1,type='l',col=1,ylim=c(0,5))
        lines(xx,a2,type='l',col=2)
        lines(xx,a3,type='l',col=3)
}


### random training models
coefList <- list()
muList <- list()
sigmaList <- list()
nK <- 100

for(dsub in 1:3){
        onedt <- dtList[[dsub]]
        X <- onedt[,-which(names(onedt)=='label')]
        nf <- ncol(onedt)
        coeM <- matrix(0,nK, nf)
        
        for(i in 1:nK){
                if(dsub==3){
                        onedt <- dt_idcard[sample(nrow(dt_idcard),1000), ]
                        X <- onedt[,-which(names(onedt)=='label')]           
                }
                print(i)
                slabel <- rep(0,nrow(onedt))
                slabel[onedt[,'label']==0] <- sample(xx,sum(onedt[,'label']==0),prob=pv3)
                slabel[onedt[,'label']==0.5] <- sample(xx,sum(onedt[,'label']==0.5),prob=pv2)
                slabel[onedt[,'label']==1] <- sample(xx,sum(onedt[,'label']==1),prob=pv1)
                ### get feature weights
                tdata = cbind(X,slabel)
                #fit <- glm("slabel ~ .", family = binomial(link = "logit"), data=tdata)
                tdata[,'slabel'] <- log(tdata[,'slabel']/(1-tdata[,'slabel']))
                fit <- lm("slabel ~ .", data=tdata)
                coeM[i, ] <- fit$coefficients
        }
        
        oneCoef <- sapply(1:nf, function(ii) mean(sort(coeM[,ii])[6:(nK-5)]))
        fillmu <- colMeans(X,na.rm = TRUE)
        fillsigma <- sapply(1:ncol(X), function(ii) sd(X[,ii],na.rm = TRUE))
        
        coefList[[dsub]] <- oneCoef
        muList[[dsub]] <- fillmu
        sigmaList[[dsub]] <- fillsigma
}

save(coefList, file='coefList')
save(muList, file='muList')
save(sigmaList, file='sigmaList')



### try to predict 
#Input: test on validation samples, one-by-one with flags
#Output: test scores
dliantong <- DataClean_Predict(dt.liantong, flag="liantong")
dyidong <- DataClean_Predict(dt.yidong, flag="yidong")
didcard <- DataClean_Predict(dt.idcard, flag="idcard")

s1 <- sapply(1:nrow(dliantong), function(ii) logistic_predict_HQ(dliantong[ii, ], 'liantong'))
s2 <- sapply(1:nrow(dyidong), function(ii) logistic_predict_HQ(dyidong[ii, ], 'yidong'))
subs <- sample(nrow(didcard), 1000)
s3 <- sapply(subs, function(ii) logistic_predict_HQ(didcard[ii, ], 'idcard'))

hist(s1)
hist(s2)
hist(s3)



