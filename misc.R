library(data.tree)

## only for test
test <- function(samflag){
        
        setwd("D:/data/中行个人征信/中行个人征信共享")
        source('D:/code/CreditCardTest/Credit_v0_1.R')
        source('D:/code/CreditCardTest/misc.R')
        options(stringsAsFactors = FALSE)
        
        ## 样本数据矩阵
        samples <- readSample()
        samtmp <- name_ID(samples[,1],samples[,2])
        
        ##删除含有司法信息的好样本
        siFanames <- read.delim("涉及司法信息-0729.txt",sep='\t',header = TRUE)
        tmp <- name_ID(siFanames[,1],siFanames[,2])
        ## 划分好坏样本
        samlab <- goodbadSample(flag=samflag) ### 好坏样本划分标准!!!!!
        labtmp <- name_ID(samlab[,1],samlab[,2])
        samlab <- samlab[!((labtmp %in% tmp) & samlab[,3]==0), ]
        labtmp <- name_ID(samlab[,1],samlab[,2])
        
        ## 按照坏样本和好样本排序
        interSam <- intersect(samtmp,labtmp)
        interSam <- labtmp[labtmp %in% interSam]
        samples <- samples[match(interSam,samtmp), ]
        n.bad <- sum(samlab[labtmp %in% interSam,3] == 1)
        n.sam <- nrow(samples)
        
        ### 运行所有样本的打分
        samsN <- unlist(samples[,1])
        outputN <- unlist(read.csv("所有输出字段表.csv",header = F)[,1])
        rawfieN <- unlist(read.csv("所有原始字段表.csv",header = F)[,1])
        newfieN <- unlist(read.csv("所有衍生字段表.csv",header = F))
        treeNodes <- read.table("AllTreesNodes.txt")
        treeNodes <- rbind(cbind(c("姓名","身份证号","手机号"),1),treeNodes,cbind(c("社会关系","身份特质","信用历史","行为偏好","履约能力","综合","一票否决"),1))
        
        aa <- matrix(-1,28,n.sam)
        aasubs <- c(110,113,117,74,86,73,50,53,57,2,26,1,160,154,139,157,128,127) #!!!!!
        bb <- matrix(-1,60,n.sam)  ## yuanshiziduan
        cc <- matrix(-1,107,n.sam)  ## yanshengziduan
        dd <- matrix(-1,107,n.sam)  ## yanshengziduan score
        ee <- matrix(-1,173,n.sam) ## all scores
        
        colnames(aa) <- samsN
        colnames(bb) <- samsN
        colnames(cc) <- samsN
        rownames(aa) <- outputN
        rownames(bb) <- rawfieN
        rownames(cc) <- c("姓名","身份证号","手机号",newfieN)
        rownames(dd) <- c("姓名","身份证号","手机号",newfieN)
        
        tmp0 <- 1:nrow(samples)
        for(i in 1:nrow(samples)){
                #print(i)
                fields0 <- samples[i,]
                tmp  <- Credit_v0_1(fields0)
                aa[,i] <- unlist(tmp$result[c(1:3,aasubs+3,167:173)])
                
                bb[,i] <- unlist(c(fields0[1:3], tmp$fields0))
                cc[,i] <- unlist(c(fields0[1:3], tmp$fields1))
                dd[,i] <- unlist(c(fields0[1:3], tmp$scores1))
                ee[,i] <- unlist(tmp$result)
                tmp0[i] <- tmp$result[172]
        }
        ee <- cbind(treeNodes,ee)
        write.csv(aa,file=paste("OldOutput/样本输出矩阵_hq",samflag,".csv",sep=""),quote=FALSE)
        write.table(bb,file=paste("OldOutput/原始指标矩阵_hq",samflag,".xls",sep=""),quote=FALSE,sep="\t")
        write.table(t(bb),file=paste("OldOutput/原始指标矩阵转置_hq",samflag,".xls",sep=""),quote=FALSE,sep="\t")
        write.csv(cc,file=paste("OldOutput/衍生指标矩阵_hq",samflag,".csv",sep=""),quote=FALSE)
        write.csv(dd,file=paste("OldOutput/衍生打分矩阵_hq",samflag,".csv",sep=""),quote=FALSE)
        write.csv(ee,file=paste("OldOutput/所有打分矩阵_hq",samflag,".csv",sep=""),quote=FALSE)

}

testmethods <- function(samflag,scoreflag=0,mystr="hq"){
        #### test different methods
        library(adabag)
        library(rpart)
        library(e1071)
        library(randomForest)
        library(party)
        library(zoo)
        source('D:/code/CreditCardTest/Credit_v0_1.R')
        
        nbads <- c(28+18,81+18)
        nsams <- c(420+18,540+18)
        
        n.sam <- nsams[samflag]
        n.bad <- nbads[samflag]
        labs=c(1,0)
        y <- c(rep(labs[2],n.bad),rep(labs[1],n.sam-n.bad))
        
        
        K=4
        cvlist <- sample.cross_split(K,n.bad,n.sam)
        #cvlist <- sample.cross(n.sam,K)
        pVM <- c()
        ksV <- c()
        methodnames <- c("BruteForce","adaBoost","rpart","SVM","randomForest","randomForestCtree","xgboost","glm","adaBoostFilled","rpartFilled","merge_adaBoost","merge_RF","weighteds")
        runMs <- c(1,5,6,9)
        
        for(mflag in runMs){
                if(mflag==1){
                        aa <- as.matrix(read.csv(paste("OldOutput/样本输出矩阵_hq",samflag,".csv",sep="")))
                        aa <- aa[,-1]
                        outputN <- unlist(read.csv("所有输出字段表.csv",header = F)[,1])
                        bb <- aa[c(9,15,21), ]
                        mode(bb) <- "numeric"
                        x <- t(bb)
                }
                
                if(mflag > 1 ){
                        yanshengM <- as.matrix(read.csv(paste("OldOutput/衍生指标矩阵_hq",samflag,".csv",sep="")))
                        tmp <- as.matrix(read.csv(paste("OldOutput/衍生打分矩阵_hq",samflag,".csv",sep="")))
                        yanshengM[c(9,10,37,73,102)-1, ] <- tmp[c(9,10,37,73,102)-1, ]
                        addx <- yanshengM[1:3, -1]
                        yanshengM <- yanshengM[-(1:3), -1]
                        mode(yanshengM) <- "numeric"
                        x <- t(yanshengM)
                        
                        if(mflag>3){
                                x[x==Inf] <- 9999
                                x[is.na(x)] <- 0
                                # options(warn = -1)
                                # xyNew <- missingFill(x,y,flag=4.5,addx)
                                # xyNew <- as.matrix(xyNew)
                                # options(warn = 0)
                                # x <- xyNew[,-1]
                        }
                }
                
                print(mflag)
                oner <- allmethods(x,y,mflag,n.bad,n.sam,cvlist,K,plot=TRUE,labs=c(1,0))
                pVM <-  cbind(pVM,oner$pV)
                ksV <- c(ksV,oner$ks)
                print(oner$ks)
        }
        
        #### merge methods
        nm <- 1:ncol(pVM)
        # print(11)
        # oner <- allmethods(pVM[,nm],y,mflag=9,n.bad,n.sam,cvlist,K,plot=TRUE,labs=c(1,0))
        # print(oner$ks)
        # pVM <- cbind(pVM,oner$pV)
        # 
        # print(12)
        # oner <- allmethods(pVM[,nm],y,mflag=5,n.bad,n.sam,cvlist,K,plot=TRUE,labs=c(1,0))
        # print(oner$ks)
        # pVM <- cbind(pVM,oner$pV)
        
        print(13)
        oner <- list()
        ws <- 0.5*log(ksV/(1-ksV))
        oner$pV <- sapply(1:nrow(pVM), function(ix) weighted.mean(pVM[ix,nm],ws))
        oner$pV <- oner$pV/max(oner$pV)
        oner$ks <- KS_value(oner$pV[y==labs[1]], oner$pV[y==labs[2]],plot=FALSE)
        print(oner$ks)
        pVM <- cbind(pVM,oner$pV)
        
        
        runMs <- c(runMs,13)   
        
        if(scoreflag > 0){
                mode(pVM) <- "numeric"
                tmp <- pVM
                for(j in 1:ncol(pVM)){
                        tmp[,j] <- rank(-as.numeric(pVM[,j]))
                }
                tmp <- tmp/nrow(tmp)
                pVM <- cbind(pVM,tmp)
                
                aa <- as.matrix(read.csv(paste("OldOutput/样本输出矩阵_hq",samflag,".csv",sep="")))
                aa <- t(aa)
                aa <- aa[-1, ]
                alabs <- paste(aa[,1],aa[,2],sep="_")
                
                bb <- read.csv("goodbad.csv")
                blabs <- paste(bb[,1],bb[,2],sep="_")
                newlab <- bb[match(alabs,blabs),'bad4']
                
                pVM <- cbind(aa[,1:2],y,newlab,pVM)
                colnames(pVM) <- c("姓名","身份证号","标签","新的标签",methodnames[runMs],paste("topfraction",methodnames[runMs],sep="_"))
                write.table(pVM,file=paste("OldOutput/Scores259_flag_",samflag,"_",mystr,".txt",sep=""),quote=FALSE,row.names = FALSE,sep="\t")
                #return(pVM)
        }
        
}


### methods ===================================================

allmethods <- function(x,y,mflag,n.bad,n.sam,cvlist,K=4,plot=TRUE,labs=c(1,0)){

        pV <- 1:n.sam
        
        for(kk in 1:K){
                ## train data
                trainsub <- cvlist$train[[kk]]
                xt <- x[trainsub, ]
                yt <- y[trainsub]
                
                if(mflag %in% c(3,5,6,7,8,10)){
                        a1 <- data.frame(xt,yt)
                        colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                        rownames(a1) <- 1:nrow(a1)
                        
                        predisub <- cvlist$pred[[kk]]
                        newdata <- x[predisub, ]
                        colnames(newdata) <- paste("X",1:(ncol(a1)-1),sep="")
                }
                
                ## one train and test
                if(mflag==1){
                        ksV <- 1:10
                        KSM <- array(0,dim=c(10,10,10))
                        for(w1 in 1:10){
                                for(w3 in 1:10){
                                        for(w2 in 1:10){
                                                oneV <- sapply(1:nrow(xt), function(i) weighted.mean(c(xt[i,1],xt[i,2],xt[i,3]),w=c(w1,w2,w3),na.rm=TRUE) )
                                                KSM[w1,w2,w3] <- KS_curves(oneV[yt==1],oneV[yt==0])
                                        }
                                }
                                #print(w1)
                        }
                        kssub <- which(KSM==max(KSM),arr.ind = TRUE)
                        #print(kssub)
                        
                        predisub <- cvlist$pred[[kk]]
                        xp <- x[predisub,]
                        preModel <- sapply(1:nrow(xp), function(i) weighted.mean(c(xp[i,1],xp[i,2],xp[i,3]),w=ksV[kssub[1,]],na.rm=TRUE) )
                }
                
                if(mflag==2 | mflag==9){
                        ### adaBoost
                        yt <- as.character(yt)
                        options(stringsAsFactors = TRUE)
                        a1 <- data.frame(xt,yt)
                        colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                        rownames(a1) <- 1:nrow(a1)
                        adafit <- boosting(Class ~.,data=data.frame(a1), coeflearn="Zhu")
                        
                        # test:
                        predisub <- cvlist$pred[[kk]]
                        newdata <- x[predisub, ]
                        colnames(newdata) <- paste("X",1:(ncol(a1)-1),sep="")
                        preModel <- predict(adafit,newdata)$prob[,2]
                }
                
                if(mflag==3 | mflag==10){
                        #### rpart
                        fit <- rpart(Class ~.,data=a1)
                        preModel <- predict(fit, newdata=as.data.frame(newdata))
                }
                
                if(mflag==4){
                        yt <- as.character(yt)
                        options(stringsAsFactors = TRUE)
                        a1 <- data.frame(xt,yt)
                        colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
                        rownames(a1) <- 1:nrow(a1)
                        model <- svm(Class ~., data=a1,probability = TRUE)  
                        
                        # test:
                        predisub <- cvlist$pred[[kk]]
                        newdata <- x[predisub, ]
                        colnames(newdata) <- paste("X",1:(ncol(a1)-1),sep="")
                        tmp <- predict(model, newdata,probability = TRUE)
                        preModel <- attr(tmp,"probabilities")[ ,1]
                }
                
                if(mflag==5){
                        fit <- randomForest(x=a1[,1:(ncol(a1)-1)],y=as.factor(a1[,"Class"])) ### RandomForest method
                        preModel <- predict(fit,as.data.frame(newdata),type="prob")[,2]
                }
                
                if(mflag==6){ # randomForest method with ctree
                        fit <- cforest(Class ~., data = a1)
                        preModel <- predict(fit,newdata=as.data.frame(newdata),type="prob", OOB=TRUE)
                        names(preModel) <- NULL
                        preModel <- unlist(preModel)
                }
                
                if(mflag==7){
                        library(xgboost)
                        fit <- xgboost(data = as.matrix(xt), label = yt, max.depth = 5, eta = 1, nthread = 5, nround = 200, objective = "binary:logistic", verbose = 0)
                        preModel <- predict(fit,as.matrix(newdata))
                        ##fit <- mob(Class ~.,data=a1,model=glinearModel,family=binomial())
                }
                
                if(mflag==8){
                        fit <- glm(Class ~.,data=a1,family=binomial())
                        preModel <- predict(fit,as.data.frame(newdata), type = "response")
                }
                
                pV[predisub] <- preModel
        }
        
        
        if(plot){
                KS_curves(pV[y==labs[1]], pV[y==labs[2]],main="",plot=plot)
                ROCplot_hq(pV,y)
        }
        
        options(stringsAsFactors = FALSE) ## mflag=2
        ks <- KS_value(pV[y==labs[1]], pV[y==labs[2]],plot=FALSE)
        
        list(ks=ks,pV=pV)
}

###============================================================
readSample <- function(){
        samples <- read.delim("zhonghang_samples577.txt",header = TRUE,sep = "\t")
        samples
}

goodbadSample <- function(flag=1){
        ##bad1 代表标签标准1：当前有欠款=坏，当前无欠款、历史无违约历史=好，其他样本=NA; bad2 标准2：当前有欠款=坏，当前无欠款且（历史违约超过3次或最长超过5天）=坏，其他样本=好。
        
        # bad: 1; good: 0;
        
        a<-read.csv('hk.csv',header=T)
        a[,24]<-as.numeric(a[,24])
        a[,25]<-as.numeric(a[,25])
        a[,27]<-as.numeric(a[,27])
        bad1<- rep(-1,nrow(a))
        bad1[which(a[,24]>0)] <- 1
        bad1[which(a[,24]==0&a[,25]==0)] <- 0
        
        bad2<- rep(0,nrow(a))
        bad2[which(a[,24]>0|a[,25]>0)]<-1
        bad2[which(a[,24]==0&a[,25]<=3&a[,27]<=5)]<-0
        #bad2[a[,24]>0 | (a[,24]==0 & (a[,25]>3 | a[,27]>5)) ] <- 1
        b<-cbind(a,bad1,bad2)
        b<-b[,c(1,3,24,25,27,88,89)]
        
        if(flag==1){ c <- b[,c(1,2,6)]; c <- c[c[,3]>=0,]; }
        if(flag==2) c <- b[,c(1,2,7)]
        
        c <- c[order(-c[,3]), ]
        c <- c[!duplicated(paste(c[,1],c[,2],sep="_")),  ]
        
        #new 18 samples
        c1 <- read.delim("zhonghang_sample_label_18.txt",header = FALSE)
        colnames(c1) <- colnames(c)
        c <- rbind(c1,c)
        
        c <- c[order(-c[,3]), ]
        
        c
}

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

sample.cross_split <- function(K,n.bad,n.sam){
        
        train <- list()
        pred <- list()
        
        list1 <- sample.cross(n.bad,K)
        list2 <- sample.cross(n.sam-n.bad,K)
        
        for(i in 1:K){
                train[[i]] <- c(list1$train[[i]], list2$train[[i]]+n.bad)
                pred[[i]] <- c(list1$pred[[i]], list2$pred[[i]]+n.bad)
        }
        
        list(train=train,pred=pred)
}

missingFill <- function(x,y,flag=1,addx=""){ 
        
        if(flag==1) return(rfImpute(x,y))
        if(flag==2){
                for(k in 1:ncol(x)) x[is.na(x[,k]), k] <-  mean(x[!is.na(x[,k]), k])
                return(cbind(y,x))
        }
        if(flag==3){
                labs=unique(y)
                for(k in 1:ncol(x)){
                        x[y==labs[1], k] <- na.approx(x[y==labs[1], k],na.rm=FALSE)
                        x[y==labs[2], k] <- na.approx(x[y==labs[2], k],na.rm=FALSE)
                        x[is.na(x[,k]), k] <-  mean(x[!is.na(x[,k]), k])
                }
                return(cbind(y,x))
        }
        
        if(flag==4){
                sexs <- sapply(1:ncol(addx), function(i) idcard_sex(addx[2,i]) )
                
                ages <- sapply(1:ncol(addx), function(i) idcard_age(addx[2,i]) )
                ageG <- cbind(18:59, c(rep(1,12),rep(2:4,each=10)) )
                xageg <- ages
                xageg <- ageG[match(ages,ageG[,1]),2]
                xageg[ages < 18] <- 0
                xageg[ages >= 60] <- 5
                groV <- paste(xageg,sexs,sep="_")
                uniG <- unique(groV)
                
                for(k in 1:ncol(x)){
                        for(j in uniG){
                                x[groV==j, k] <- na.fill_hq(x[groV==j, k])
                        }
                        x[is.na(x[,k]), k] <-  median(x[!is.na(x[,k]), k])
                }
                return(cbind(y,x))  
        }
        
        if(flag==4.5){
                
                sexs <- sapply(1:ncol(addx), function(i) idcard_sex(addx[2,i]) )
                
                ages <- sapply(1:ncol(addx), function(i) idcard_age(addx[2,i]) )
                ageG <- cbind(18:59, c(rep(1,12),rep(2:4,each=10)) )
                xageg <- ages
                xageg <- ageG[match(ages,ageG[,1]),2]
                xageg[ages < 18] <- 0
                xageg[ages >= 60] <- 5
                groV <- paste(xageg,sexs,sep="_")
                uniG <- unique(groV)
                
                #匹配上一年龄段同性别
                groV_up <- paste(xageg-1,sexs,sep="_")
                uniG_up <- unique(groV_up)
                u<-x
                #匹配下一年龄段同性别
                groV_dn <- paste(xageg+1,sexs,sep="_")
                uniG_dn <- unique(groV_dn)
                d<-x
                
                for(k in 1:ncol(x)){
                        for(j in uniG)     x[groV==j, k] <- na.fill_hq(x[groV==j, k])
                        for(j in uniG_up)  u[groV==j, k] <- na.fill_hq(u[groV==j, k])
                        for(j in uniG_dn)  d[groV==j, k] <- na.fill_hq(d[groV==j, k])
                        #u[is.na(u[,k]), k] <-  median(u[!is.na(u[,k]), k])
                        #d[is.na(d[,k]), k] <-  median(d[!is.na(d[,k]), k])
                        #匹配上一年龄段，最高段匹配结果NA
                        #匹配下一年龄段，最低段匹配结果NA
                        
                        x[is.na(x[,k]),k] <-u[is.na(x[,k]), k]
                        x[is.na(x[,k]),k] <-d[is.na(x[,k]), k]
                        #若弄年龄段无数，用上一年龄段补，若仍无数，用下一年龄段补
                        x[is.na(x[,k]), k] <-  median(x[!is.na(x[,k]), k])
                }
                return(cbind(y,x))  
        }
        
        
        
        if(flag==5){
                nasubs <- which(is.na(x), arr.ind = TRUE)
                fillx <- sapply(1:nrow(nasubs), function(k) na.fill_hq1(nasubs[k,],x) )
                x[cbind(nasubs[,1],nasubs[,2])] <- fillx
                #for(k in 1:ncol(x)) x[is.na(x[,k]), k] <-  mean(x[!is.na(x[,k]), k])
                return(cbind(y,x))  
        }
        
        if(flag==6){
                library(DMwR)  
                x <- knnImputation(x,k=5)
                return(cbind(y,x))
        }

}

na.fill_hq <- function(xx){
        xx[is.na(xx)] <- median(xx[!is.na(xx)])
        xx
}

na.fill_hq1 <- function(subs,x){
        # step1 : chose samples
        subs1 <- which(!is.na(x[,subs[2]]))
        # step2 : chose index
        subs2 <- which(!is.na(x[subs[1],]))
        xtmp <- x[subs1,subs2]
        tmpsubs2 <- !(apply(xtmp,2,FUN = anyNA))
        xtmp <- xtmp[ ,tmpsubs2]
        subs2 <- subs2[tmpsubs2]
        
        # step3 : normalize
        xtmp <- rbind(x[subs[1],subs2],xtmp)
        xtmp <- sapply(1:ncol(xtmp), function(k) normalize_hq(xtmp[,k]) )
        
        # step4 : filled by nearest neighbor 
        dV <- sapply(2:nrow(xtmp), function(k) dist_hq(xtmp[1,],xtmp[k,]) )
        fillsub <- subs1[which.min(dV)]
        as.numeric(x[fillsub,subs[2]])
}

normalize_hq <- function(xx){
       
        x1 <- xx[!is.na(xx)]
        if(sd(x1)==0) {
                xx
        }else{
                (xx-min(x1))/(max(x1)-min(x1))
        }
}

dist_hq <- function(x1,x2){
        sqrt(sum((x1-x2)^2))
}

ROCplot_hq <- function(score,y){
        library(ROCR)
        pred <- prediction(score,y)
        perf <- performance(pred,"tpr","fpr")
        plot(perf)
        #perf <- performance(pred,"auc")
}

name_ID <- function(name,id){
        #sapply(1:length(name), function(i) paste(name[i], substr(id[i],1,14),sep="_") )
        paste(name, id,sep="_")
}

KS_curves <- function(goodV,badV,main="",plot=FALSE){
        sample1 <- as.numeric(badV)
        sample1 <- sample1[!is.na(sample1)]
        sample2 <- as.numeric(goodV)
        sample2 <- sample2[!is.na(sample2)]
        
        x0 <- seq(1,0,-0.0001)
        y1 <- sapply(x0,function(x01) sum(sample1 >= x01))
        y1 <- y1/length(sample1)
        y2 <- sapply(x0,function(x01) sum(sample2 >= x01))
        y2 <- y2/length(sample2)
        xv <- which.max(y2-y1)
        
        if(plot){
                print(max(y2-y1))
                plot(1:length(x0),y1,  col=1,lwd=1,xaxt="n",type="l",xlab="Score",ylab="Fn(x)",main=main,ylim=c(0,1))
                lines(1:length(x0),y2, col=2,lwd=1,type="l")
                abline(v=xv,col="blue",lwd=2)
                legend("bottomright",legend=c("坏样本","好样本"),col=1:2,lwd=2)
                text(xv,y=0.5,round(max(y2-y1),4))
                axis(1,at=seq(1,length(x0),length.out=10),labels=round(seq(1,0,length.out = 10),2))
        }
        
        ks <- max(y2-y1)
        ks
}

KS_value <- function(goodV,badV,plot=FALSE){
        badV <- as.numeric(badV[!is.na(badV)])
        goodV <- as.numeric(goodV[!is.na(goodV)])
        
        library(ROCR)
        pred <- prediction( c(badV,goodV),c(rep(0,length(badV)),rep(1,length(goodV))) )
        perf <- performance(pred,"tpr","fpr")
        if(plot) plot(perf)
        max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
}

