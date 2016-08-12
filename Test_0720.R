
## 构建原始字段=====================================
setwd("D:/data/中行个人征信/中行个人征信共享")

tmp <- c("移动近3个月","移动近12个月")
bb <- unlist(read.csv("字段_黄强临时移动.csv",header = FALSE, strip.white = TRUE))
bb <- unique(bb)
cc <- c()

for(i in 1:length(bb)){
     one4 <- paste(tmp,bb[i],sep="")
     cc <- c(cc,one4)
}
write.csv(cc,file="Dictionary_HQ_move.csv",quote=FALSE,row.names = FALSE)


tmp <- c("联通近3个月","联通近12个月")
bb <- unlist(read.csv("字段_黄强临时联通.csv",header = FALSE, strip.white = TRUE))
cc <- c()

for(i in 1:length(bb)){
        one4 <- paste(tmp,bb[i],sep="")
        cc <- c(cc,one4)
}
write.csv(cc,file="Dictionary_HQ_unicom.csv",quote=FALSE,row.names = FALSE)

## 手动修改一些不对的字段  ！！！！

a1 <- unlist(read.csv("Dictionary_HQ_move.csv",header=FALSE))
a2 <- unlist(read.csv("Dictionary_HQ_unicom.csv",header=FALSE))
a3 <- union(a1,a2)
write.csv(a3,file="Dictionary_HQ_raw_v0.csv",quote=FALSE,row.names = FALSE)


aa <- unlist(read.csv("Dictionary_HQ_raw_v0.csv"))
aa <- gsub("移动","",aa)
aa <- gsub("联通","",aa)
aa <- sort(unique(aa))
write.csv(aa,file="Dictionary_HQ_raw_v1.csv",quote=FALSE,row.names = FALSE)


### 处理字段的区间到得分的映射表=======================================
setwd("D:/data/中行个人征信/中行个人征信共享")
ziduan <- read.csv("金融画像衍生字段表0722——LY.csv")
ziduan <- ziduan[,1:5]
subs <- which(is.na(ziduan[,4]) & is.na(ziduan[,5]))
for(i in 1:(subs[1]-1)){
       if(ziduan[i,1]==""){
               ziduan[i,1:2] <- ziduan[i-1,1:2]
       }
}

newziduan <- c()
for(i in 1:length(subs)){
       tmp <- ziduan[subs[i],3]
       if(tmp!=""){
               tmp <- gsub(' ','',tmp)
               tmp <- unlist(strsplit(tmp,';'))
               for(j in 1:length(tmp)){
                       tmp1 <- unlist(strsplit(tmp[j],',')) 
                       if(grepl('<',tmp1[1])){
                               u1 <- gsub('<','',tmp1[1]) 
                               u1 <- gsub('=','',u1)
                               onev <- c("",u1,tmp1[2])
                       }else if(grepl('>',tmp1[1])){
                               u1 <- gsub('>','',tmp1[1]) 
                               u1 <- gsub('=','',u1)
                               onev <- c(u1,'',tmp1[2])
                       }else if(grepl('-',tmp1[1])){
                               u1 <- unlist(strsplit(tmp1[1],'-'))
                               onev <- c(u1[1],u1[2],tmp1[2])
                       }else if(tmp1[1]=="0"){
                              onev <- c("",0,tmp1[2]) 
                        }else{
                                for(kk in 1:3){
                                        tmp1[kk] <- gsub('\\(','',tmp1[kk])
                                        tmp1[kk] <- gsub(']','',tmp1[kk])
                                        tmp1[kk] <- gsub('\\+','',tmp1[kk])
                                }
                                onev <- tmp1
                              
                       }
                       onerow <- c(ziduan[subs[i],1:2],onev)
                       newziduan <- rbind(newziduan,onerow)
               }
       }else{
               onerow <- c(ziduan[subs[i],1:2],"","","")
               newziduan <- rbind(newziduan,onerow)
       }
}
colnames(newziduan)  <- colnames(ziduan)
newziduan <- rbind(ziduan[1:(subs[1]-1), ], newziduan)
newziduan[is.na(newziduan)] <- ""
newziduan <- as.matrix(newziduan)

write.csv(newziduan,file="金融画像衍生字段表_LY_0723.csv",quote=FALSE,row.names = FALSE)



### 树上所有点的权重 =======================================
source('D:/code/CreditCardTest/Credit_v0_1.R')
trees1 <- trees_construct()
tree1 <- trees1$tree1
wV1 <- tree1$Get("weight")

tree2 <- trees1$tree2
wV2 <- tree2$Get("weight")

tree3 <- trees1$tree3
wV3 <- tree3$Get("weight")

treeNodes <- c(names(wV1),names(wV2),names(wV3))
treeweigs <- c(wV1,wV2,wV3)
write.table(cbind(treeNodes,treeweigs),file="AllTreesNodes.txt",quote=FALSE,col.names = FALSE,row.names = FALSE)


### 民生坏样本===========================================
library(xlsx)
setwd("D:/data/中行个人征信/中行个人征信共享")
source('D:/code/CreditCardTest/Credit_v0.R')
samples <- read.xlsx2("数据样本0724.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")


sams <- read.xlsx2("民生坏样本.xls",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
sams <- sams[!duplicated(sams[,3]),]
# idxALL <- sams[,3]
# 
# 
# idx <- samples[,2]
# subs <- rep(-1,length(idx))
# for(i in 1:length(idx)){
#         tmp <- substr(idx[i],1,14)
#         if(sum(grepl(tmp,idxALL)) > 0) subs[i] <- idxALL[which(grepl(tmp,idxALL))] 
# }


### 所有好坏样本的打分=======================================
library(xlsx)
setwd("D:/data/中行个人征信/中行个人征信共享")
allS <- as.matrix(read.csv("所有打分矩阵.csv"))
onelist <- list()

#for(i in 3:10){
i=127
        print(i)
        plot(as.numeric(allS[i,4:52]),col=1,type="b",main=paste(allS[i,1],allS[i,2],sep="_"))
        abline(v=21.5,lwd=2,col=2)
        
        onelist[[1]] <- as.numeric(allS[i,4:24])
        onelist[[2]] <- as.numeric(allS[i,25:52])
        boxplot(onelist)
        
        boxplot.stats(onelist[[1]])
#}

        
        
        
        
#合并坏样本按顺序===================
        
        library(xlsx)
        setwd("D:/data/中行个人征信/中行个人征信共享")
        goodbad <- read.csv("goodbad.csv")
        goddbad <- goodbad[as.numeric(goodbad[,"累计逾期次数"]) > 0 ,]
        goodbad <- goodbad[order(-goodbad[,"累计逾期次数"]), ]
        goodbad <- goodbad[!duplicated(goodbad[,1]), ]
        
        samples <- read.xlsx2("数据样本0727-黄强.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
        
        inSams <- intersect(samples[,1],goodbad[,1])
        inSams <- goodbad[goodbad[,1] %in% inSams,1]
        subs <- 1:nrow(samples)
        sub1 <- match(inSams,samples[,1])
        sub2 <- setdiff(subs,sub1)
        samples <- samples[c(sub1,sub2), ]
        write.table(samples,file="数据样本0727-黄强3.txt",quote=FALSE,sep="\t",row.names = FALSE)

        
        
        
        
### 民生剩余样本==========================        
        minsheng <- read.csv("民生坏样本.csv")
        
        samples <- read.xlsx2("数据样本0727-黄强4.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
        samples <- samples[ ,1:60]
        goodbad <- read.csv("goodbad.csv")
        
        dianxinhaoduan <- c("189","181","180","177","153","133","1700")
        
        shenfenNUM14 <- sapply(1:nrow(minsheng), function(i) substr(minsheng[i,3],1,14))
        goodbadshenfenNUM14 <- sapply(1:nrow(goodbad), function(i) substr(goodbad[i,2],1,14))
        samshenfenNUM14 <- sapply(1:nrow(samples), function(i) substr(samples[i,2],1,14))
        
        minsheng <- minsheng[!(shenfenNUM14 %in% c(samshenfenNUM14,goodbadshenfenNUM14)), ]
        minsheng <- minsheng[!duplicated(minsheng[,3]), ]
        
        write.table(minsheng,file="民生样本592.txt",row.names = FALSE,quote=FALSE,sep="\t")
        
        write.table(minsheng[,c(1,3,68)],file="民生样本592_3col.txt",row.names = FALSE,quote=FALSE,sep="\t")
        
        
        
        
        
        
### 遍历三方面权重比例====================================
        
        
        load("bb")
        aa = bb
        mode(aa) <- "numeric"
        subs <- c(sample(142,100), sample(504-142,300)+142)
        aa <- aa[,subs]
        save(aa,file="bb_onetrain")
        
        
        rm(list=ls())
        gc()
        source('D:/code/CreditCardTest/Credit_v0_1.R')
        load("bb_onetrain")
        n.sam=400
        n.bad=100
        
        KSM <- array(0,dim=c(21,21,21))
        for(w1 in 1:21){
                for(w3 in 1:21){
                        for(w2 in 1:21){
                                oneV <- sapply(1:ncol(aa), function(i) weighted.mean(c(aa[1,i],aa[2,i],aa[3,i]),w=c(w1,w2,w3),na.rm=TRUE) )
                                #if(w2>=w1 & w2 >=w3) 
                                KSM[w1,w2,w3] <- KS_curves(oneV[(n.bad+1):n.sam],oneV[1:n.bad])
                        }
                }
                print(w1)
        }
        
        which(KSM==max(KSM),arr.ind = TRUE)
        max(KSM)
        aafit <- sapply(1:ncol(aa), function(i) weighted.mean(c(aa[1,i],aa[2,i],aa[3,i]),w=c(11,1,1),na.rm=TRUE) )
        KS_curves(aafit[(n.bad+1):n.sam],aafit[1:n.bad],plot=TRUE)
        
        
        #####===================================================
        rm(list=ls())
        gc()
        source('D:/code/CreditCardTest/Credit_v0_1.R')
        load("bb_onetrain")
        n.sam=457
        n.bad=130
        
        KSM <- array(0,dim=c(21,21))
 
                for(w3 in 1:21){
                        for(w2 in 1:21){
                                oneV <- sapply(1:ncol(aa), function(i) weighted.mean(c(aa[2,i],aa[3,i]),w=c(w2,w3),na.rm=TRUE) )
                                KSM[w2,w3] <- KS_curves(oneV[(n.bad+1):n.sam],oneV[1:n.bad])
                        }
                        ptint(w3)
                }
             
        which(KSM==max(KSM),arr.ind = TRUE)
        max(KSM)
        
        ####========================================================
        rm(list=ls())
        gc()
        
        source('D:/code/CreditCardTest/Credit_v0_1.R')
        load("bb")
        aa = bb
        mode(aa) <- "numeric"
        
        n.sam=570
        n.bad=23
        
        KSM <- array(0,dim=c(21,21,21))
        for(w1 in 1:21){
                for(w3 in 1:21){
                        for(w2 in 1:21){
                                oneV <- sapply(1:ncol(aa), function(i) weighted.mean(c(aa[1,i],aa[2,i],aa[3,i]),w=c(w1,w2,w3),na.rm=TRUE) )
                                if(w2>=w1 & w2 >=w3) KSM[w1,w2,w3] <- KS_curves(oneV[(n.bad+1):n.sam],oneV[1:n.bad])
                        }
                }
                print(w1)
        }
        
        which(KSM==max(KSM),arr.ind = TRUE)
        max(KSM)
        
        
### 所有衍生指标和类标签的 相关性大小========================================
n.bad <- 100
n.sam <- 504
labs <- c(rep(1,n.bad),rep(0,n.sam-n.bad))
        
yanshengS <- read.csv("衍生打分矩阵_2.csv")
rownames(yanshengS) <- paste(yanshengS[,1],1:nrow(yanshengS),sep="_")
yanshengS <- as.matrix(yanshengS[-(1:3),-1])
mode(yanshengS) <- "numeric"

corV <- 1:nrow(yanshengS)
for(i in 1:nrow(yanshengS)) corV[i] <- cor(x=yanshengS[i,],y=labs, use="pairwise.complete.obs") 


### 坏样本的新定义方法 =================================================================
a<-read.csv('hk.csv',header=T)
a[,24]<-as.numeric(a[,24])
a[,25]<-as.numeric(a[,25])
a[,27]<-as.numeric(a[,27])
bad<-0
bad[which(a[,24]>0|a[,25]>0)]<-1
bad[which(a[,24]==0 & a[,25]<=3 & a[,27]<=30)]<-0
b<-cbind(a,bad)
b<-b[order(b[,3],-bad),c(1,3,24,25,27,88)]
write.table(b,file="goodbad4.txt",quote=FALSE,col.names = FALSE, sep="\t")


### 写到数据库表里
tmp <- b
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbRemoveTable(con,"zhonghang_samplelabels")
dbWriteTable(con, "zhonghang_samplelabels", as.data.frame(tmp))
dbDisconnect(con)


### 所有样本写到数据库===============================================================
samples <- read.xlsx2("数据样本0727-黄强4.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
samples <- samples[ ,1:60]
sam0729Yi <- read.xlsx2("数据样本0729 - 移动全.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
sam0729Yi[sam0729Yi=="ERROR"] <- NA
sam0729Lian <- read.xlsx2("数据样本0729 - 联通全.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
sam0729Lian[sam0729Lian=="ERROR"] <- NA
samples <- rbind(samples,sam0729Yi,sam0729Lian)

### 写到数据库表里
tmp <- samples
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbRemoveTable(con,"zhonghang_samples")
dbWriteTable(con, "zhonghang_samples", as.data.frame(tmp))
dbDisconnect(con)

### 15 indexes  ============================================================================
n.bad=100
n.sam=504
load("outputIndex504")
mode(aa) <- "numeric"
valM <- aa[c(4:8,10:14,16:20), ]
valM <- t(valM)
valM <- cbind(valM,c(rep(1,n.bad),rep(0,n.sam-n.bad)))
colnames(valM)[1:15] <- paste(colnames(valM)[1:15],1:15,sep="_")
colnames(valM)[16] <- "标签"

tmp <- valM
library(RMySQL)
library(DBI)
con <- dbConnect(dbDriver("MySQL"),dbname="mathes_version3",user='etlmathes',password="yAUJ4c",host="172.16.2.244")
dbRemoveTable(con,"zhonghang_index15")
dbWriteTable(con, "zhonghang_index15", as.data.frame(tmp))
dbDisconnect(con)

### test ctree method =============================

rm(list=ls())
gc()

source('D:/code/CreditCardTest/Credit_v0_1.R')
options(stringsAsFactors = TRUE)
library(rpart)
library(C50)
library(adabag)
library(rpart.plot)

n.bad <- 100
n.sam <- 504

a1 <- as.matrix(read.csv("OldOutput/衍生指标矩阵_2.csv"))
#a1 <- as.matrix(read.csv("OldOutput/衍生打分矩阵_2.csv"))
a1 <- a1[-(1:3), -1]
mode(a1) <- "numeric"

#lab <- c(rep(0,n.bad),rep(1,n.sam-n.bad))
#a1 <- cbind(lab, t(a1))
#colnames(a1) <- c("lab", paste("X",1:(ncol(a1)-1),sep=""))

#### boosting method
lab <- c(rep("0",n.bad),rep("1",n.sam-n.bad))
a1 <- data.frame(t(a1),lab)
colnames(a1) <- c(paste("X",1:(ncol(a1)-1),sep=""),"Class")
rownames(a1) <- 1:nrow(a1)

adafit <- boosting(Class ~.,data=a1[c(1:75,101:300),], coeflearn="Zhu")
pred <- predict(adafit,a1[c(76:100,301:504),-ncol(a1)])$prob[,2]
KS_curves(pred[-(1:25)],pred[1:25],main="",plot=TRUE)
KS_value(pred[-(1:25)],pred[1:25],plot=FALSE)
        
        
#KS_curves(pred[(n.bad+1):n.sam],pred[1:n.bad],main="",plot=TRUE)

#### rpart method
# mfit <- rpart(lab~.,data=data.frame(a1))
# #rpart.plot(mfit)
# #summary(mfit)
# pred <- predict(mfit,data.frame(a1[-1,]))
# KS_curves(pred[(n.bad+1):n.sam],pred[1:n.bad],main="",plot=TRUE)

#### C50 method
# a1[is.na(a1)] <- "missing"
# a1 <- a1[1:200, ]
# C50fit <- C5.0(x = a1[,-1], y = as.factor(a1[,1]),control = C5.0Control(winnow = TRUE,subset=TRUE),trails=1)
# pred <- predict(C50fit, a1[,-1], type="prob")
# KS_curves(pred[(n.bad+1):200,2],pred[1:n.bad,2],main="",plot=TRUE)
#all(rownames(a1)==rownames(pred))
#a1 <- matrix(c(1,0.1,0.2,0.3,1,0.11,0.15,0.25,0,1,2,3,0,0.5,0.6,0.7,0,0.66,0.67,0.8),5,4,byrow=T)


### 所有衍生字段表=====================================================
jinrongziduan <- read.csv("金融画像衍生字段表_LY_0723_Chang.csv")
jinrongziduan[,2] <- paste(jinrongziduan[,1],jinrongziduan[,2],sep="_")
uniziduan <- jinrongziduan[!duplicated(jinrongziduan[,2]),2]
uniziduan <- uniziduan[uniziduan!=""]

## 金融画像近期字段得分映射
jinrongziduan1 <- read.csv("金融画像衍生字段表_LY_0723_Duan.csv")
jinrongziduan1[,2] <- paste(jinrongziduan1[,1],jinrongziduan1[,2],sep="_")
uniziduan1 <- jinrongziduan1[!duplicated(jinrongziduan1[,2]),2]
uniziduan1 <- uniziduan1[uniziduan1!=""]

#航空字段的得分 
hangkongziduan <- read.csv("航空数据衍生指标得分划分.csv",strip.white = TRUE)
ziduan <- hangkongziduan[!duplicated(hangkongziduan[,1]),1]
ziduan <- ziduan[ziduan!=""]

ziduans <- c(uniziduan,uniziduan1,ziduan)
write.csv(ziduans,file="所有衍生字段表.csv",quote=FALSE,row.names = FALSE)

### 纠正身份证号码错误===========================================
#D:\data\中行个人征信\中行个人征信共享\好样本0729\有盾数据样本_移动_0729.xlsx
#D:\data\中行个人征信\中行个人征信共享\好样本0727\新样本有盾数据_银行卡信息0727.xlsx
rm(list=ls())
gc()
library(xlsx)
#source('D:/code/CreditCardTest/misc.R')

readSample <- function(){
        ### 读取所有可以的样本
        samples <- read.xlsx2("数据样本0727-黄强4.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
        samples <- samples[ ,1:60]
        sam0729Yi <- read.xlsx2("0729样本文件/数据样本0729 - 移动全.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
        sam0729Yi[sam0729Yi=="ERROR"] <- NA
        sam0729Lian <- read.xlsx2("好样本0729/数据样本0812 - 联通全.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
        sam0729Lian[sam0729Lian=="ERROR"] <- NA
        samples <- rbind(samples,sam0729Yi,sam0729Lian)        
        
        samples
}
samples <- readSample()

allsamples <- read.xlsx2("民生坏样本.xls",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
aone <- paste(allsamples[,1],allsamples[,3],sep="_")
allsamples <- allsamples[!duplicated(aone), ]

for(i in 1:nrow(samples)){
        tmp <- allsamples[match(samples[i,1],allsamples[,1]),3]
        if(is.na(tmp)) print(i)
        samples[i,2] <- tmp
}

#which(duplicated(samples[,1]))

##王霞
samples[340,2] <- "620102720514502"
samples[341,2] <- "511202197903010821"
## 赵鹏飞
samples[460,2] <- "371202197809130310"
samples[461,2] <- "34212619790714053X"

### 张华
samples[489,2] <- "110106680426272"

### 张伟
samples[30,2] <- "420106197601260822"

write.table(samples,file="zhonghang_samples559.txt",row.names = FALSE,quote=FALSE,sep="\t")

library(DBI)
library(RMySQL)
conn <- dbConnect(MySQL(), 
                  user="etlmathes",
                  password="yAUJ4c",
                  host="172.16.2.244",
                  dbname="mathes_version3")
dbSendQuery(conn,'SET NAMES GBK')
dbRemoveTable(conn,"zhonghang_samples559")
dbWriteTable(conn, "zhonghang_samples559",  as.data.frame(samples), row.names=FALSE)
dbDisconnect(conn)

### 输出所有人被一票否决导致打分很低的人名=============
rm(list=ls())
gc()
setwd("D:/data/中行个人征信/中行个人征信共享")
source('D:/code/CreditCardTest/misc.R')
source('D:/code/CreditCardTest/Credit_v0_1.R')

TESTONE <- function(fields0){
        
        ## 首先判断是否具有一票否决权
        yipiaoFlag <- 0
        personOne <- fields0[1:3]
        shenfenzhengNUM <- personOne[2]
        oneage <- idcard_age(shenfenzhengNUM)
        siFainfo <- read.csv("个人司法查询.csv")
        siFaNUM <- siFainfo[,2]
        for(i in 1:nrow(siFainfo)) siFaNUM[i] <- substr(siFainfo[i,2],1,14)
        XinYongCut <- 0
        fields0 <- fields0[-(1:3)]
        fields0[grepl("N/A",fields0)] <- NA
        
        ## only for test
        fields1=rep(0,104)
        scores1=rep(0,104)
        ntmp <- 169
        
        if(oneage <= 18 | oneage>=60){
                result <- c(personOne,rep(0,ntmp),"年龄")
                yipiaoFlag <- 1
        }else if( any(siFaNUM==substr(shenfenzhengNUM,1,14)) ){
                tmpsub <- which(siFaNUM==substr(shenfenzhengNUM,1,14))
                if(siFainfo[tmpsub,3]>0){
                        ## 司法记录
                        result <- c(personOne,rep(0,ntmp),"司法记录")
                        yipiaoFlag <- 1
                }
        }
        
        if(yipiaoFlag==1){TRUE;}else{FALSE;}
}


samflag=2

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

tmp0 <- 1:nrow(samples)
for(i in 1:nrow(samples)){
        fields0 <- samples[i,]
        tmp0[i]  <- TESTONE(fields0)
}

yipiaoS <- samples[which(tmp0==TRUE),1:3]
write.table(yipiaoS,file="YipiaoSamples.txt",quote=FALSE,row.names = FALSE,sep = "\t")


