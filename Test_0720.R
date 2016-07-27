
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