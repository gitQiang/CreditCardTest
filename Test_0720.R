
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



### 处理字段的区间到得分的映射表=======================================
setwd("D:/data/中行个人征信/中行个人征信共享")
ziduan <- read.csv("金融画像衍生字段表——LY.csv")
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
newziduan <- as.matrix(newziduan)

write.csv(newziduan,file="金融画像衍生字段表_LY_Fill.csv",quote=FALSE,row.names = FALSE)




