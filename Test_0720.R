
## ����ԭʼ�ֶ�=====================================
setwd("D:/data/���и�������/���и������Ź���")

tmp <- c("�ƶ���3����","�ƶ���12����")
bb <- unlist(read.csv("�ֶ�_��ǿ��ʱ�ƶ�.csv",header = FALSE, strip.white = TRUE))
bb <- unique(bb)
cc <- c()

for(i in 1:length(bb)){
     one4 <- paste(tmp,bb[i],sep="")
     cc <- c(cc,one4)
}
write.csv(cc,file="Dictionary_HQ_move.csv",quote=FALSE,row.names = FALSE)


tmp <- c("��ͨ��3����","��ͨ��12����")
bb <- unlist(read.csv("�ֶ�_��ǿ��ʱ��ͨ.csv",header = FALSE, strip.white = TRUE))
cc <- c()

for(i in 1:length(bb)){
        one4 <- paste(tmp,bb[i],sep="")
        cc <- c(cc,one4)
}
write.csv(cc,file="Dictionary_HQ_unicom.csv",quote=FALSE,row.names = FALSE)

## �ֶ��޸�һЩ���Ե��ֶ�  ��������

a1 <- unlist(read.csv("Dictionary_HQ_move.csv",header=FALSE))
a2 <- unlist(read.csv("Dictionary_HQ_unicom.csv",header=FALSE))
a3 <- union(a1,a2)
write.csv(a3,file="Dictionary_HQ_raw_v0.csv",quote=FALSE,row.names = FALSE)


aa <- unlist(read.csv("Dictionary_HQ_raw_v0.csv"))
aa <- gsub("�ƶ�","",aa)
aa <- gsub("��ͨ","",aa)
aa <- sort(unique(aa))
write.csv(aa,file="Dictionary_HQ_raw_v1.csv",quote=FALSE,row.names = FALSE)


### �����ֶε����䵽�÷ֵ�ӳ���=======================================
setwd("D:/data/���и�������/���и������Ź���")
ziduan <- read.csv("���ڻ��������ֶα�����LY.csv")
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

write.csv(newziduan,file="���ڻ��������ֶα�_LY_Fill.csv",quote=FALSE,row.names = FALSE)



