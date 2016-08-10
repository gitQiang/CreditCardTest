#' Single person credit scores based on 60 original indexes.
#' 
#' Copyright reserved by Qiang Huang, 2016/08/09.
#' 
#' More details please contact: Qiang Huang. 
#' Qiang Huang, Email: huangqiang@3golden.com.cn
#'====================================================


library(data.tree)

##=======================================================================================================================

Creditf <- function(fields0){
        ## ����ԭʼ�ֶΣ�fields0; 
        ## �����ָ�ʽ�� ���� ����֤�� �ֻ��� ���ڻ����ֽ���6�����֣�������ֺ�һ���ۺϴ�֣� ���ڻ�����Զ��6�����֣�������ֺ�һ���ۺϴ�֣� ���л�����6������5���ֺ�һ���ۺϴ�֣� ����Զ�ںͷ��д���ۺ�6���� ��������ֺ�һ���ۺϴ�֣�
        
        
        ### ����ȷ����ģ���ļ�,load����
        load("")
        
        ## step 0: ���ַ���������ԭʼ�ֶΣ�
        
        ## �����ж��Ƿ����һƱ���Ȩ
        yipiaoFlag <- 0
        personOne <- fields0[1:3]
        shenfenzhengNUM <- personOne[2]
        oneage <- idcard_age(shenfenzhengNUM)
        fields0 <- fields0[-(1:3)]
        fields0[grepl("N/A",fields0)] <- NA
        
        if(oneage <= 18 | oneage>=60){
                result <- c(personOne,rep(0,ntmp),"����")
                yipiaoFlag <- 1
        }
        
        if(yipiaoFlag == 0){
                for(i in 1:length(fields0)){
                        if(grepl("]",fields0[i]) | grepl("\\[",fields0[i]) | grepl("\\(",fields0[i]) | grepl("\\)",fields0[i])){
                                
                                tmp <- unlist(strsplit(as.character(fields0[i]),",") )[1] ## lower bound
                                tmp <- gsub("\\[","",tmp)
                                tmp <- gsub("]","",tmp)
                                tmp <- gsub("\\(","",tmp)
                                tmp <- gsub("\\)","",tmp)
                                
                                tmp1 <- unlist(strsplit(as.character(fields0[i]),",") )[2] ## upper bound
                                tmp1 <- gsub("\\[","",tmp1)
                                tmp1 <- gsub("]","",tmp1)
                                tmp1 <- gsub("\\(","",tmp1)
                                tmp1 <- gsub("\\)","",tmp1)
                                
                                tmpaa <- c(as.numeric(tmp),as.numeric(tmp1))
                                
                                fields0[i] <- min(tmpaa[!is.na(tmpaa)])
                                if(fields0[i]==0) fields0[i]=1
                        }
                }
                
                ## step 1: ��ԭʼ�ֶε������ֶΣ� ������transform_fileds�� �����ֶα����ļ���
                trans <-  transform_fileds(fields0,shenfenzhengNUM)
                fields1 <- trans$fields1
                nleaf <- trans$nleaf
                nspe <- trans$nspe
                
                ## step 2: ��ȡÿ�������ֶεľ����֣� ������fields_scores�� �����ֶξ����ֱ����ļ���
                scores1 <- fields_scores(fields1,nleaf)
                scores1 <- as.numeric(scores1)
                scores1[nspe] <- fields1[nspe]
                fields1[c(9,10,37,73,102)-4, ] <- scores1[c(9,10,37,73,102)-4, ] !!!!!
        
                ## step 3: ������
                scores <- credit_scores(fits1,fields1,nleaf)
                
                ## step 4: Output
                result <- unlist(c(personOne,scores))
               
        }
        
        result
}

transform_fileds <- function(fields0, shenfenzhengNUM){
        
        fields0I <- fields0
        fields0 <- as.numeric(fields0[1:39])
        fieldsHangkong <- fields0I[40:length(fields0I)]
        
        nleaf <- 1:3
        fields1 <- 1:104
        ## ���ڻ���Զ��ԭʼ�ֶε������ֶ�ӳ��
        n <- 0
        
        ## xingfeipianhao
        fields1[n+1] <- fields0[n+9]/fields0[n+8]
        fields1[n+2] <- fields0[n+2]
        fields1[n+3] <- fields0[n+10]
        fields1[n+4] <- NA
        fields1[n+5] <- fields0[n+35]
        fields1[n+6] <- fields0[n+17]
        fields1[n+7] <- fields0[n+16]
        fields1[n+8] <- fields0[n+2]/fields0[n+35]
        fields1[n+9] <- fields0[n+10]/fields0[n+17]
        fields1[n+10] <- NA
        fields1[n+11] <- fields0[n+4]/fields0[n+10]
        fields1[n+12] <- fields0[n+15]/fields0[n+17]
        fields1[n+13] <- (fields0[n+5] + fields0[n+16])/(fields0[n+2] + fields0[n+35]) ### add 0805 Shiyonglv
        fields1[n+14] <- fields0[n+3]
        fields1[n+15] <- fields0[n+1]
        fields1[n+16] <- fields0[n+36]
        
        #lvyuenengli
        fields1[n+17] <- fields0[n+8]/12
        fields1[n+18] <- fields0[n+7]/12
        fields1[n+19] <- fields0[n+9]/12
        fields1[n+20] <- (fields0[n+10]+fields0[n+17])/12
        fields1[n+21] <- (fields0[n+15]+fields0[n+4])/12
        fields1[n+22] <- fields0[n+17]/fields0[n+16]
        fields1[n+23] <- fields0[n+16]/12
        fields1[n+24] <- fields0[n+6]/12
        fields1[n+25] <- fields0[n+5]/12
        fields1[n+26] <- fields0[n+8]/(fields0[n+17]+fields0[n+10])
        fields1[n+27] <- (fields0[n+10]+fields0[n+17])/(fields0[n+6])
        fields1[n+28] <- fields0[n+37]
        fields1[n+29] <- fields0[n+8]-fields0[n+6]
        fields1[n+30] <- fields0[n+9]
        
        #shehuiguanxi
        fields1[n+31] <- fields0[n+38]
        fields1[n+32] <- fields0[n+39]
        
        #shenfentezhi
        fields1[n+33] <- idcard_age(shenfenzhengNUM)
        fields1[n+34] <- idcard_sex(shenfenzhengNUM)
        fields1[n+35] <- city(idcard(shenfenzhengNUM))
        
        #xinyonglishi
        fields1[n+36] <- fields0[n+35]
        fields1[n+37] <- fields0[n+34]
        fields1[n+38] <- fields0[n+17]/12
        fields1[n+39] <- fields0[n+16]/12
        fields1[n+40] <- fields0[n+15]/fields0[n+17]
        fields1[n+41] <- fields0[n+17]/fields0[n+16]
        fields1[n+42] <- fields0[n+39]
        fields1[n+43] <- fields0[n+12]/12
        fields1[n+44] <- fields0[n+11]/12
        fields1[n+45] <- fields0[n+12]/fields0[n+18]
        fields1[n+46] <- fields0[n+18]-fields0[n+12]
        
        
        ## ���ڻ������ԭʼ�ֶε������ֶ�ӳ��
        n <- 0
        n1 <- 46
        nleaf[1] <- n1
        ## xingweipianhao
        fields1[n1+1] <- fields0[n+24]/fields0[n+23]
        fields1[n1+2] <- fields0[n+24]/fields0[n+9]
        fields1[n1+3] <- fields0[n+25]/fields0[n+17]
        fields1[n1+4] <- NA
        fields1[n1+5] <- fields0[n+19]/fields0[n+25]
        fields1[n1+6] <- fields0[n+30]/fields0[n+17]
        fields1[n1+7] <- (fields0[n+20]+fields0[n+31])/(fields0[n+2]+fields0[n+35]) ### add 0805
        
        ## lvyuenengli 
        fields1[n1+8] <- fields0[n+23]
        fields1[n1+9] <- fields0[n+22]
        fields1[n1+10] <- (fields0[n+23]/3)/(fields0[n+8]/12)
        fields1[n1+11] <- (fields0[n+22]/3)/(fields0[n+7]/12)
        fields1[n1+12] <- fields0[n+32]+fields0[n+21]
        fields1[n1+13] <- fields0[n+31]+fields0[n+20]
        fields1[n1+14] <- ((fields0[n+32]+fields0[n+21])/3)/((fields0[n+17]+fields0[n+6])/12)
        fields1[n1+15] <- ((fields0[n+31]+fields0[n+20])/3)/((fields0[n+16]+fields0[n+5])/12)
        fields1[n1+16] <- (fields0[n+23]/(fields0[n+32]+fields0[n+21]))/(fields0[n+8]/(fields0[n+6]+fields0[n+17]))
        fields1[n1+17] <- fields0[n+23]/(fields0[n+32]+fields0[n+21])
        fields1[n1+18] <- ((fields0[n+32]+fields0[n+25])/(fields0[n+32]+fields0[n+21]))/((fields0[n+17]+fields0[n+10])/(fields0[n+17]+fields0[n+6]))
        fields1[n1+19] <- (fields0[n+32]+fields0[n+25])/(fields0[n+32]+fields0[n+21])
        fields1[n1+20] <- fields0[n+37]
        fields1[n1+21] <- fields0[n+38] # zaixianshichang
        fields1[n1+22] <- fields0[n+39] # shoujixiaofei 
        
        ## shenfentezhi
        fields1[n1+23] <- idcard_age(shenfenzhengNUM) # Nianling
        fields1[n1+24] <- idcard_sex(shenfenzhengNUM) # Xingbie
        fields1[n1+25] <- city(idcard(shenfenzhengNUM)) # juzhudi
        
        ##
        fields1[n1+26] <- fields0[n+32]
        fields1[n1+27] <- fields0[n+31]
        fields1[n1+28] <- (fields0[n+32]/3)/(fields0[n+17]/12)
        fields1[n1+29] <- (fields0[n+31]/3)/(fields0[n+16]/12)
        fields1[n1+30] <- fields0[n+27]/fields0[n+33]
        fields1[n1+31] <- (fields0[n+27]/fields0[n+33])/(fields0[n+12]/fields0[n+18])
        fields1[n1+32] <- fields0[n+33] - fields0[n+27]
        
        
        ## ����ԭʼ�ֶε������ֶ�ӳ��
        n <- 0
        n2 <- 78
        nleaf[2] <- n2
        nspe <- n2 + c(1,19,21)
        
        if(is.na(fieldsHangkong[n+1]) | fieldsHangkong[n+1]==""){
                fields1[n2+(1:26)] <- NA
        }else{
                fields1[n2+19] <- flight(fieldsHangkong[n+9])
                fields1[n2+20] <- idcard_age(shenfenzhengNUM)
                fields1[n2+21] <- city(idcard(shenfenzhengNUM))
                fields1[n2+23] <- lastflight(fieldsHangkong[n+18])
                fields1[n2+c(1:5,24:26)] <- Hangkong_y1_5(fieldsHangkong[n+7], fieldsHangkong[n+8],shenfenzhengNUM) 
                #nspe <- n2 + c(1,19,21)
                
                fields1[n2+6] <- as.numeric(fieldsHangkong[n+10])
                fields1[n2+7] <- as.numeric(fieldsHangkong[n+11])
                fields1[n2+8] <- as.numeric(fieldsHangkong[n+10])/as.numeric(fieldsHangkong[n+11])
                fields1[n2+9] <- as.numeric(fieldsHangkong[n+1])
                fields1[n2+10] <- as.numeric(fieldsHangkong[n+15])
                fields1[n2+11] <- as.numeric(fieldsHangkong[n+15])/as.numeric(fieldsHangkong[n+1])
                fields1[n2+12] <- as.numeric(fieldsHangkong[n+13])*as.numeric(fieldsHangkong[n+1])
                fields1[n2+13] <- as.numeric(fieldsHangkong[n+13])
                fields1[n2+14] <- as.numeric(fields1[n2+12])/as.numeric(fields1[n2+10])
                fields1[n2+15] <- as.numeric(fieldsHangkong[n+4])
                fields1[n2+16] <- as.numeric(fieldsHangkong[n+4]) + as.numeric(fieldsHangkong[n+5])
                fields1[n2+17] <- as.numeric(fieldsHangkong[n+4])/as.numeric(fieldsHangkong[n+1])
                fields1[n2+18] <- as.numeric(fields1[n2+16])/as.numeric(fieldsHangkong[n+1])
                fields1[n2+22] <- as.numeric(fieldsHangkong[n+2])/as.numeric(fieldsHangkong[n+1])
                
                tmp <- fields1[(n2+1):(n2+26)]
                tmp[tmp==Inf] <- 9999999999
                fields1[(n2+1):(n2+26)] <- tmp
        }
        
        
        nleaf[3] <- length(fields1)
        
        ## add special index for Jinrong Chang and Duan
        nspe <- c(13,35, ## shiyonglv; juzhudi; Changqi
                  53,71, ## shiyonglv; juzhudi; Duanqi
                  nspe)
        
        list(fields1=fields1,nleaf=nleaf,nspe=nspe)
}

fields_scores <- function(fields1,nleaf){
        
        n1 <- 1:nleaf[1]
        n2 <- (nleaf[1]+1):nleaf[2]
        n3 <- (nleaf[2]+1):nleaf[3]
        tmp <- getBreaks()
        
        #options(stringsAsFactors=FALSE)
        ## ���ڻ����Զ�ڵ÷�
        breaksL <- tmp$breas1
        labelsL <- tmp$labels1
        JinrongScore <- sapply(n1, function(i){
                getScores(as.numeric(fields1[i]), breaksL[[i]])
                #cut(as.numeric(fields1[i]), breaksL[[i]],labelsL[[i]])
        })
        JinrongScore <- as.vector(JinrongScore)
        
        ## ���ڻ���Ľ��ڵ÷�
        breaksL <- tmp$breas2
        labelsL <- tmp$labels2
        JinrongScore1 <- sapply(n2, function(i){
                getScores(as.numeric(fields1[i]), breaksL[[i-max(n1)]])
        })
        JinrongScore1 <- as.vector(JinrongScore1)
        
        #�����ֶεĵ÷�
        breaksL <- tmp$breas
        labelsL <- tmp$labels
        
        HangkongScore <- sapply(n3, function(i){
                getScores(as.numeric(fields1[i]), breaksL[[i-max(n2)]],flag=2)
        })
        HangkongScore <- as.vector(HangkongScore)
        
        
        scores1 <- c(JinrongScore, JinrongScore1, HangkongScore)
        
        scores1
}

getBreaks <- function(){
        
        ## ���ڻ���Զ���ֶε÷�ӳ��
        jinrongziduan <- read.csv("���ڻ��������ֶα�_LY_0723_Chang.csv")
        jinrongziduan[,2] <- paste(jinrongziduan[,1],jinrongziduan[,2],sep="_")
        jinrongziduan[is.na(jinrongziduan[,3]),3] <- -Inf
        jinrongziduan[is.na(jinrongziduan[,4]),4] <- Inf
        uniziduan <- setdiff(unique(jinrongziduan[,2]),"")
        breas1 <- list()
        labels1 <- list()
        for(i in 1:length(uniziduan)){
                tmpsubs <- which(jinrongziduan[,2]==uniziduan[i])
                tmp <- jinrongziduan[tmpsubs,3:5]
                breas1[[i]] <- tmp
                labels1[[i]] <- 0 #as.numeric(jinrongziduan[tmpsubs,5])
        }
        names(breas1) <- uniziduan
        
        ## ���ڻ�������ֶε÷�ӳ��
        jinrongziduan1 <- read.csv("���ڻ��������ֶα�_LY_0723_Duan.csv")
        jinrongziduan1[,2] <- paste(jinrongziduan1[,1],jinrongziduan1[,2],sep="_")
        jinrongziduan1[is.na(jinrongziduan1[,3]),3] <- -Inf
        jinrongziduan1[is.na(jinrongziduan1[,4]),4] <- Inf
        uniziduan1 <- setdiff(unique(jinrongziduan1[,2]),"")
        breas2 <- list()
        labels2 <- list()
        for(i in 1:length(uniziduan1)){
                tmpsubs <- which(jinrongziduan1[,2]==uniziduan1[i])
                tmp <- jinrongziduan1[tmpsubs,3:5]
                breas2[[i]] <- tmp
                labels2[[i]] <- 0 #as.numeric(jinrongziduan1[tmpsubs,5])
        }
        names(breas2) <- uniziduan1
        
        
        #�����ֶεĵ÷� 
        hangkongziduan <- read.csv("������������ָ��÷ֻ���.csv",strip.white = TRUE)
        ziduan <- setdiff(unique(hangkongziduan[,1]),"")
        for(i in 1:nrow(hangkongziduan)){
                if(hangkongziduan[i,1]==""){
                        hangkongziduan[i,1] <- hangkongziduan[i-1,1]
                }
        }
        hangkongziduan[,5] <- gsub("infty","Inf",hangkongziduan[,5])
        
        breas <- list()
        labels <- list()
        for(i in 1:length(ziduan)){
                tmpsubs <- which(hangkongziduan[,1]==ziduan[i])
                tmp <- hangkongziduan[tmpsubs,4:6]
                breas[[i]] <- tmp #sort(unique(as.numeric(tmp)))
                labels[[i]] <- 0 #as.numeric(hangkongziduan[tmpsubs,6])
        }
        names(breas) <- ziduan
        
        list(breas1=breas1,labels1=labels1, breas2=breas2,labels2=labels2, breas=breas,labels=labels)
        
}

getScores <- function(f,breaksL,flag=1){
        
        f1 <- NA
        if(any(is.na(breaksL))){
                f1 <- NA
        }else if(is.na(f)){
                f1 <- NA
        }else if(flag==1){
                for(i in 1:nrow(breaksL)){
                        if(f > as.numeric(breaksL[i,1]) & f <= as.numeric(breaksL[i,2])){
                                f1 <- breaksL[i,3]
                                break;
                        }
                }
        }else if(flag==2){
                for(i in 1:nrow(breaksL)){
                        if(f >= as.numeric(breaksL[i,1]) & f < as.numeric(breaksL[i,2])){
                                f1 <- breaksL[i,3]
                                break;
                        }
                }
        }
        
        f1
}

credit_scores <- function(fits1,fields1,nleaf){
        fit1 <- fits1$fit1
        fit2 <- fits1$fit2
        fit3 <- fits1$fit3
        fit4 <- fits1$fit4
        
        colnames(fields1) <- paste("X",1:length(fields1),sep="") #!!!!!
        result <- 1:4
        result[1] <- predict(fit1,data.frame(fields1)[ 1:nleaf[1] ])$prob[,2]
        result[2] <- predict(fit2,data.frame(fields1)[ (nleaf[1]+1):nleaf[2] ])$prob[,2]
        result[3] <- predict(fit3,data.frame(fields1)[ (nleaf[2]+1):nleaf[3] ])$prob[,2]
        result[4] <- predict(fit4,data.frame(fields1))$prob[,2]
        
        # preModel <- predict(adafit,newdata)$prob[,2] ## method=2 and method=9
        # preModel <- predict(fit,as.data.frame(newdata),type="prob")[,2] ## method=5

        result  
}

#### pieces of functions to deal with raw fields0 with text

flight <- function(s){
        f<-substring(s,1,1)
        if(f=="��" | f=="��" | f=="��"){
                return(1)
        }else if (f =='��'){
                return(0.5)
        }else{
                return(0)
        }
}

idcard_age <- function(s){
        if(nchar(s) > 15) birthday <- substr(s,7,10)# ����֤��������
        if( nchar(s) == 15 ) birthday <- as.numeric(substr(s,7,8)) + 1900 # ����֤��������
        age<-as.numeric(format(Sys.Date(),format='%Y'))-as.numeric(birthday)
        
        return(age)
}

idcard_sex <- function(s){
        ## sex: man 1; women: 2; 
        sex <- 0
        if( nchar(s)==15 )  sex <- ifelse(as.numeric(substr(s,15,15)) %% 2 == 0, 2, 1)
        if( nchar(s)==18 )  sex <- ifelse(as.numeric(substr(s,17,17)) %% 2 == 0, 2, 1)     
        if( sex == 0 ) print("��Ч������֤��")
        sex
}

city <- function(s){
        d <- read.csv('city_grade.csv', header = T)
        d[,1]<-as.character(d[,1])
        k<-0
        if(any(d$city==s)) k<-d$score[which(d$city==s)]
        
        return(k)
}

idcard <- function(s){
        d2<-read.csv('id_city.csv',header = T) # ǰ4λ-���ж�Ӧ��
        d2[,2] <- as.character(d2[,2])
        d2[,2] <- gsub("\t","",d2[,2])
        d2[,2] <- gsub(" ","",d2[,2])
        s4<-substr(s,1,4) # ����֤ǰ4λ
        return(as.character(d2[d2[,1]==s4,2]))
}

lastflight <- function(s){
        if( is.na(as.numeric(s)) ){
                NA
        }else{
                if(!( any(grepl("-",s)) | any(grepl("/",s)) ) ) s <- paste(substr(s,1,4),substr(s,5,6),substr(s,7,8),sep="-")
                s<-as.Date(s,'%Y%m%d')
                gap<-difftime(Sys.Date(),s,units='days')
                gap
        }
}

Hangkong_y1_5 <- function(a,b,a0){
        #����a=a7,b=a8,id=a0
        #���y1-y5,y26-y28
        if(is.na(a) | is.na(b) | a=="" | b==""){
                c(0,0,0,0,0,0,0,0)       
        }else{
                ct<-read.csv('id_city.csv',header=T)
                
                a<-split_city(a)
                b<-split_city(b)
                a<-rbind(a,b)
                a<-as.data.frame(a)
                a<-a[which(a$city!=''),]
                b<-aggregate(a$times,by=list(a$city),sum)
                names(b)<-c('city','times')
                y28<-crossprod(b$times/sum(b$times))
                
                a<-merge(b,ct,by='city')
                y26<-length(a$city)
                
                if(y26>0){
                        t<-rep(0,length(a$city))
                        for(i in 1:length(a$city))t[i]<-city(a[i,'city'])
                        a<-cbind(a,score=t)
                        y27<-length(a[which(a$score==1),'city'])
                        y2<-y27/y26
                        y3<-length(a[which(a$score==1|a$score==2),'city'])/y26
                }else{
                        y27 <- 0; y2 <- 0; y3 <- 0;    
                }
                
                a<-a[order(-a$times),]
                t<-rep(1,length(a$city))
                if(length(a$city)>1)
                        for(i in 2:length(a$city)){
                                t[i]<-t[i-1]
                                if(a$times[i]<a$times[i-1])t[i]<-t[i-1]+1
                        }
                a<-cbind(a,t)
                a<-a[which(a$t<=3),]
                if(length(a$city)>0) y1 <- mean(a[which(a$t==1),'score']) else y1<-NA
                
                idcity<-idcard(a0)
                y4<-length(a[which(a$t==1&a$city==idcity),'city'])
                y5<-length(a[which(a$t<=3&a$city==idcity),'city'])
                
                c(y1,y2,y3,y4,y5,y26,y27,y28)
        }
        
}

split_city <- function(a){
        u<-gsub('�δ�','��',gsub(' ','',a))
        t<-as.character(unlist(strsplit(u,'\\d+��,')))
        u1 <- gsub("\\D"," ",u)
        k <- as.numeric(unlist(strsplit(u1," ")))
        k <- k[!is.na(k)]
        
        if(length(k)==0){
                k<-NA
                t<-''
        }
        out<-as.data.frame(cbind(city=t,times=k))
        out$times<-as.numeric(out$times)
        return(out)
}
