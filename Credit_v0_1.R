library(data.tree)

## only for test
test <- function(){
        
        rm(list=ls())
        gc()
        library(xlsx)
        setwd("D:/data/���и�������/���и������Ź���")
        source('D:/code/CreditCardTest/Credit_v0_1.R')
        samples <- read.xlsx2("��������0727-��ǿ4.xlsx",1,as.data.frame = TRUE, header=TRUE, colClasses="character")
        samples <- samples[ ,1:60]
        n.sam <- nrow(samples)
        n.bad <- 24
        
        samsN <- unlist(samples[,1])
        outputN <- unlist(read.csv("��������ֶα�.csv",header = F)[,1])
        rawfieN <- unlist(read.csv("����ԭʼ�ֶα�.csv",header = F)[,1])
        newfieN <- unlist(read.csv("���������ֶα�.csv",header = F))
        
        treeNodes <- read.table("AllTreesNodes.txt")
        treeNodes <- rbind(cbind(c("����","����֤��","�ֻ���"),1),treeNodes,cbind(c("����ϵ","��������","������ʷ","��Ϊƫ��","��Լ����","�ۺ�","һƱ���"),1))
        
        aa <- matrix(-1,28,n.sam)
        aasubs <- c(107,110,113,72,83,71,49,52,55,2,25,1,156,150,135,153,124,123)
        bb <- matrix(-1,60,n.sam)
        cc <- matrix(-1,103,n.sam)
        dd <- matrix(-1,103,n.sam)
        ee <- matrix(-1,169,n.sam)
        
        colnames(aa) <- samsN
        colnames(bb) <- samsN
        colnames(cc) <- samsN
        rownames(aa) <- outputN
        rownames(bb) <- rawfieN
        rownames(cc) <- c("����","����֤��","�ֻ���",newfieN)
        rownames(dd) <- c("����","����֤��","�ֻ���",newfieN)
        
        tmp0 <- 1:nrow(samples)
        for(i in 1:nrow(samples)){
                print(i)
                fields0 <- samples[i,]
                tmp  <- Credit_v0_1(fields0)
                aa[,i] <- unlist(tmp$result[c(1:3,aasubs+3,163:169)])
                
                bb[,i] <- unlist(c(fields0[1:3], tmp$fields0))
                cc[,i] <- unlist(c(fields0[1:3], tmp$fields1))
                dd[,i] <- unlist(c(fields0[1:3], tmp$scores1))
                ee[,i] <- unlist(tmp$result)
                tmp0[i] <- tmp$result[168]
        }
        ee <- cbind(treeNodes,ee)
        write.csv(aa,file="�����������_1.csv",quote=FALSE)
        write.table(bb,file="ԭʼָ�����_1.xls",quote=FALSE,sep="\t")
        write.table(t(bb),file="ԭʼָ�����ת��_1.xls",quote=FALSE,sep="\t")
        write.csv(cc,file="����ָ�����_1.csv",quote=FALSE)
        write.csv(dd,file="������־���_1.csv",quote=FALSE)
        write.csv(ee,file="���д�־���_1.csv",quote=FALSE)
        
        plot(1:length(tmp0), as.numeric(tmp0),type="b")
        abline(v=n.bad+0.5,col=2,lwd=2)
        # 
        bb <- aa[c(9,15,21), ]
        save(bb,file="bb")
        
        mains <- outputN[-c(1:3)]#c("����","Զ��","����","�ۺ�")
        k <- 1
        for(n in 4:27){
                KS_curves(aa[n,(n.bad+1):n.sam],aa[n,1:n.bad],main=mains[k],plot=TRUE)
                k <- k+1
        }
        
        X <- as.matrix(t(bb))
        mode(X) <- "numeric"
        colnames(X) <- c("X1","X2","X3")
        Y <- c(rep(0,n.bad),rep(1,n.sam-n.bad))
        data <- list()
        data$X <- X
        data$Y <- Y
        fitlm <- lm(Y~.,data=data.frame(data))
        fitglm <- glm(Y~.,data=data.frame(data),family = binomial())
        
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
                legend("bottomright",legend=c("������","������"),col=1:2,lwd=2)
                text(xv,y=0.5,round(max(y2-y1),4))
                axis(1,at=seq(1,length(x0),length.out=10),labels=round(seq(1,0,length.out = 10),2))
        }
        
        ks <- max(y2-y1)
        ks
}


Credit_v0_1 <- function(fields0){
        ## ����ԭʼ�ֶΣ�fields0; ԭʼ�ֶ�˳����ļ���Dictionary_HQ_raw_v0.csv
        ## �����ָ�ʽ�� ���� ����֤�� �ֻ��� ���ڻ����ֽ���6�����֣�������ֺ�һ���ۺϴ�֣� ���ڻ�����Զ��6�����֣�������ֺ�һ���ۺϴ�֣� ���л�����6������5���ֺ�һ���ۺϴ�֣� ����Զ�ںͷ��д���ۺ�6���� ��������ֺ�һ���ۺϴ�֣�
        

        ## step 0: ���ַ���������ԭʼ�ֶΣ� ������ ��ԭʼ�ֶα����ļ���

        ## �����ж��Ƿ����һƱ���Ȩ
        yipiaoFlag <- 0
        personOne <- fields0[1:3]
        shenfenzhengNUM <- personOne[2]
        oneage <- idcard_age(shenfenzhengNUM)
        siFainfo <- read.csv("����˾����ѯ.csv")
        siFaNUM <- siFainfo[,2]
        for(i in 1:nrow(siFainfo)) siFaNUM[i] <- substr(siFainfo[i,2],1,14)
        XinYongCut <- 0
        fields0 <- fields0[-(1:3)]
        
        ## only for test
        fields1=rep(0,100)
        scores1=rep(0,100)
        
        if(oneage <= 18 | oneage>=60){
                result <- c(personOne,rep(0,165),"����")
                yipiaoFlag <- 1
        }else if( any(siFaNUM==substr(shenfenzhengNUM,1,14)) ){
                tmpsub <- which(siFaNUM==substr(shenfenzhengNUM,1,14))
                if(siFainfo[tmpsub,3]>0){
                        ## ˾����¼
                        result <- c(personOne,rep(0,165),"˾����¼")
                        yipiaoFlag <- 1
                }
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
                #print(all(is.na(scores1[nspe])))
                scores1 <- as.numeric(scores1)
                scores1[nspe] <- fields1[nspe]
                #scores1[is.na(scores1)] <- 0
                
                ## step 3: Ԥ�������ڻ���ͷ��л��������������trees_construct; ���ڻ�����״�ṹ���ļ���  ���л�����״�ṹ���ļ���
                trees1 <- trees_construct()
                
                ## step 4: ����ÿһ����εĴ�֣���������������֣� ������credit_scores;
                scores <- credit_scores(trees1,scores1,nleaf)
                
                ## step 5: Output
                result1 <- unlist(c(personOne,scores))
                
                #!!!!! 24 sub only for test
                if(is.na(as.numeric(result1[165])) | as.numeric(result1[165]) <= XinYongCut){
                        ## ������ʷ
                        result <- c(result1,"������ʷ") #����������
                        result[c(71,1,123,165)+3] <- 0
                }else{
                        result <- c(result1,0)         
                }
        }

        #result
        
        list(result=result,fields0=fields0, fields1=fields1,scores1=scores1)
}

transform_fileds <- function(fields0, shenfenzhengNUM){
        
        fields0I <- fields0
        fields0 <- as.numeric(fields0[1:39])
        fieldsHangkong <- fields0I[40:length(fields0I)]
        
        nleaf <- 1:3
        fields1 <- 1:100
        ## ���ڻ���Զ��ԭʼ�ֶε������ֶ�ӳ��
        n <- 0
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
        fields1[n+13] <- fields0[n+3]
        fields1[n+14] <- fields0[n+1]
        fields1[n+15] <- fields0[n+36]
        
        #lvyuenengli
        fields1[n+16] <- fields0[n+8]/12
        fields1[n+17] <- fields0[n+7]/12
        fields1[n+18] <- fields0[n+9]/12
        fields1[n+19] <- (fields0[n+10]+fields0[n+17])/12
        fields1[n+20] <- (fields0[n+15]+fields0[n+4])/12
        fields1[n+21] <- fields0[n+17]/fields0[n+16]
        fields1[n+22] <- fields0[n+16]/12
        fields1[n+23] <- fields0[n+6]/12
        fields1[n+24] <- fields0[n+5]/12
        fields1[n+25] <- fields0[n+8]/(fields0[n+17]+fields0[n+10])
        fields1[n+26] <- (fields0[n+10]+fields0[n+17])/(fields0[n+6])
        fields1[n+27] <- fields0[n+37]
        fields1[n+28] <- fields0[n+8]-fields0[n+6]
        fields1[n+29] <- fields0[n+9]
        
        #shehuiguanxi
        fields1[n+30] <- fields0[n+38]
        fields1[n+31] <- fields0[n+39]
        
        #shenfentezhi
        ##fields1[n+32] <- shenfentezhi(nianling)
        ##fields1[n+33] <- juzhudi
        
        fields1[n+32] <- idcard_age(shenfenzhengNUM)
        fields1[n+33] <- city(idcard(shenfenzhengNUM))
        
        #xinyonglishi
        fields1[n+34] <- fields0[n+35]
        fields1[n+35] <- fields0[n+34]
        fields1[n+36] <- fields0[n+17]/12
        fields1[n+37] <- fields0[n+16]/12
        fields1[n+38] <- fields0[n+15]/fields0[n+17]
        fields1[n+39] <- fields0[n+17]/fields0[n+16]
        fields1[n+40] <- fields0[n+39]
        fields1[n+41] <- fields0[n+12]/12
        fields1[n+42] <- fields0[n+11]/12
        fields1[n+43] <- fields0[n+12]/fields0[n+18]
        fields1[n+44] <- fields0[n+18]-fields0[n+12]
        
        
        ## ���ڻ������ԭʼ�ֶε������ֶ�ӳ��
        n1 <- 44
        nleaf[1] <- n1
        
        fields1[n1+1] <- fields0[n+24]/fields0[n+23]
        fields1[n1+2] <- fields0[n+24]/fields0[n+9]
        fields1[n1+3] <- fields0[n+25]/fields0[n+17]
        fields1[n1+4] <- NA
        fields1[n1+5] <- fields0[n+19]/fields0[n+25]
        fields1[n1+6] <- fields0[n+30]/fields0[n+17]
        fields1[n1+7] <- fields0[n+23]
        fields1[n1+8] <- fields0[n+22]
        fields1[n1+9] <- (fields0[n+23]/3)/(fields0[n+8]/12)
        fields1[n1+10] <- (fields0[n+22]/3)/(fields0[n+7]/12)
        fields1[n1+11] <- fields0[n+32]+fields0[n+21]
        fields1[n1+12] <- fields0[n+31]+fields0[n+20]
        fields1[n1+13] <- ((fields0[n+32]+fields0[n+21])/3)/((fields0[n+17]+fields0[n+6])/12)
        fields1[n1+14] <- ((fields0[n+31]+fields0[n+20])/3)/((fields0[n+16]+fields0[n+5])/12)
        fields1[n1+15] <- (fields0[n+23]/(fields0[n+32]+fields0[n+21]))/(fields0[n+8]/(fields0[n+6]+fields0[n+17]))
        fields1[n1+16] <- fields0[n+23]/(fields0[n+32]+fields0[n+21])
        fields1[n1+17] <- ((fields0[n+32]+fields0[n+25])/(fields0[n+32]+fields0[n+21]))/((fields0[n+17]+fields0[n+10])/(fields0[n+17]+fields0[n+6]))
        fields1[n1+18] <- (fields0[n+32]+fields0[n+25])/(fields0[n+32]+fields0[n+21])
        fields1[n1+19] <- fields0[n+37]
        fields1[n1+20] <- fields0[n+38] # zaixianshichang
        fields1[n1+21] <- fields0[n+39] # shoujixiaofei 
        fields1[n1+22] <- NA # gaoguanrenzhi
        fields1[n1+23] <- city(idcard(shenfenzhengNUM)) # juzhudi
        fields1[n1+24] <- fields0[n+32]
        fields1[n1+25] <- fields0[n+31]
        fields1[n1+26] <- (fields0[n+32]/3)/(fields0[n+17]/12)
        fields1[n1+27] <- (fields0[n+31]/3)/(fields0[n+16]/12)
        fields1[n1+28] <- fields0[n+27]/fields0[n+33]
        fields1[n1+29] <- (fields0[n+27]/fields0[n+33])/(fields0[n+12]/fields0[n+18])
        fields1[n1+30] <- fields0[n+33] - fields0[n+27]
        

        ## ����ԭʼ�ֶε������ֶ�ӳ��
        n <- 0
        n2 <- 74
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
        nspe <- c(33,67,nspe)
        
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

trees_construct <- function(){

        ## ���ڻ���Զ��
        tree1 <- Node$new("JinronghuaxiangYuan")
        Xingweipianhao_tree1 <- tree1$AddChild("Xingweipianhao")
        Touzi_tree1 <- Xingweipianhao_tree1$AddChild("Touzi")
        Yongkaxiguan_tree1 <- Xingweipianhao_tree1$AddChild("Yongkaxiguan")
        Jiejika_tree1 <- Yongkaxiguan_tree1$AddChild("Jiejika")
        Jiejikashuliang_tree1 <- Jiejika_tree1$AddChild("Jiejikashuliang")
        Jiejikaxiaofeijine12_tree1 <- Jiejika_tree1$AddChild("Jiejikaxiaofeijine12")
        Jiejikaxiaofeicishu12_tree1 <- Jiejika_tree1$AddChild("Jiejikaxiaofeicishu12")
        Xinyongka_tree1 <- Yongkaxiguan_tree1$AddChild("Xinyongka")
        Xinyongkashuliang_tree1 <- Xinyongka_tree1$AddChild("Xinyongkashuliang")
        Xinyongkaxiaofeijine12_tree1 <- Xinyongka_tree1$AddChild("Xinyongkaxiaofeijine12")
        Xinyongkaxiaofeicishu12_tree1 <- Xinyongka_tree1$AddChild("Xinyongkaxiaofeicishu12")
        Jiedaibi_tree1 <- Yongkaxiguan_tree1$AddChild("Jiedaibi")
        Jiedaibishuliangbi_tree1 <- Jiedaibi_tree1$AddChild("Jiedaibishuliangbi")
        Jiedaibixiaofeijinebi12_tree1 <- Jiedaibi_tree1$AddChild("Jiedaibixiaofeijine12")
        Jiedaibixiaofeicishu12_tree1 <- Jiedaibi_tree1$AddChild("Jiedaibixiaofeicishu12")
        Wanggouyushitidianbili_tree1 <- Yongkaxiguan_tree1$AddChild("Wanggouyushitidianbili")
        Jiejikaxianshangxiaofeijine12_tree1 <-  Wanggouyushitidianbili_tree1$AddChild("Jiejikaxianshangxiaofeijine12")
        Xinyongkaxianshangxiaofeijine12_tree1 <-  Wanggouyushitidianbili_tree1$AddChild("Xinyongkaxianshangxiaofeijine12")
        Yongkanianxian_tree1 <- Xingweipianhao_tree1$AddChild("Yongkanianxian")
        Jiejikayongkanianxian_tree1 <- Yongkanianxian_tree1$AddChild("Jiejikayongkanianxian")
        Jiejikazhangling_tree1 <- Jiejikayongkanianxian_tree1$AddChild("Jiejikazhangling")
        Jiejikakaling_tree1 <- Jiejikayongkanianxian_tree1$AddChild("Jiejikakaling")
        Xinyongkayongkazhangling_tree1 <- Yongkanianxian_tree1$AddChild("Xinyongkayongkazhangling")
        Lvyuenengli_tree1 <- tree1$AddChild("Lvyuenengli")
        Shouzhi_tree1 <-  Lvyuenengli_tree1$AddChild("Shouzhi")  
        Shou_tree1 <- Shouzhi_tree1$AddChild("Shou")
        Ruzhangjine12_tree1 <- Shou_tree1$AddChild("Ruzhangjine12")
        Ruzhangbishu12_tree1 <- Shou_tree1$AddChild("Ruzhangbishu12")
        Zhi_tree1 <- Shouzhi_tree1$AddChild("Zhi")
        Touzizhi_tree1 <- Zhi_tree1$AddChild("Touzizhi")
        Xiaofeizhi_tree1 <- Zhi_tree1$AddChild("Xiaofeizhi")
        Jinexiaofei_tree1 <- Xiaofeizhi_tree1$AddChild("Jinexiaofei")
        Xiaofeijine12_tree1 <- Jinexiaofei_tree1$AddChild("Xiaofeijine12")
        Xianshangxiaofei12_tree1 <- Jinexiaofei_tree1$AddChild("Xianshangxiaofei12")
        Dancixiaofeijine12_tree1 <- Jinexiaofei_tree1$AddChild("Dancixiaofeijine12")
        Pinlvxiaofei_tree1 <- Xiaofeizhi_tree1$AddChild("Pinlvxiaofei")
        Chuzhangzhi_tree1 <- Zhi_tree1$AddChild("Chuzhangzhi")
        Jinechuzhang_tree1 <- Chuzhangzhi_tree1$AddChild("Jinechuzhang")
        Pinlvchuzhang_tree1 <- Chuzhangzhi_tree1$AddChild("Pinlvchuzhang")
        Bilv_tree1 <- Shouzhi_tree1$AddChild("Bilv")
        Shouruzhichubi12_tree1 <- Bilv_tree1$AddChild("Shouruzhichubi12")
        XiaofeiZhichubi12_tree1 <- Bilv_tree1$AddChild("XiaofeiZhichubi12")
        Zichanfuzhai_tree1 <-  Lvyuenengli_tree1$AddChild("Zichanfuzhai")        
                Zichanjine_tree1 <- Zichanfuzhai_tree1$AddChild("Zichanjine")
                        Jiejikayue_tree1 <- Zichanjine_tree1$AddChild("Jiejikayue")
                        Zongruzhangjine12_tree1 <- Zichanjine_tree1$AddChild("Zongruzhangjine12")
        Gerentouzi12_tree1 <- Zichanfuzhai_tree1$AddChild("Gerentouzi12")
        Shehuiguanxi_tree1 <- tree1$AddChild("Shehuiguanxi")
        Zaixianshichang_tree1 <- Shehuiguanxi_tree1$AddChild("Zaixianshichang")
        Shoujixiaofei_tree1 <- Shehuiguanxi_tree1$AddChild("Shoujixiaofei")
        Shenfentezhi_tree1 <- tree1$AddChild("Shenfentezhi")
        Nianling_tree1 <-  Shenfentezhi_tree1$AddChild("Nianling")
        #Gaoguanrenzhi_tree1 <-  Shenfentezhi_tree1$AddChild("Gaoguanrenzhi")
        #GerentouziShen_tree1 <-  Shenfentezhi_tree1$AddChild("GerentouziShen")
        Juzhudi_tree1 <-  Shenfentezhi_tree1$AddChild("Juzhudi")
        Xinyonglishi_tree1 <- tree1$AddChild("Xinyonglishi")
        Huoyuedu_tree1 <- Xinyonglishi_tree1$AddChild("Huoyuedu")
        XinyongkaXin_tree1 <- Huoyuedu_tree1$AddChild("XinyongkaXin")
        Xinyongkashuliangxin_tree1 <- XinyongkaXin_tree1$AddChild("Xinyongkashuliangxin")
        Yinhangshuliangxin_tree1 <- XinyongkaXin_tree1$AddChild("Yinhangshuliangxin")
        Xiaofeixin_tree1 <- XinyongkaXin_tree1$AddChild("Xiaofeixin")
        Xinyongkaxiaofeijinexin_tree1 <- Xiaofeixin_tree1$AddChild("Xinyongkaxiaofeijinexin")
        Xinyongkaxiaofeibishuxin_tree1 <- Xiaofeixin_tree1$AddChild("Xinyongkaxiaofeibishuxin")
        Xinyongkaxianshangxiaofeibixin_tree1 <- Xiaofeixin_tree1$AddChild("Xinyongkaxianshangxiaofeibixin")
        Xinyongkadancixiaofeijinexin_tree1 <- Xiaofeixin_tree1$AddChild("Xinyongkadancixiaofeijinexin")
        Houfufeishixiang_tree1 <- Huoyuedu_tree1$AddChild("Houfufeishixiang")
        Shouxindu_tree1 <- Xinyonglishi_tree1$AddChild("Shouxindu")
        huankuanjine12 <- Shouxindu_tree1$AddChild("huankuanjine12")
        huankuanbishu12 <- Shouxindu_tree1$AddChild("huankuanbishu12")
        huanqingbili12 <- Shouxindu_tree1$AddChild("huanqingbili12")
        huanqingtuoqiane12 <- Shouxindu_tree1$AddChild("huanqingtuoqiane12")
                                               
        ## ���ڻ������
        ## ���ڻ��� short term
        tree2 <- Node$new("JinronghuaxiangJin")
        Xingweipianhao_tree2 <- tree2$AddChild("Xingweipianhao")
        Touzi_tree2 <- Xingweipianhao_tree2$AddChild("Touzi")
        Touzizhanbi_tree2 <- Touzi_tree2$AddChild("Touzizhanbi")
        Touziqushi_tree2 <- Touzi_tree2$AddChild("Touziqushi")
        Yongkaxiguan_tree2 <- Xingweipianhao_tree2$AddChild("Yongkaxiguan")
        Jiedaibi_tree2 <- Yongkaxiguan_tree2$AddChild("Jiedaibi")
        Jiejikaxinyongkaxiaofeijinebi_tree2 <- Jiedaibi_tree2$AddChild("Jiejikaxinyongkaxiaofeijinebi")
        Jiejikaxinyongkaxiaofeicishubi_tree2 <- Jiedaibi_tree2$AddChild("Jiejikaxinyongkaxiaofeicishubi")
        Wanggouyushitidianbili_tree2 <- Yongkaxiguan_tree2$AddChild("Wanggouyushitidianbili")
        Xianshangxiaofeibijiejika_tree2 <- Wanggouyushitidianbili_tree2$AddChild("Xianshangxiaofeibijiejika")
        Xianshangxiaofeibixinyongka_tree2 <- Wanggouyushitidianbili_tree2$AddChild("Xianshangxiaofeibixinyongka")
        Lvyuenengli_tree2 <- tree2$AddChild("Lvyuenengli")
        Shouzhi_tree2 <-  Lvyuenengli_tree2$AddChild("Shouzhi")  
        Shou_tree2 <- Shouzhi_tree2$AddChild("Shou")
        Bianhuafudushou_tree2 <- Shou_tree2$AddChild("Bianhuafudushou")
        Jinefudushou_tree2 <- Bianhuafudushou_tree2$AddChild("Jinefudushou")
        Pinlvfudushou_tree2 <- Bianhuafudushou_tree2$AddChild("Pinlvfudushou")
        Bianhuaqushishou_tree2 <- Shou_tree2$AddChild("Bianhuaqushishou")
        Jinequshishou_tree2 <- Bianhuaqushishou_tree2$AddChild("Jinequshishou")
        Pinlvqushishou_tree2 <- Bianhuaqushishou_tree2$AddChild("Pinlvqushishou")
        Zhi_tree2 <- Shouzhi_tree2$AddChild("Zhi")
        Bianhuafuduzhi_tree2 <- Zhi_tree2$AddChild("Bianhuafuduzhi")
        Jinefudu_tree2 <- Bianhuafuduzhi_tree2$AddChild("Jinefudu")
        Pinlvfudu_tree2 <- Bianhuafuduzhi_tree2$AddChild("Pinlvfudu")
        Bianhuaqushizhi_tree2 <- Zhi_tree2$AddChild("Bianhuaqushizhi")
        Jinequshi_tree2 <- Bianhuaqushizhi_tree2$AddChild("Jinequshi")
        Pinlvqushi_tree2 <- Bianhuaqushizhi_tree2$AddChild("Pinlvqushi")
        Bilv_tree2 <- Shouzhi_tree2$AddChild("Bilv")
        Shouruzhichubi12_tree2 <- Bilv_tree2$AddChild("Shouruzhichubi12")
        Shouruzhichubiqushi_tree2 <- Shouruzhichubi12_tree2$AddChild("Shouruzhichubiqushi")
        Shouruzhichubifudu_tree2 <- Shouruzhichubi12_tree2$AddChild("Shouruzhichubifudu")
        Xiaofeizhichubi12_tree2 <- Bilv_tree2$AddChild("Xiaofeizhichubi12")
        Xiaofeizhichubiqushi_tree2 <- Xiaofeizhichubi12_tree2$AddChild("Xiaofeizhichubiqushi")
        Xiaofeizhichubifudu_tree2 <- Xiaofeizhichubi12_tree2$AddChild("Xiaofeizhichubifudu")
        Zichanfuzhai_tree2 <-  Lvyuenengli_tree2$AddChild("Zichanfuzhai")          
        Shehuiguanxi_tree2 <- tree2$AddChild("Shehuiguanxi")
        Zaixianshichang_tree2 <- Shehuiguanxi_tree2$AddChild("Zaixianshichang")
        Shoujixiaofei_tree2 <- Shehuiguanxi_tree2$AddChild("Shoujixiaofei")
        Shenfentezhi_tree2 <- tree2$AddChild("Shenfentezhi")
        Gaoguanrenzhi_tree2 <-  Shenfentezhi_tree2$AddChild("Gaoguanrenzhi")
        #GerentouziShen_tree2 <-  Shenfentezhi_tree2$AddChild("GerentouziShen")
        Juzhudi_tree2 <-  Shenfentezhi_tree2$AddChild("Juzhudi")
        Xinyonglishi_tree2 <- tree2$AddChild("Xinyonglishi")
        Huoyuedu_tree2 <- Xinyonglishi_tree2$AddChild("Huoyuedu")
        Xinyongkaxiaofeijine_tree2 <- Huoyuedu_tree2$AddChild("Xinyongkaxiaofeijine")
        Xinyongkaxiaofeibishu_tree2 <- Huoyuedu_tree2$AddChild("Xinyongkaxiaofeibishu")
        Xinyongkaxiaofeijinebi_tree2 <- Huoyuedu_tree2$AddChild("Xinyongkaxiaofeijinebi")
        Xinyongkaxiaofeicishubi_tree2 <- Huoyuedu_tree2$AddChild("Xinyongkaxiaofeicishubi")
        Shouxindu_tree2 <- Xinyonglishi_tree2$AddChild("Shouxindu")
        huankuanjinebi_tree2 <- Shouxindu_tree2$AddChild("huankuanjinebi")
        huankuanjinebilv_tree2 <- Shouxindu_tree2$AddChild("huankuanjinebilv")
        huankuantuoqiane_tree2 <- Shouxindu_tree2$AddChild("huankuantuoqiane")
        
					 
        ## �������ݻ���
        tree3 <- Node$new("Hangkonghuaxiang")
                Lvyuenengli5_tree3 <- tree3$AddChild("Lvyuenengli5")
                        Guonei5_tree3 <-  Lvyuenengli5_tree3$AddChild("Guonei5")
                                Zuipinfenchengshi5_tree3 <- Guonei5_tree3$AddChild("Zuipinfenchengshi5")
                                Yixianchengshizhanbi5_tree3 <- Guonei5_tree3$AddChild("Yixianchengshizhanbi5")
                                Yierxianchengshizhanbi5_tree3 <- Guonei5_tree3$AddChild("Yierxianchengshizhanbi5")
                                Juzhudizuipinfan5_tree3 <- Guonei5_tree3$AddChild("Juzhudizuipinfan5")
                                Juzhudizuipinfanqiansan5_tree3 <- Guonei5_tree3$AddChild("Juzhudizuipinfanqiansan5")
                        Cishu5_tree3 <- Lvyuenengli5_tree3$AddChild("Cishu5")
                                Guoneicishu5_tree3 <- Cishu5_tree3$AddChild("Guoneicishu5")
                                Guojicishu5_tree3 <- Cishu5_tree3$AddChild("Guojicishu5")  
                                Guoneiguojicishu5_tree3 <- Cishu5_tree3$AddChild("Guoneiguojicishu5")  
                Xinyonglishi4_tree3 <- tree3$AddChild("Xinyonglishi4")
                        Xingcheng4_tree3 <- Xinyonglishi4_tree3$AddChild("Xingcheng4")
                                Feixingcishu4_tree3 <- Xingcheng4_tree3$AddChild("Feixingcishu4")
                                Licheng4_tree3 <- Xingcheng4_tree3$AddChild("Licheng4")
                                Lichengcishu4_tree3 <- Xingcheng4_tree3$AddChild("Lichengcishu4")
                        Jiage4_tree3 <- Xinyonglishi4_tree3$AddChild("Jiage4")
                                Zongjiage4_tree3 <- Jiage4_tree3$AddChild("Zongjiage4")
                                Jiagecishubi4_tree3 <- Jiage4_tree3$AddChild("Jiagecishubi4")
                                Jiagelichengbi4_tree3 <- Jiage4_tree3$AddChild("Jiagelichengbi4")
                        Cangwei4_tree3 <- Xinyonglishi4_tree3$AddChild("Cangwei4")
                                Toudengcangcishu4_tree3 <- Cangwei4_tree3$AddChild("Toudengcangcishu4")
                                Toudengcangshangwucangcishu4_tree3 <- Cangwei4_tree3$AddChild("Toudengcangshangwucangcishu4")
                                Toudengcangzhanbi4_tree3 <- Cangwei4_tree3$AddChild("Toudengcangzhanbi4")
                                Toudengcangshangwucangzhanbi4_tree3 <- Cangwei4_tree3$AddChild("Toudengcangshangwucangzhanbi4_tree3")
                        Hangkonggongsi4_tree3 <- Xinyonglishi4_tree3$AddChild("Hangkonggongsi4")
                Shenfentezhi3_tree3 <- tree3$AddChild("Shenfentezhi3")
                        Nianling3_tree3 <- Shenfentezhi3_tree3$AddChild("Nianling3")
                        Juzhudi3_tree3 <- Shenfentezhi3_tree3$AddChild("Juzhudi3")
                Xingweipianhao2_tree3 <- tree3$AddChild("Xingweipianhao2")
                        Fanmangyuechengjishu2_tree3 <- Xingweipianhao2_tree3$AddChild("Fanmangyuechengjishu2")
                        Zuihouyicifeixingjujin2_tree3 <- Xingweipianhao2_tree3$AddChild("Zuihouyicifeixingjujin2")
                Shehuiguanxi1_tree3 <- tree3$AddChild("Shehuiguanxi1")
                        Chengshigeshu1_tree3 <- Shehuiguanxi1_tree3$AddChild("Chengshigeshu1")
                        Yixianchengshi1_tree3 <- Shehuiguanxi1_tree3$AddChild("Yixianchengshi1")
                        Chengshijizhongdu1_tree3 <- Shehuiguanxi1_tree3$AddChild("Chengshijizhongdu1")
              
        tree1$Set(weight = c(10,2,4,4,3,2,8,0,3,1,
                             5,4,2,1,5,4,2,5,5,2,
                             5,5,5,5,3,8,3,7,3,3,
                             2,3,7,4,2,4,3,5,7,3,
                             4,6,4,2,6,4,6,4,1,5,
                             5,1,5,5,3,4,8,2,2,6,
                             3,3,2,2,2,6,4,1,5,5))
        
        tree2$Set(weight = c(10,2,5,5,5,5,5,5,5,5,
                             5,5,3,8,3,7,7,3,3,7,
                             3,3,7,7,3,3,7,3,4,6,
                             3,7,4,3,7,2,1,5,5,1,
                             5,5,3,4,2,2,3,3,6,5,
                             5,5))
        
        tree3$Set(weight = c(1,5,5,6,4,4,1,1,4,1,
                             1,1,4,2,2,2,1,2,1,1,
                             1,1,1,1,1,1,1,3,4,1,
                             2,1,2,1,2,3,2))
                        
        list(tree1=tree1,tree2=tree2,tree3=tree3)
}

credit_scores <- function(trees1,scores1,nleaf){
        
        n1 <- 1:nleaf[1]
        n2 <- (nleaf[1]+1):nleaf[2]
        n3 <- (nleaf[2]+1):nleaf[3]
        
        tree1 <- trees1$tree1
        tree1$Set(value = scores1[n1], filterFun = isLeaf)
        tree1$Do(function(node) node$value <- Valuef(node)[1], filterFun = isNotLeaf)
        result1 <- Get(list(tree1$Shehuiguanxi,tree1$Shenfentezhi,tree1$Xinyonglishi,tree1$Xingweipianhao,tree1$Lvyuenengli,tree1),attribute = "value")
        
        tree2 <- trees1$tree2
        tree2$Set(value = scores1[n2], filterFun = isLeaf)
        tree2$Do(function(node) node$value <- Valuef(node)[1], filterFun = isNotLeaf)
        result2 <- Get(list(tree2$Shehuiguanxi,tree2$Shenfentezhi,tree2$Xinyonglishi,tree2$Xingweipianhao,tree2$Lvyuenengli,tree2),attribute = "value")
        
        tree3 <- trees1$tree3
        tree3$Set(value = scores1[n3], filterFun = isLeaf)
        tree3$Do(function(node) node$value <- Valuef(node)[1], filterFun = isNotLeaf)
        result3 <- Get(list(tree3$Shehuiguanxi1,tree3$Shenfentezhi3,tree3$Xinyonglishi4,tree3$Xingweipianhao2,tree3$Lvyuenengli5,tree3),attribute = "value")

        #result4 <- sapply(1:length(result1), function(i) weighted.mean(c(result1[i], result3[i]), w=c(3,1), na.rm = TRUE))
        result4 <- sapply(1:length(result2), function(i) weighted.mean(c(result2[i], result1[i], result3[i]), w=c(3,4,1), na.rm = TRUE))
        
        result2[2] <- result1[2] ##!!!!!
        result <- c(result2,result1,result3,result4)
        
        # ### test output !!!
        aa1 <- tree1$Get("value")
        aa2 <- tree2$Get("value")
        aa3 <- tree3$Get("value")
        result <- c(aa1,aa2,aa3,result4)
        
        result  
}

Valuef <- function(node) {
        if(length(node$value)>0){
                result <- c(node$value,node$weight)
        }else{
                tmp <- sapply(node$children,Valuef)
                onev <- weighted.mean(x=tmp[1,],w=tmp[2,],na.rm = TRUE)
                result <- c(onev,node$weight)
        }
        
        return (result)
}

#### pieces of functions to deal with raw fields0 with text

flight<-function(s){
        f<-substring(s,1,1)
        if(f=="��" | f=="��" | f=="��"){
                return(1)
        }else if (f =='��'){
                return(0.5)
        }else{
                return(0)
        }
}

idcard_age<-function(s){
        birthday<-substr(s,7,10)# ����֤��������
        age<-as.numeric(format(Sys.Date(),format='%Y'))-as.numeric(birthday)
        
        return(age)
}

city<-function(s){
        d <- read.csv('city_grade.csv', header = T)
        d[,1]<-as.character(d[,1])
        k<-0
        if(any(d$city==s)) k<-d$score[which(d$city==s)]
        
        return(k)
}

idcard<-function(s){
        d2<-read.csv('id_city.csv',header = T) # ǰ4λ-���ж�Ӧ��
        d2[,2] <- as.character(d2[,2])
        d2[,2] <- gsub("\t","",d2[,2])
        d2[,2] <- gsub(" ","",d2[,2])
        s4<-substr(s,1,4) # ����֤ǰ4λ
        return(as.character(d2[d2[,1]==s4,2]))
}

lastflight<-function(s){
        if(!( any(grepl("-",s)) | any(grepl("/",s)) ) ) s <- paste(substr(s,1,4),substr(s,5,6),substr(s,7,8),sep="-")
        s<-as.Date(s,'%Y%m%d')
        gap<-difftime(Sys.Date(),s,units='days')
        return(gap)
}

Hangkong_y1_5<-function(a,b,a0){
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

split_city<-function(a){
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
