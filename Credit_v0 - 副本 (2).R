library(data.tree)

## only for test
test <- function(){
        rm(list=ls())
        gc()
        
        setwd("D:/data/中行个人征信/中行个人征信共享")
        source('D:/code/CreditCardTest/Credit_v0.R')
        samples <- read.csv("数据样本0724.csv")
        samples <- samples[,1:60]

        aa <- 1:nrow(samples)
        for(i in 1:nrow(samples)){
                print(i)
                fields0 <- samples[i,]
                tmp  <- Credit_v0(fields0)
                aa[i] <- as.numeric(tmp[14])
                
                #cat(unlist(tmp$fields0), file="RawFields.txt", append=TRUE, sep = "\n")
                #cat(unlist(tmp$fields1), file="DeriveFields.txt", append=TRUE, sep = "\n")
        }
        
        plot(1:length(aa), as.numeric(aa),type="b")
        abline(v=21.5,col=2,lwd=2)
        
}

Credit_v0 <- function(fields0){
        ## 输入原始字段：fields0; 原始字段顺序见文件：Dictionary_HQ_raw_v0.csv
        ## 输出打分格式： 姓名 身份证号 手机号 金融画像打分近期6个数字（五个部分和一个综合打分） 金融画像打分远期6个数字（五个部分和一个综合打分） 飞行画像打分6个数（5部分和一个综合打分） 金融远期和飞行打分综合6个数 （五个部分和一个综合打分）
        
        ## step 0: 由字符串解析出原始字段； 函数： ；原始字段表见文件：
        personOne <- fields0[1:3]
        fields0 <- fields0[-(1:3)]
        
        for(i in 1:length(fields0)){
                if(grepl("]",fields0[i]) | grepl("\\[",fields0[i]) | grepl("\\(",fields0[i]) | grepl("\\)",fields0[i])){
                        tmp <- unlist(strsplit(as.character(fields0[i]),",") )[1]
                        tmp <- gsub("\\[","",tmp)
                        tmp <- gsub("]","",tmp)
                        tmp <- gsub("\\(","",tmp)
                        tmp <- gsub("\\)","",tmp)
                        fields0[i] <- as.numeric(tmp)
                        if(fields0[i]==0) fields0[i]=1
                }
        }
        
        shenfenzhengNUM <- personOne[2]
        
        ## step 1: 由原始字段到衍生字段； 函数：transform_fileds； 衍生字段表见文件：
        trans <-  transform_fileds(fields0,shenfenzhengNUM)
        fields1 <- trans$fields1
        #fields1[is.na(fields1)] <- 0
        nleaf <- trans$nleaf
                
        ## step 2: 提取每个衍生字段的具体打分； 函数：fields_scores； 衍生字段具体打分表见文件：
        scores1 <- fields_scores(fields1,nleaf)
        scores1 <- as.numeric(scores1)
        #scores1[is.na(scores1)] <- 0
        
        ## step 3: 预构建金融画像和飞行画像的树；函数：trees_construct; 金融画像树状结构见文件：  飞行画像树状结构见文件：
        trees1 <- trees_construct()
        
        ## step 4: 计算每一个层次的打分，并获得最终输出打分； 函数：credit_scores;
        scores <- credit_scores(trees1,scores1,nleaf)
        
        ## step 5: Output
        result <- unlist(c(personOne,scores))
        result
        
        #list(result=result,fields0=fields0, fields1=fields1)
}

transform_fileds <- function(fields0, shenfenzhengNUM){
        
        fields0I <- fields0
        fields0 <- as.numeric(fields0[1:39])
        fieldsHangkong <- fields0I[40:length(fields0I)]
        
        nleaf <- 1:3
        fields1 <- 1:98
        ## 金融画像远期原始字段到衍生字段映射
        n <- 0
        fields1[n+1] <- fields0[n+9]/fields0[n+8]
        fields1[n+2] <- fields0[n+2]
        fields1[n+3] <- fields0[n+10]
        fields1[n+4] <- 0
        fields1[n+5] <- fields0[n+35]
        fields1[n+6] <- fields0[n+17]
        fields1[n+7] <- fields0[n+16]
        fields1[n+8] <- fields0[n+2]/fields0[n+35]
        fields1[n+9] <- fields0[n+10]/fields0[n+17]
        fields1[n+10] <- 0
        fields1[n+11] <- fields0[n+4]/fields0[n+10]
        fields1[n+12] <- fields0[n+15]/fields0[n+17]
        fields1[n+13] <- fields0[n+3]
        fields1[n+14] <- fields0[n+1]
        fields1[n+15] <- fields0[n+36]
        
        #lvyuenengli
        fields1[n+16] <- fields0[n+8]/fields0[n+12]
        fields1[n+17] <- fields0[n+7]/fields0[n+12]
        fields1[n+18] <- fields0[n+9]/fields0[n+12]
        fields1[n+19] <- (fields0[n+10]+fields0[n+17])/12
        fields1[n+20] <- (fields0[n+15]+fields0[n+4])/12
        fields1[n+21] <- fields0[n+17]/fields0[n+16]
        fields1[n+22] <- fields0[n+16]/12
        fields1[n+23] <- fields0[n+6]/12
        fields1[n+24] <- fields0[n+5]/12
        fields1[n+25] <- fields0[n+8]/(fields0[n+17]+fields0[n+6])
        fields1[n+26] <- (fields0[n+10]+fields0[n+17])/(fields0[n+17]+fields0[n+6])
        fields1[n+27] <- fields0[n+37]
        fields1[n+28] <- fields0[n+8]
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
        fields1[n+36] <- fields0[n+17]/fields0[n+12]
        fields1[n+37] <- fields0[n+16]/fields0[n+12]
        fields1[n+38] <- fields0[n+15]/fields0[n+17]
        fields1[n+39] <- fields0[n+17]/fields0[n+16]
        fields1[n+40] <- fields0[n+39]
        fields1[n+41] <- fields0[n+12]/12
        fields1[n+42] <- fields0[n+11]/12
        fields1[n+43] <- fields0[n+12]/fields0[n+18]
        
        
        ## 金融画像近期原始字段到衍生字段映射
        n1 <- 43
        nleaf[1] <- n1
        
        fields1[n1+1] <- fields0[n+24]/fields0[n+23]
        fields1[n1+2] <- fields0[n+24]/fields0[n+9]
        fields1[n1+3] <- fields0[n+25]/fields0[n+17]
        fields1[n1+4] <- 0
        fields1[n1+5] <- fields0[n+19]/fields0[n+25]
        fields1[n1+6] <- fields0[n+30]/fields0[n+17]
        fields1[n1+7] <- fields0[n+23]
        fields1[n1+8] <- fields0[n+22]
        fields1[n1+9] <- (fields0[n+23]/fields0[n+3])/(fields0[n+8]/fields0[n+12])
        fields1[n1+10] <- (fields0[n+22]/fields0[n+3])/(fields0[n+7]/fields0[n+12])
        fields1[n1+11] <- fields0[n+32]+fields0[n+21]
        fields1[n1+12] <- fields0[n+31]+fields0[n+20]
        fields1[n1+13] <- ((fields0[n+32]+fields0[n+21])/fields0[n+3])/((fields0[n+17]+fields0[n+6])/fields0[n+12])
        fields1[n1+14] <- ((fields0[n+31]+fields0[n+20])/fields0[n+3])/((fields0[n+16]+fields0[n+5])/fields0[n+12])
        fields1[n1+15] <- (fields0[n+23]/(fields0[n+32]+fields0[n+21]))/(fields0[n+8]/(fields0[n+6]+fields0[n+17]))
        fields1[n1+16] <- fields0[n+23]/(fields0[n+32]+fields0[n+21])
        fields1[n1+17] <- ((fields0[n+32]+fields0[n+25])/(fields0[n+32]+fields0[n+21]))/((fields0[n+17]+fields0[n+10])/(fields0[n+17]+fields0[n+6]))
        fields1[n1+18] <- (fields0[n+32]+fields0[n+25])/(fields0[n+32]+fields0[n+21])
        fields1[n1+19] <- fields0[n+37]
        fields1[n1+20] <- fields0[n+38] # zaixianshichang
        fields1[n1+21] <- fields0[n+39] # shoujixiaofei 
        fields1[n1+22] <- 0 # gaoguanrenzhi
        fields1[n1+23] <- city(idcard(shenfenzhengNUM)) # juzhudi
        fields1[n1+24] <- fields0[n+32]
        fields1[n1+25] <- fields0[n+31]
        fields1[n1+26] <- (fields0[n+32]/fields0[n+3])/(fields0[n+17]/fields0[n+12])
        fields1[n1+27] <- (fields0[n+31]/fields0[n+3])/(fields0[n+16]/fields0[n+12])
        fields1[n1+28] <- fields0[n+27]/fields0[n+33]
        fields1[n1+29] <- (fields0[n+27]/fields0[n+33])/(fields0[n+12]/fields0[n+18])
        

        ## 航空原始字段到衍生字段映射
        n <- 0
        n2 <- 72
        nleaf[2] <- n2

        fields1[n2+19] <- flight(fieldsHangkong[n+9])
        fields1[n2+20] <- idcard_age(shenfenzhengNUM)
        fields1[n2+21] <- city(idcard(shenfenzhengNUM))
        fields1[n2+23] <- lastflight(fieldsHangkong[n+18])
        fields1[n2+c(1:5,24:26)] <- Hangkong_y1_5(fieldsHangkong[n+7], fieldsHangkong[n+8],shenfenzhengNUM) 
        
        fields1[n2+6] <- as.numeric(fieldsHangkong[n+10])
        fields1[n2+7] <- as.numeric(fieldsHangkong[n+11])
        fields1[n2+8] <- as.numeric(fieldsHangkong[n+10])/as.numeric(fieldsHangkong[n+11])
        fields1[n2+9] <- as.numeric(fieldsHangkong[n+1])
        fields1[n2+10] <- as.numeric(fieldsHangkong[n+15])
        fields1[n2+11] <- as.numeric(fieldsHangkong[n+15])/as.numeric(fieldsHangkong[n+1])
        fields1[n2+12] <- as.numeric(fieldsHangkong[n+13])*as.numeric(fieldsHangkong[n+1])
        fields1[n2+13] <- as.numeric(fieldsHangkong[n+13])
        fields1[n2+14] <- as.numeric(fields1[n+12])/as.numeric(fields1[n+10])
        fields1[n2+15] <- as.numeric(fieldsHangkong[n+4])
        fields1[n2+16] <- as.numeric(fieldsHangkong[n+4]) + as.numeric(fieldsHangkong[n+5])
        fields1[n2+17] <- as.numeric(fieldsHangkong[n+4])/as.numeric(fieldsHangkong[n+1])
        fields1[n2+18] <- as.numeric(fields1[n+16])/as.numeric(fieldsHangkong[n+1])
        fields1[n2+22] <- as.numeric(fieldsHangkong[n+2])/as.numeric(fieldsHangkong[n+1])

        
        nleaf[3] <- length(fields1)
        
        list(fields1=fields1,nleaf=nleaf)
}

fields_scores <- function(fields1,nleaf){

        n1 <- 1:nleaf[1]
        n2 <- (nleaf[1]+1):nleaf[2]
        n3 <- (nleaf[2]+1):nleaf[3]
        tmp <- getBreaks()
        
        #options(stringsAsFactors=FALSE)
        ## 金融画像的远期得分
        breaksL <- tmp$breas1
        labelsL <- tmp$labels1
        JinrongScore <- sapply(n1, function(i){
                getScores(as.numeric(fields1[i]), breaksL[[i]])
                #cut(as.numeric(fields1[i]), breaksL[[i]],labelsL[[i]])
        })
        JinrongScore <- as.vector(JinrongScore)
        
        ## 金融画像的远期得分
        breaksL <- tmp$breas2
        labelsL <- tmp$labels2
        JinrongScore1 <- sapply(n2, function(i){
                getScores(as.numeric(fields1[i]), breaksL[[i-max(n1)]])
        })
        JinrongScore1 <- as.vector(JinrongScore1)
        
        #航空字段的得分
        breaksL <- tmp$breas
        labelsL <- tmp$labels

        HangkongScore <- sapply(n3, function(i){
                getScores(as.numeric(fields1[i]), breaksL[[i-max(n2)]])
                })
        HangkongScore <- as.vector(HangkongScore)


        scores1 <- c(JinrongScore, JinrongScore1, HangkongScore)
        
        scores1
}

getBreaks <- function(){
        
        ## 金融画像远期字段得分映射
        jinrongziduan <- read.csv("金融画像衍生字段表_LY_0723_Chang.csv")
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
        
        ## 金融画像近期字段得分映射
        jinrongziduan1 <- read.csv("金融画像衍生字段表_LY_0723_Duan.csv")
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
        
        
        #航空字段的得分 
        hangkongziduan <- as.matrix( read.csv("航空数据衍生指标得分划分.csv") )
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

getScores <- function(f,breaksL){
        
        if(any(is.na(breaksL))){
                f1 <- NA
        }else if(is.na(f)){
                f1 <- NA
        }else{
                for(i in 1:nrow(breaksL)){
                        if(f>breaksL[i,1] & f <= breaksL[i,2]){
                                f1 <- breaksL[i,3]
                        }
                }
        }
        f1
}

trees_construct <- function(){

        ## 金融画像远期
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
        Jiejikayue_tree1 <- Zichanjine_tree1$AddChild("Zichanjine")
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
        
        tree1$Set(weight = c(10,2,4,4,3,2,8,0,3,1,
                             5,4,2,1,5,4,2,5,5,2,
                             5,5,5,5,3,8,3,7,3,3,
                             2,3,7,4,2,4,3,5,7,3,
                             4,6,4,2,6,4,6,4,1,5,
                             5,1,5,5,3,4,8,2,2,6,
                             3,3,2,2,2,6,4,1,5)/10)
					 
                                               
        ## 金融画像近期
        ## 金融画像 short term
        tree2 <- Node$new("JinronghuaxiangYuan")
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
        
        tree2$Set(weight = c(10,2,5,5,5,5,5,5,5,5,
                             5,5,3,8,3,7,7,3,3,7,
                             3,3,7,7,3,3,7,3,4,6,
                             3,7,4,3,7,2,1,5,5,1,
                             5,5,3,4,2,2,3,3,6,5,
                             5)/10)  

					 
        ## 飞行数据画像
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
                        
        tree3$Set(weight = c(1,5,5,3,2,2,1,1,4,1,1,1,4,2,2,2,1,2,1,1,1,1,1,1,1,1,1,3,4,1,2,1,2,1,2,3,2))
                        
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

        result4 <- sapply(1:length(result2), function(i) weighted.mean(c(result2[i], result3[i]), w=c(3,1), na.rm = TRUE))

        result2[2] <- result1[2] ##!!!!!
        result <- c(result2,result1,result3,result4)
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
        if(f %in% c('国','东','南')){
                return (1)
        }else if (f =='海'){
                return(0.5)
        }else{
                return(0)
        }
}

idcard_age<-function(s){
        birthday<-substr(s,7,10)# 身份证出生日期
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
        d2<-read.csv('id_city.csv',header = T) # 前4位-城市对应表
        d2[,2] <- as.character(d2[,2])
        d2[,2] <- gsub("\t","",d2[,2])
        d2[,2] <- gsub(" ","",d2[,2])
        s4<-substr(s,1,4) # 身份证前4位
        return(as.character(d2[d2[,1]==s4,2]))
}

lastflight<-function(s){
        if(!( any(grepl("-",s)) | any(grepl("/",s)) ) ) s <- paste(substr(s,1,4),substr(s,5,6),substr(s,7,8),sep="-")
        s<-as.Date(s,'%Y%m%d')
        gap<-difftime(Sys.Date(),s,units='days')
        return(gap)
}

Hangkong_y1_5<-function(a,b,a0){
        #输入a=a7,b=a8,id=a0
        #输出y1-y5,y26-y28
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
        u<-gsub('次次','次',gsub(' ','',a))
        t<-as.character(unlist(strsplit(u,'\\d+次,')))
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

