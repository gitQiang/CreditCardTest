load('coefList')
load('muList')
load('sigmaList')

onlinePredict <- function(onedt, flag='liantong', NAm=2){
        
        onedt <- as.matrix(onedt, nrow=1)
        onesam <- DataClean_Predict(onedt, flag=flag)
        
        flags <- c('liantong','yidong','idcard')
        
        k <- ifelse(flag %in% flags, which(flag==flags), 3)
        if(NAm == 1) onesam[is.na(onesam)] <- 0
        if(NAm == 2){
                subs <- which(is.na(onesam))
                for(u in subs) onesam[u] <- rnorm(1, muList[[k]][u], sigmaList[[k]][u])
        }
        
        alp <- sum(coefList[[k]] * c(1,as.numeric(onesam)))
        
        s <- 1/(1+exp(-alp))       
        
        s
}  
    
        


DataClean_Predict <- function(dt, flag){
        
        if(flag=="liantong"){
                ### build new indelxes for ChinaUnicom 
                newindexes_liantong <- c("性别", "最近三个月的信用卡消费金额", "最近三个月的信用卡消费次数", "最近三个月的信用卡还款金额", "最近三个月的信用卡还款次数", "最近十二个月的信用卡消费金额", "最近十二个月的信用卡消费次数", "最近十二个月的信用卡还款金额", "最近十二个月的信用卡还款次数", "最近三个月的借记卡支出金额", "最近三个月的借记卡支出次数", "最近三个月的借记卡收入金额", "最近三个月的借记卡收入次数", "最近十二个月的借记卡支出金额", "最近十二个月的借记卡支出次数", "最近十二个月的借记卡收入金额", "最近十二个月的借记卡收入次数", "借贷比", "信用卡消费金额长短期", "信用卡消费笔数长短期", "信用卡还款金额长短期", "信用卡还款笔数长短期", "借记卡支出金额长短期", "借记卡支出笔数长短期", "借记卡收入金额长短期", "借记卡收入笔数长短期", "信用卡近三个月的平均单次消费金额", "信用卡近三个月的平均单次还款金额", "信用卡近十二个月的平均单次消费金额", "信用卡近十二个月的平均单次还款金额")
                
                dt1 <- dt[, newindexes_liantong]
        }
        
        if(flag=="yidong"){
                ### build new indelxes for China Mobile
                newindexes_yidong <- c("性别", "借记卡余额总量", "借记卡3个月出账金额", "借记卡3个月入账金额", "借记卡12个月出账金额", "借记卡12个月入账金额", "信用卡数量", "借贷比", "借记卡入账长短比", "借记卡出账长短比", "借记卡3个月出入比", "借记卡12个月出入比")
                
                a1 <- (dt[,"借记卡12个月入账金额"] - dt[,"借记卡3个月入账金额"])/9
                a2 <- (dt[,"借记卡12个月入账金额"]/12 - a1)
                a3 <- (dt[,"借记卡12个月出账金额"] - dt[,"借记卡3个月出账金额"])/9
                a4 <- (dt[,"借记卡12个月出账金额"]/12 - a3)
                a5 <- a4/a2
                newderive_yidong <- cbind(a1, a2, a3, a4, a5)
                colnames(newderive_yidong) <- c("入账均9", "入账差9", "出账均9", "出账差9", "出入比9")
                
                dt1 <- cbind(dt[, newindexes_yidong], newderive_yidong)
        }
        
        if(!(flag %in% c("yidong","liantong"))){
                newindex_id <- c("性别", "移动", "联通", "华北区", "东北区",   "华东区", "华中华南", "西南区")
                dt1 <- dt[,newindex_id]  
        }
        
        ### normalization of features
        for(i in 1:ncol(dt1)){
                if(median(dt1[,i], na.rm = TRUE) > 500 ) dt1[,i] <- log(1+dt1[,i])
        } 
        
        dt1       
}