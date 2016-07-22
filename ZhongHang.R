#' Codes for Bank of China
#' 
#' More details please contact: Qiang Huang. 
#' Copyright reserved by Qiang Huang.
#' 
#'====================================================
#' Qiang Huang, 2016/07/20, huangqiang@3golden.com.cn
#'====================================================


### only for test ======================================================================
setwd("D:\\data\\中行个人征信\\中行个人征信共享")
rawFile <- "民生坏样本.csv"
weights <- list()
weights[[1]] <- c(2,3,5,0,2,3,5,0,2,3,5,0,1,2,4,0,1,3,4,0,1,2,3,0,1,2,3,2,1,0,5,3,2,0)
weights[[2]] <- c(3,3,4,0,3,2,0,3,3,4,0)
weights[[3]] <- c(4,3,3,0)
#========================================================================================

## 输入是：指标体系，包含缺失数据；和自定义的权重； 输出：每个人的打分。
## 输入的具体格式是： 姓名 身份证号 手机号 日期 指标1 指标2 指标3
## 输出包括长期和短期。
## 输出的具体格式： 姓名 身份证号 手机号 短期信用评分 长期信用评分

library(lubridate)
library(data.table)
library(zoo)
library(bit64)

### useful functions
normalize_hq <- function(x,flag=1) {
        x <- as.numeric(x)
        if(flag==1){
                return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
        }else if(flag==2){
                return(1/(1+exp(-x)))
        }else if(flag==3){
                x <- x/(x+median(x,na.rm=T))
                return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
        }else if(flag==4){
                return( 1 - (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) )
        }else if(flag==5){
                return(1/(x+1)) 
        }else if(flag==6){
                x <- 1/(1+exp(-x))
                return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
        }
}

groupByDate_hq <- function(dt,flag=1){
        
        year <- year(dt[,'date'])
        if(flag==1){
                month <- month(dt[,'date'])
                gs <- paste(year, month, sep="-")
        }else if(flag==2){
                season <- quarter(dt[,'date'])
                gs <- paste(year, season, sep="-")
        }else if(flag==3){
                gs <- year
        }else if(flag==4){
                tmp <- seq(min(year),max(year),by=3)
                gs <- sapply(year, function(i) max(tmp[tmp<=i]))
                gs <- paste(gs,3,sep="_")
        }else if(flag==5){
                tmp <- seq(min(year),max(year),by=5)
                gs <- sapply(year, function(i) max(tmp[tmp<=i]))
                gs <- paste(gs,5,sep="_")
        }
        
        tmpdata <- sapply(1:ncol(dt), function(i)  sapply(unique(gs), function(ix) median(dt[gs==ix,i])) )
        colnames(tmpdata) <- colnames(dt)                
        
        tmpdata
}

tof <- function(oneV,na.rm=FALSE,fill=FALSE,f="mean"){
        ## replace function of as.numeric
        options(warn=-1)
        oneV <- as.numeric(oneV)
        options(warn=0)
        
        if(na.rm) oneV <- oneV[!is.na(oneV)]
        if(fill){
                if(f=="mean") oneV[is.na(oneV)] <- mean(oneV[!is.na(oneV)])
                if(f=="min") oneV[is.na(oneV)] <- min(oneV[!is.na(oneV)])
                if(f=="median") oneV[is.na(oneV)] <- median(oneV[!is.na(oneV)])
                if(f=="approx") oneV <- na.approx(oneV,maxgap=Inf,na.rm=FALSE)
        }
        oneV
}

rating <- function(n,oneM){
        if ( n == 1 ){
                oneW <- weights[[n]]
                tmpsub <- c(0,which(oneW==0))
                out <- sapply( 1:(length(tmpsub)-1), function(i) oneM[,(tmpsub[i]+2-i):(tmpsub[i+1]-i)] %*% oneW[(tmpsub[i]+1):(tmpsub[i+1]-1)] )
        } else {
                oneM <- rating(n-1,oneM)
                oneW <- weights[[n]]
                tmpsub <- c(0,which(oneW==0))
                out <- sapply( 1:(length(tmpsub)-1), function(i) oneM[,(tmpsub[i]+2-i):(tmpsub[i+1]-i)] %*% oneW[(tmpsub[i]+1):(tmpsub[i+1]-1)] )
        }
        
        return(out)
}

#### step 1: read and clean raw data;
####==========================================
options(warn=-1)
dt <- fread(rawFile, strip.white = TRUE, sep=",", options(datatable.fread.datatable=FALSE))
options(warn=0)

dt <- as.matrix(dt)
dt <- dt[ , c(1,3,68,11,12:18,20:35,38,41,42),drop=FALSE]#!!!!! only for test

indexs <- colnames(dt)

# clean the currency info
dt <- sapply(1:ncol(dt), function(i) gsub(' ', '', dt[ ,i]))

# data type transform
# dt$count <- as.numeric(dt$count)
# mode(dt) <- "numeric"

# data index normalization
tmp <- sapply(5:ncol(dt), function(i) normalize_hq(dt[,i]) )
dt <- cbind(dt[ , 1:4], tmp)

# clean the date info 
dt[,4] <- gsub('\\.','\\-',dt[, 4])

# remove the NA records
dt <- na.omit(dt)

colnames(dt) <- indexs
####=======================================



gflags <- c(3,4) ## 长期评分和短期评分？？？？？
nHier <- length(weights)

#for(flag in gflags){
        
        #### step 2: rating different indexs;
        ####=======================================
        dtOne <- dt #groupByDate_hq(dt,flag=3)
        indexM <- dtOne[, -(1:4)]
        mode(indexM) <- "numeric"
        indexMerged <- rating(nHier,indexM)

#}

####=======================================

#### step 3: merge and output 
####=======================================

persons <- paste(dtOne[,1],dtOne[,2],sep="_")
indexNew <- aggregate(indexMerged, list(persons), mean)
dtWrite <- cbind(dtOne[match(indexNew[,1],persons),1:4],indexNew[,2])

# rate the client by weighted score
bins <- quantile(indexNew[,2], probs=c(0,pnorm(-1.5),pnorm(-0.5),pnorm(0.5),pnorm(1.5),1),na.rm=T)
labels <- as.vector(cut(indexNew[,2], breaks=bins, labels=c('E','D','C','B','A')))
labels[is.na(indexNew[,2])] <- 'E'  #!!!!!


dtWrite <- cbind(dtWrite,labels)
colnames(dtWrite) <- c("姓名","身份证号","手机号","到期日期","短期信用评分","等级")
write.csv(dtWrite, file="评分结果.csv", row.names=FALSE,quote=FALSE)
