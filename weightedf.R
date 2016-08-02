source('D:/code/CreditCardTest/Credit_v0_1.R')
library(data.tree)

tree0f <- function(){
        
        ## ×ÛºÏ»­ÏñÊ÷Í¼
        tree0 <- Node$new("ZongheHuaxiang")
        
        Jin_tree0 <- tree0$AddChild("Jin")
        Shehuiguanxi_Jin <- Jin_tree0$AddChild("Shehuiguanxi_Jin")
        Shenfentezhi_Jin <- Jin_tree0$AddChild("Shenfentezhi_Jin")
        Xinyonglishi_Jin <- Jin_tree0$AddChild("Xinyonglishi_Jin")
        Xingweipianhao_Jin <- Jin_tree0$AddChild("Xingweipianhao_Jin")
        Lvyuenengli_Jin <- Jin_tree0$AddChild("Lvyuenengli_Jin")
        
        Yuan_tree0 <- tree0$AddChild("Yuan")
        Shehuiguanxi_Yuan <- Yuan_tree0$AddChild("Shehuiguanxi_Yuan")
        Shenfentezhi_Yuan <- Yuan_tree0$AddChild("Shenfentezhi_Yuan")
        Xinyonglishi_Yuan <- Yuan_tree0$AddChild("Xinyonglishi_Yuan")
        Xingweipianhao_Yuan <- Yuan_tree0$AddChild("Xingweipianhao_Yuan")
        Lvyuenengli_Yuan <- Yuan_tree0$AddChild("Lvyuenengli_Yuan")
        
        Hang_tree0 <- tree0$AddChild("Hang")
        Shehuiguanxi_Hang <- Hang_tree0$AddChild("Shehuiguanxi_Hang")
        Shenfentezhi_Hang <- Hang_tree0$AddChild("Shenfentezhi_Hang")
        Xinyonglishi_Hang <- Hang_tree0$AddChild("Xinyonglishi_Hang")
        Xingweipianhao_Hang <- Hang_tree0$AddChild("Xingweipianhao_Hang")
        Lvyuenengli_Hang <- Hang_tree0$AddChild("Lvyuenengli_Hang")
        
        tree0
}

weightedf <- function(wV){
        valM <- aa[c(4:8,10:14,16:20), ]
        
        oneV <- sapply(1:ncol(valM), function(i){
                tree0 <- tree0f()
                tree0$Set(weight = wV)
                tree0$Set(value = valM[,i], filterFun = isLeaf)
                tree0$Do(function(node) node$value <- Valuef(node)[1], filterFun = isNotLeaf)
                tree0$value
                })
        
        
        f <- KS_curves(oneV[(n.bad+1):n.sam],oneV[1:n.bad],main="",plot=FALSE)
        
        -f
}


n.bad <- 100
n.sam <- 504
load("outputIndex504")
mode(aa) <- "numeric"

wV0 <- runif(19,0,100) #c(1,3,1,2,3,4,5,4,1,2,3,4,5,1,1,2,3,4,5)
res <- optim(wV0,weightedf,control = list(trace=1,maxit=100,fnscale=1))

#weightedf(c(1,3,1,1,3,2,3,4,1,1,3,2,3,1,1,2,3,4,5))
#optimize(weightedf,)
