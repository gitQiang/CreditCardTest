library(data.tree)

Credit_v0 <- function(fields0){
        ## 输入原始字段：fields0; 原始字段顺序见文件：Dictionary_HQ_raw_v0.csv
        ## 输出打分格式： 姓名 身份证号 手机号 金融画像打分近期6个数字（五个部分和一个综合打分） 金融画像打分远期6个数字（五个部分和一个综合打分） 飞行画像打分6个数（5部分和一个综合打分） 金融远期和飞行打分综合6个数 （五个部分和一个综合打分）
        
        
        ## step 1: 由原始字段到衍生字段； 函数：transform_fileds； 衍生字段表见文件：
        fields1 <-  transform_fileds(fields0)
        
        ## step 2: 提取每个衍生字段的具体打分； 函数：fields_scores； 衍生字段具体打分表见文件：
        scores1 <- fields_scores(fields1)
        
        ## step 3: 预构建金融画像和飞行画像的树；函数：trees_construct; 金融画像树状结构见文件：  飞行画像树状结构见文件：
        trees1 <- trees_construct()
        
        ## step 4: 计算每一个层次的打分，并获得最终输出打分； 函数：credit_scores;
        scores <- credit_scores(trees1,scores1)
        
        ## step 5: Output
        
        
        
}

transform_fileds <- function(fields0){
        
}

fields_scores <- function(fields1){

        
        
        #航空字段的得分    
        breaksy <- list()
        breaksy[[1]] <- c()
        breaksy[[2]] <- c(0,0.3,0.7,1)
        
}

getBreaks <- function(){
        #航空字段的得分 
        
        
        
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
                        Nianling_tree1 <-  Shenfentezhi_tree1$AddChild("Shenfentezhi")
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
        
        tree1$Set(weight = c(10,2,4,4,3,2,8,0,3,1,5,4,2,1,5,4,2,5,5,2,5,5,5,5,3,8,4,7,3,4,2,3,7,4,2,4,3,5,7,3,2,6,4,2,6,4,6,4,1,5,5,1,5,5,3,4,8,2,2,6,3,3,2,2,2,6,4,1,5)/10)
                                
                                
                                
        ## 金融画像近期
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
        
        tree2$Set(weight = c(2,5,5,5,5,5,5,5,5,5,5,5,3,8,4,7,7,3,3,7,3,4,7,7,3,3,7,3,2,6,3,7,4,3,7,2,1,5,5,1,5,5,3,4,2,2,3,3,6,5,5)/10)                
                                
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
                        Fanmangyuechengjishu2_tree3 <- Xingweipianhao2_tree3$AddChild("Xingweipianhao2")
                        Zuihouyicifeixingjujin2_tree3 <- Xingweipianhao2_tree3$AddChild("Zuihouyicifeixingjujin2")
                Shehuiguanxi1_tree3 <- tree3$AddChild("Shehuiguanxi1")
                        Chengshigeshu1_tree3 <- Shehuiguanxi1_tree3$AddChild("Chengshigeshu1")
                        Yixianchengshi1_tree3 <- Shehuiguanxi1_tree3$AddChild("Yixianchengshi1")
                        Chengshijizhongdu1_tree3 <- Shehuiguanxi1_tree3$AddChild("Chengshijizhongdu1")
                        
        tree3$Set(weight = c(1,5,5,3,2,2,1,1,4,1,1,1,4,2,2,2,1,2,1,1,1,1,1,1,1,1,1,3,4,1,2,1,2,1,2,3,2))
                        
        list(tree1=tree1,tree2=tree2,tree3=tree3)
}

credit_scores <- function(trees1,scores1){
        
        tree3 <- trees1$tree3
        tree3$Set(scoreF = c(function(self) { 
                x <- sapply(self$children, function(child) GetAttribute(child, "value", format = identity) );
                w <- sapply(self$children, function(child) GetAttribute(child, "weight", format = identity) );
                weighted.mean(x,w,na.rm = TRUE) 
                }), filterFun = isNotLeaf)
        print(tree3, "scoreF")
}
