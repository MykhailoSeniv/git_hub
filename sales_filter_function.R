library(stats)
library(glmnet)
library(tibble)
library(jsonlite)
library(rJava)
library(xlsx)
library(plyr)
library(rlang)
library(R6)
library(dplyr)
library(reshape)
library(data.table)
library(lubridate)
library(doParallel)
library(tidyr)
library(forecast)
library(imputeTS)
library(Matrix)

library(ggplot2)


# data subset
sales_filter<-function(task,min.obs=21,nfolds.cv.glmnet=5,forecast_method="lasso"){
  
   start_time<-Sys.time()
  # data download
  sales<-as.data.table(task$sales)
  sales$date<-as.Date(sales$date)
  MP0<-task$MP
  pricelog<-task$pricelog
  
  #initial matrix
  if (is.null(task$data_matrix)){
    lager<-unique(sales$lagerID)
    filial<-unique(sales$filid)
    matrix<-as.data.frame(sales[,list(datefrom=as.Date(min(date)),dateto=as.Date(max(date))),by=c('lagerID','filid')])
  }else{
    matrix<-task$data_matrix
  }
  # sales class change
  sales<-as.data.frame(sales)
  # MP data transform
  MP<-MP0[,c("mainLagerId","filialId","marketProgramId","documentId","periodFrom","periodTo")] # structure of MP data.frame
  MP<-MP[MP$marketProgramId %in% c(1,376,378,451,397,386,568,768,749,383,384,394,530),]
  MP$periodFrom<-as.Date(MP$periodFrom)
  MP$periodTo<-as.Date(MP$periodTo)
  MP<-as.data.table(MP)
  MP<-MP[,list(date=seq(periodFrom, periodTo, by = "1 day")),by=c('mainLagerId','filialId','marketProgramId','documentId')]
  MP$isMP<-1
  MP<-unique(MP,by=c("mainLagerId","filialId","marketProgramId","date"))
  MP_long<-as.data.frame(MP)
  MP$marketProgramId<-paste('MP',MP$marketProgramId,sep="")
  MP<-MP %>%
    spread(marketProgramId, documentId, fill=0)
  MP[,5:dim(MP)[2]][MP[,5:dim(MP)[2]]>1]<-1  # fix several identical MP with different documentId
  MP<-as.data.frame(MP)
  
  ##create matrix
  matrix<-as.data.frame(matrix %>% 
                          rowwise() %>%
                          do(data.frame(.[c("lagerID","filid")],date=seq(.$datefrom,.$dateto,by="1 day"))))
  ##MATRIX FILLING
  #mp
  if (is.null(MP)){
    matrix$isMP<-0
  } else{
    matrix<-left_join(matrix,MP,by = c("date"="date","lagerID"="mainLagerId","filid"="filialId")) # join only is MP label
    matrix[is.na(matrix)]<-0
  }
  #sales
  matrix<-as.data.table(left_join(matrix,sales[,c("date","lagerID","filid","sumOut_F","kolvo_F","kolvoStore")],by=c("date","lagerID","filid")))
  
    #sales and stock check (>=0)
  matrix<-matrix[,kolvo_F:=ifelse(kolvo_F<0 | is.na(kolvo_F),0,kolvo_F)]
  matrix<-matrix[,sumOut_F:=ifelse(sumOut_F<0 | is.na(sumOut_F),0,sumOut_F)]
  matrix$sumOut_F<-ifelse(matrix$sumOut_F>0 & matrix$kolvo_F==0,0,matrix$sumOut_F)
  matrix$kolvo_F<-ifelse(matrix$kolvo_F>0 & matrix$sumOut_F==0,0,matrix$kolvo_F)
  matrix<-matrix[,kolvoStore:=ifelse(kolvoStore<0 | is.na(kolvoStore),0,kolvoStore)]
  matrix<-as.data.frame(matrix)
  #price fil_log
  pricelog<-task$pricelog
  pricelog[pricelog$activeTo>Sys.Date(),]$activeTo<-Sys.Date()
  pricelog$activeFrom<-as.Date(pricelog$activeFrom)
  pricelog$activeTo<-as.Date(pricelog$activeTo)
   
  # start<-Sys.time()
  # pricelog<-as.data.frame(pricelog %>%
  #                            rowwise() %>%
  #                            do(data.frame(.[c("lagerId","filialId","price")], date = seq(.$activeFrom, .$activeTo, by = "1 day"))))
  #  end<-Sys.time()
  #  end-start
  
   pricelog<-as.data.table(pricelog)
   pricelog<-pricelog[,list(filialId,lagerId,price,date=seq(activeFrom, activeTo, by = "1 day")),by=1:nrow(pricelog)] 
  ##
   pricelog<-pricelog[,-1]
  
  names(pricelog)<-c("lagerId","filialId","price_log","date")
  pricelog<-as.data.table(pricelog)
  pricelog<-as.data.frame(pricelog[,list(price_log=max(price_log)),by=c("lagerId","filialId","date")])
  
  matrix<-left_join(matrix,pricelog,by= c("lagerID"="lagerId","filid"="filialId","date"="date"))  

  ## FIRST out_of_stock detection 
  matrix<-as.data.table(matrix)
  matrix<-matrix[,out_of_stock:=ifelse(isMP==0 & kolvoStore==0 & kolvo_F==0,1,0)]
  
  ## matrix mod calculation & matrix nomod calculation
  matrix<-matrix[,frequency:=length(kolvo_F),by=c('lagerID','filid')]
  matrix<-matrix[,good_observ:=frequency-sum(out_of_stock),by=c('lagerID','filid')]
  matrix<-matrix[,min_kolvo:=min(kolvo_F),by=c('lagerID','filid')] 
  matrix<-matrix[,max_kolvo:=max(kolvo_F),by=c('lagerID','filid')]
  matrix<-matrix[order(date),stock_id:=rleid(out_of_stock),by=c('lagerID','filid')]
  matrix<-matrix[,stock_length:=length(date),by=c('lagerID','filid','stock_id')]
  matrix<-matrix[,count_unique:=length(unique(kolvo_F)),by=c('lagerID','filid')]
  # count_unique<-matrix[,list(count_unique=length(unique(kolvo_F))),by=c('lagerID','filid')]
  # matrix<-left_join(matrix,no_zero_count,by=c('lagerID','filid'))
  # z<-matrix[is.na(matrix$no_zero_count),]
  # z$no_zero_count<-1
  # z[is.na(z$no_zero_count)]$no_zero_count<-9999
  
  # matrix_mod_calculation<-matrix[matrix$good_observ>min.obs & matrix$min_kolvo!=matrix$max_kolvo,]
  # matrix_no_mod_calculation<-matrix[(matrix$good_observ<=min.obs & matrix$good_observ>2) | (matrix$good_observ>2 & matrix$min_kolvo==matrix$max_kolvo),]
  # small<-matrix[matrix$good_observ<=2,]
  
  matrix_mod_calculation<-matrix[matrix$good_observ>min.obs & matrix$count_unique>3,]
  matrix_no_mod_calculation<-matrix[(matrix$good_observ<=min.obs & matrix$good_observ>2) | (matrix$good_observ>2 & matrix$count_unique<4),]
  small<-matrix[matrix$good_observ<=2,]
  
  
    ##FEATURE GENERATION
  #calendar
  matrix_mod_calculation$DAY<-as.factor(wday(matrix_mod_calculation$date))
  matrix_mod_calculation$MONTH<-as.factor(month(matrix_mod_calculation$date))
  matrix_mod_calculation$WEEK<-week(matrix_mod_calculation$date)
  matrix_mod_calculation$YEAR<-year(matrix_mod_calculation$date)
  matrix_mod_calculation$day_month<-day(matrix_mod_calculation$date)
  matrix_mod_calculation<-matrix_mod_calculation[order(date),TREND:=1:length(date),by=c("lagerID","filid")]
  # Holidays
  matrix_mod_calculation$NY<-ifelse(matrix_mod_calculation$day_month %in% c(25:31) & matrix_mod_calculation$MONTH==12,1,0)
  matrix_mod_calculation$Easter<-ifelse(matrix_mod_calculation$date %in%  append(
    append(
      append(
        append(
          append(
            append(
              append(append(seq(as.Date('2012-04-09'),as.Date('2012-04-15'),by="day"),
                            seq(as.Date('2013-04-30'),as.Date('2013-05-05'),by="day")),
                     seq(as.Date('2014-04-14'),as.Date('2014-04-20'),by="day")),
              seq(as.Date('2015-04-06'),as.Date('2015-04-12'),by="day")),
            seq(as.Date('2016-04-25'),as.Date('2016-05-01'),by="day")),
          seq(as.Date('2017-04-10'),as.Date('2017-04-16'),by="day")),
        seq(as.Date('2018-04-02'),as.Date('2018-04-08'),by="day")),
      seq(as.Date('2019-04-22'),as.Date('2019-04-28'),by="day")),
    seq(as.Date('2020-04-13'),as.Date('2020-04-19'),by="day")),1,0)
  matrix_mod_calculation$StNick<-ifelse(matrix_mod_calculation$day_month %in% c(13:19) & matrix_mod_calculation$MONTH==12,1,0)
  matrix_mod_calculation$StValentine<-ifelse(matrix_mod_calculation$day_month %in% c(8:14) & matrix_mod_calculation$MONTH==2,1,0)
  matrix_mod_calculation$WomenDay<-ifelse(matrix_mod_calculation$day_month %in% c(2:8) & matrix_mod_calculation$MONTH==3,1,0)
  matrix_mod_calculation$Christmas<-ifelse(matrix_mod_calculation$day_month %in% c(1:7) & matrix_mod_calculation$MONTH==1,1,0)
  
  # level correction
  levels(matrix_mod_calculation$MONTH)<-c(levels(matrix_mod_calculation$MONTH),setdiff(c(1:12),levels(matrix_mod_calculation$MONTH)))
  levels(matrix_mod_calculation$DAY)<-c(levels(matrix_mod_calculation$DAY),setdiff(c(1:7),levels(matrix_mod_calculation$DAY)))
  
  ##OUT-OF-STOCK detection (AVG calculation)
  
  # noMP_avgW<-matrix[ kolvoStore>0 & isMP==0,list(avgW=mean(kolvo_F,na.rm=T),avgWgrn=mean(sumOut_F)),by=c("lagerID","filid","YEAR","MONTH","WEEK")]
  # noMP_avgM<-matrix[ kolvoStore>0 & isMP==0,list(avgM=mean(kolvo_F,na.rm=T),avgMgrn=mean(sumOut_F)),by=c("lagerID","filid","YEAR","MONTH")]
  # matrix<-left_join(matrix,noMP_avgW,by=c("lagerID","filid","YEAR","MONTH","WEEK"))
  # matrix<-left_join(matrix,noMP_avgM,by=c("lagerID","filid","YEAR","MONTH"))
  
  #################### add action potential sales !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # matrix$AVG<-ifelse(matrix$isMP==0,
  #                    ifelse(is.na(matrix$kolvo_F),
  #                           ifelse(is.na(matrix$avgW),matrix$avgM,matrix$avgW),
  #                           ifelse(is.na(matrix$avgW),
  #                                  ifelse(matrix$kolvo_F<matrix$avgM & matrix$kolvoStore<matrix$avgM,matrix$avgM,matrix$kolvo_F),
  #                                  ifelse(matrix$kolvo_F<matrix$avgW & matrix$kolvoStore<matrix$avgW,matrix$avgW,matrix$kolvo_F))),
  #                    matrix$kolvo_F) ########add potential from DWHSALESREMAINS !!!!!!!!!!!!!!! 
  # 
  # matrix$AVGgrn<-ifelse(matrix$isMP==0,
  #                       ifelse(is.na(matrix$kolvo_F),
  #                              ifelse(is.na(matrix$avgW),matrix$avgMgrn,matrix$avgWgrn),
  #                              ifelse(is.na(matrix$avgW),
  #                                     ifelse(matrix$kolvo_F<matrix$avgM & matrix$kolvoStore<matrix$avgM,matrix$avgMgrn,matrix$sumOut_F),
  #                                     ifelse(matrix$kolvo_F<matrix$avgW & matrix$kolvoStore<matrix$avgW,matrix$avgWgrn,matrix$sumOut_F))),
  #                       matrix$sumOut_F) ########add potential from DWHSALESREMAINS !!!!!!!!!!!!!!!
  
  #PRICES
  matrix_mod_calculation$price<-ifelse(is.nan(matrix_mod_calculation$sumOut_F/matrix_mod_calculation$kolvo_F),matrix_mod_calculation$price_log,matrix_mod_calculation$sumOut_F/matrix_mod_calculation$kolvo_F)
  matrix_mod_calculation<-matrix_mod_calculation[,price:=(na.locf(price)),by=c('lagerID','filid')]
 
    #Sales lag feature
  matrix_mod_calculation<-matrix_mod_calculation[order(date),kolvo_F_lag:=lag(kolvo_F,order_by = date,1),by=c("lagerID","filid")]
  matrix_mod_calculation<-matrix_mod_calculation[order(date),out_of_stock_lag:=lag(out_of_stock,order_by = date,1),by=c("lagerID","filid")]
  matrix_mod_calculation<-matrix_mod_calculation[,kolvo_F_lag:=ifelse(is.na(kolvo_F_lag),kolvo_F,kolvo_F_lag)]
  matrix_mod_calculation<-matrix_mod_calculation[,out_of_stock_lag:=ifelse(is.na(out_of_stock_lag),out_of_stock,out_of_stock_lag)]
  matrix_mod_calculation<-matrix_mod_calculation[,kolvo_F_lag:=ifelse(out_of_stock_lag==1,NA,kolvo_F_lag)]
  #matrix_mod_calculation<-matrix_mod_calculation[,kolvo_F_lag:=na.ma(kolvo_F_lag,k=2,weighting = "simple"),by=c("lagerID","filid")]
  matrix_mod_calculation<-matrix_mod_calculation[,kolvo_F_lag:=na.locf(kolvo_F_lag),by=c("lagerID","filid")]
  
  
  ################### Model calculation/not calculation 
  matrix_mod_calculation<-as.data.frame(matrix_mod_calculation)
  
  out_of_stock_1<-matrix_mod_calculation[matrix_mod_calculation$out_of_stock==1 & matrix_mod_calculation$good_observ*0.5>=matrix_mod_calculation$stock_length,]
  out_of_stock_1$outlier<-0
  
  out_of_stock_mod_na<-matrix_mod_calculation[matrix_mod_calculation$out_of_stock==1 & matrix_mod_calculation$good_observ*0.5<matrix_mod_calculation$stock_length,]
  out_of_stock_mod_na<-out_of_stock_mod_na[,c('lagerID','filid','date',"isMP","out_of_stock","kolvoStore","kolvo_F")]
  
  out_of_stock_no_mod_na<-matrix_no_mod_calculation[matrix_no_mod_calculation$out_of_stock==1 & matrix_no_mod_calculation$good_observ*0.5<matrix_no_mod_calculation$stock_length,]
  out_of_stock_no_mod_na<-out_of_stock_no_mod_na[,c('lagerID','filid','date',"isMP","out_of_stock","kolvoStore","kolvo_F")]
  
  out_of_stock_na<-rbind(out_of_stock_mod_na,out_of_stock_no_mod_na)
  
  out_of_stock_na$isMPcor<-out_of_stock_na$isMP
  out_of_stock_na$outlier<-NA
  out_of_stock_na$out_of_stock<-NA
  out_of_stock_na$kolvo_filter<-NA
  out_of_stock_na<-out_of_stock_na[,c("lagerID","filid","date","isMPcor","outlier","out_of_stock","kolvoStore","kolvo_F","kolvo_filter")]
  
  matrix_no_mod_calculation<-matrix_no_mod_calculation[!(matrix_no_mod_calculation$out_of_stock==1 & matrix_no_mod_calculation$good_observ*0.5<matrix_no_mod_calculation$stock_length),]
  
  matrix_mod_calculation<-matrix_mod_calculation[matrix_mod_calculation$out_of_stock==0,]
  features_schedule<-c('TREND','NY','Easter','StNick','StValentine','WomenDay','Christmas','price','kolvo_F_lag')
  matrix_mod_calculation<-matrix_mod_calculation[,append(append(c('lagerID','filid','date','YEAR','DAY','MONTH','isMP','out_of_stock','kolvoStore','kolvo_F'),features_schedule),names(matrix_mod_calculation[,grep("^MP.",names(matrix))]))]
  
  
  end_data_prepear<-Sys.time()
  # matrix_mod_calculation<-matrix_mod_calculation[!is.na(matrix_mod_calculation$kolvo_F),]#matrix_mod_calculation[is.na(matrix_mod_calculation)]<-0
  start_lasso_modeling<-Sys.time()
  # lasso model
  lasso_model<-dlply(matrix_mod_calculation, c("lagerID","filid"), .progress='time', function(x) {
    sales_matrix<-as.matrix(model.matrix(data=x[,-which(names(x) %in% c('lagerID','filid','date','isMP','out_of_stock','kolvoStore','YEAR'))],kolvo_F~.))
    #sales_matrix<-sales_matrix[,colSums(sales_matrix != 0) > 0]
    lasso<-cv.glmnet(sales_matrix,x$kolvo_F,alpha=1,nfolds = nfolds.cv.glmnet)
  })

  end_lasso_modeling<-Sys.time()
  
  # lasso coefficient
  lasso_coefficient<-data.frame(ldply(lasso_model,function(mod){
    cvm<-mod[["cvm"]]
    cvm_improve<-round((1-lag(cvm)/cvm)*100,2)
    cvm_improve[1]<-cvm_improve[2]
    s<-which.max(cvm_improve>-1)[1]
    lambda<-mod[["lambda"]][[s]]
    rownames_to_column(as.data.frame(as.matrix(coef.cv.glmnet(mod,s=lambda))),"Feature")}))
   #CV lambda
  lambda<-data.frame(ldply(lasso_model,function(mod){
    cvm<-mod[["cvm"]]
    cvm_improve<-round((1-lag(cvm)/cvm)*100,2)
    cvm_improve[1]<-cvm_improve[2]
    s<-which.max(cvm_improve>-1)[1]
    lambda<-mod[["lambda"]][[s]]}))
  colnames(lambda)[3]<-"lambda"
  
  # significant features
  significant_features<-lasso_coefficient[lasso_coefficient$X1!=0,]
  significant_features$Feature<-ifelse(significant_features$Feature=="X.Intercept.","1",(significant_features$Feature))
 
  end_feature_extract<-Sys.time() 
  # lm model
  lm_model<- dlply(matrix_mod_calculation, c("lagerID","filid"), .progress='time',  function(x) {
    sales_matrix<-as.data.frame(model.matrix(data=x[,-which(names(x) %in% c("date","documentId"))],kolvo_F~.-1))
    sales_matrix<-sales_matrix[,colSums(sales_matrix != 0) > 0]
    xt<-cbind(kolvo_F=x$kolvo_F, sales_matrix)
    m<-lm(paste("kolvo_F",paste(significant_features[significant_features$lagerID==unique(x$lagerID) & significant_features$filid==unique(x$filid),]$Feature,collapse="+"),sep ="~"),data=xt)
  })
  # R<-data.frame(ldply(lm_model,function(mod){summary(mod)$r.squared})) 
  end_lm_modelling<-Sys.time()
  # outlier detection
  outliers<-ldply(lm_model, .progress='time',  function(a) {cook<-as.data.frame(transform(cooks.distance(a)))})
  names(outliers)<-c('lagerID','filid','cook')
  
  matrix_mod_calculation<-cbind(matrix_mod_calculation,cook=outliers$cook)
  matrix_mod_calculation<-as.data.table(matrix_mod_calculation)
  matrix_mod_calculation<-matrix_mod_calculation[,cook:=na.mean(cook),by=c('lagerID','filid')] # if choosen binary factor have only one observation from the data set then cook distanse for this observation = NaN (leave one out cross validation)
  
  matrix_mod_calculation<-matrix_mod_calculation[,mean_cook:=mean(cook),by=c('lagerID','filid')]
  matrix_mod_calculation<-matrix_mod_calculation[,sd_cook:=sd(cook),by=c('lagerID','filid')]
  matrix_mod_calculation$outlier<-ifelse(matrix_mod_calculation$cook>matrix_mod_calculation$mean_cook+3*matrix_mod_calculation$sd_cook & matrix_mod_calculation$isMP==0, 1,0)
  
  #outliers<-matrix_mod_calculation[matrix_mod_calculation$outlier==1,c("lagerID","filid","date")]
  end_outlier_detection<-Sys.time()
  # significant_MP
  significant_MP<-significant_features[grep("MP",significant_features$Feature),c('lagerID','filid','Feature')]
  significant_MP$Feature<-as.numeric(substring(significant_MP$Feature,3))
  
  # MP_cor
  if (dim(significant_MP)[1]==0){
    MP0_cor<-NULL
  } else{
  MP0_cor<-inner_join(MP_long[,c('date','mainLagerId','filialId','documentId','marketProgramId','isMP')],significant_MP,by=c('mainLagerId'='lagerID','filialId'='filid','marketProgramId'='Feature'))
  MP0_cor$marketProgramId<-paste('MP',MP0_cor$marketProgramId,sep="")
  MP0_cor<-MP0_cor %>%
    spread(marketProgramId, documentId, fill=0)
  MP0_cor[,5:dim(MP0_cor)[2]][MP0_cor[,5:dim(MP0_cor)[2]]>1]<-1
  MP0_cor[,5:dim(MP0_cor)[2]]<-NULL
  colnames(MP0_cor)[4]<-"isMPcor"
  }
  # matrix_no_mod_calculation. Outlier detection and forecast.
 
  start_small<-Sys.time()
  
  small$isMPcor<-small$isMP
  small$outlier<-NA
  small$out_of_stock<-NA
  small$kolvo_filter<-NA
  small<-small[,c("lagerID","filid","date","isMPcor","outlier","out_of_stock","kolvoStore","kolvo_F","kolvo_filter")]
  if (dim(matrix_no_mod_calculation)[1]==0){result_no_mod=NULL} else{
    matrix_no_mod_calculation<-as.data.table(matrix_no_mod_calculation)
    #matrix_no_mod_calculation<-matrix_no_mod_calculation[isMP==0,outlier:=ifelse(1.5*(quantile(kolvo_F,0.75)-quantile(kolvo_F,0.25))+quantile(kolvo_F,0.75)<kolvo_F,1,0),by=c('lagerID','filid')]
    matrix_no_mod_calculation<-matrix_no_mod_calculation[isMP==0,outlier:=ifelse(mean(kolvo_F)+3*sd(kolvo_F)<kolvo_F,1,0),by=c('lagerID','filid')]
    matrix_no_mod_calculation<-matrix_no_mod_calculation[,outlier:=ifelse(is.na(outlier),0,outlier)]
    matrix_no_mod_calculation<-matrix_no_mod_calculation[,netSales:=ifelse(isMP==1 | out_of_stock==1 | outlier==1,NA,kolvo_F)]
    matrix_no_mod_calculation<-matrix_no_mod_calculation[,forecast:=ifelse(is.na(mean(netSales,na.rm=T)),0,na_ma(netSales,k=2,weighting = "simple")),by=c("lagerID","filid")]
    matrix_no_mod_calculation$isMPcor<-matrix_no_mod_calculation$isMP
    matrix_no_mod_calculation<-matrix_no_mod_calculation[,kolvo_filter:=if (isMPcor==1 | outlier==1 | out_of_stock==1){forecast}else{kolvo_F},by=c("lagerID","filid","date")]
    result_no_mod<-matrix_no_mod_calculation[,c("lagerID","filid","date","isMPcor","outlier","out_of_stock","kolvoStore","kolvo_F","kolvo_filter")]
  }
  
  result_no_mod<-rbind(result_no_mod,small,out_of_stock_na)
  
  end_small_data<-Sys.time()
  # Data correction for forecast
  
  matrix_mod_calculation<-as.data.frame(matrix_mod_calculation)
  sales_cor_matrix<-rbind(matrix_mod_calculation[,names(matrix_mod_calculation) %in% names(out_of_stock_1)],out_of_stock_1[,names(out_of_stock_1) %in% names(matrix_mod_calculation)])

  if (is.null(MP0_cor)){
    sales_cor_matrix$isMPcor<-0} else {
      sales_cor_matrix<-left_join(sales_cor_matrix,MP0_cor,by=c("lagerID"="mainLagerId","filid"="filialId","date"="date"))
      sales_cor_matrix$isMPcor[is.na(sales_cor_matrix$isMPcor)]<-0    
    }

  
  sales_cor_matrix$price_origin<-sales_cor_matrix$price
  sales_cor_matrix<-as.data.table(sales_cor_matrix)
  
  PRICE_COR_MONTH<-sales_cor_matrix[isMPcor==0,list(price_cor_month=mean(price)),by=c('filid','lagerID','MONTH','YEAR')]
  PRICE_COR_ALL<-sales_cor_matrix[isMPcor==0,list(price_cor_all=mean(price)),by=c('filid','lagerID')]
  
  sales_cor_matrix<-left_join(sales_cor_matrix,PRICE_COR_MONTH[,c('lagerID','filid','MONTH','YEAR','price_cor_month')],by=c('lagerID','filid','MONTH','YEAR'))
  sales_cor_matrix<-left_join(sales_cor_matrix,PRICE_COR_ALL[,c('lagerID','filid','price_cor_all')],by=c('lagerID','filid'))
  sales_cor_matrix<-as.data.table(sales_cor_matrix)
  sales_cor_matrix<-sales_cor_matrix[,price_cor:=ifelse(is.na(price_cor_month),price_cor_all,price_cor_month)]
  sales_cor_matrix<-sales_cor_matrix[,price:=ifelse(isMPcor==0,price,price_cor)]
  
  sales_cor_matrix<-sales_cor_matrix[,isMPcor_lag:=lag(isMPcor,order_by = date),by=c('lagerID','filid')]
  sales_cor_matrix[is.na(sales_cor_matrix$isMPcor_lag)]$isMPcor_lag<-0
  sales_cor_matrix$kolvo_F_lag_origin<-sales_cor_matrix$kolvo_F
  
  sales_cor_matrix<-sales_cor_matrix[,noMP_Sales:=ifelse(isMPcor==1 | out_of_stock==1 | outlier==1,NA,kolvo_F)]
  sales_cor_matrix<-sales_cor_matrix[order(date),MA:=na.ma(noMP_Sales,k=3,weighting = "simple"),by=c("lagerID","filid")]
  sales_cor_matrix<-sales_cor_matrix[order(date),kolvo_F_lag_forecast:=lag(MA,n=1,order_by = date),by=c("lagerID","filid")]
  sales_cor_matrix<-sales_cor_matrix[,kolvo_F_lag:=ifelse(isMPcor==1 & isMPcor_lag==1,kolvo_F_lag_forecast,kolvo_F_lag)]
  sales_cor_matrix<-as.data.frame(sales_cor_matrix)
  
  end_data_prepear_forecast<-Sys.time()
  
  if (forecast_method=="lm"){
    sales_cor_matrix<-as.data.table(ddply(sales_cor_matrix, c("lagerID","filid"),.progress='time', function(x){
      sales_matrix<-as.matrix(model.matrix(data=x[,append(append(c('DAY','MONTH','kolvo_F'),features_schedule),names(x[,grep("^MP.",names(x))]))],kolvo_F~.))
      sales_matrix[,grep("^MP.",names(sales_matrix))]<-0
      transform(x, forecast=round(predict(lm_model[[paste(unique(x$lagerID) ,unique(x$filid),sep=".")]] , newdata=as.data.frame(sales_matrix)),2))}))
     } else {
    if (forecast_method=="lasso"){
      sales_cor_matrix<-as.data.table(ddply(sales_cor_matrix, c("lagerID","filid"),.progress='time', function(x){
        sales_matrix<-as.matrix(model.matrix(data=x[,append(append(c('DAY','MONTH','kolvo_F'),features_schedule),names(x[,grep("^MP.",names(x))]))],kolvo_F~.))
        #sales_matrix<-sales_matrix[,colSums(sales_matrix != 0) > 0]
        sales_matrix[,grep("^MP.",names(sales_matrix))]<-0
        transform(x, forecast_lasso=round(predict(lasso_model[[paste(unique(x$lagerID),unique(x$filid),sep=".")]],s=lambda[lambda$lagerID==unique(x$lagerID) & lambda$filid==unique(x$filid),]$lambda,newx=as.matrix(sales_matrix)),2))}))
    } else {
      error='need forecast method'
    }
     }
       names(sales_cor_matrix)[dim(sales_cor_matrix)[2]]<-'forecast'
       sales_cor_matrix$forecast<-ifelse(sales_cor_matrix$forecast<0,0,sales_cor_matrix$forecast)
       sales_cor_matrix$out_of_stock<-ifelse(sales_cor_matrix$kolvo_F<sales_cor_matrix$forecast & sales_cor_matrix$kolvoStore<sales_cor_matrix$forecast & sales_cor_matrix$isMPcor==0,1,0)
       sales_cor_matrix$kolvo_filter<-ifelse(sales_cor_matrix$isMPcor==1 | sales_cor_matrix$outlier==1 | sales_cor_matrix$out_of_stock==1,sales_cor_matrix$forecast,sales_cor_matrix$kolvo_F)
       result_mod<-sales_cor_matrix[,c("lagerID","filid","date","isMPcor","outlier","out_of_stock","kolvoStore","kolvo_F","kolvo_filter")]
    
  #end_forecast<-Sys.time()
  result<-rbind(result_mod,result_no_mod)
  

  return(result)
}



# end_time<-Sys.time()
# paste('data prepear for',(end_data_prepear-start_time))
# paste('lasso modelind',(end_lasso_modeling-end_data_prepear))
# paste('feature extractions',(end_feature_extract-end_lasso_modeling))
# paste('lm_modeling',(end_lm_modelling-end_feature_extract))
# paste('outliers_detection', (end_outlier_detection-end_feature_extract))
# paste('small data',(end_small_data-start_small))
# paste('preparation to forecast', (end_data_prepear_forecast-end_small_data))
# paste('forecast',(end_forecast-end_data_prepear_forecast))
# paste('all time',(end_time-start_time))
# 
# 
# end_data_prepear_forecast

getwd()
saveRDS(filter,'banana_filtered_sales')

r<-as.data.table(result)
r<-r[list(a=length(kolvo_F)),by=c('lagerID','filid')] 
  r<-result[result$date>='2019-04-15',]
  
  
  start_time<-Sys.time()
  filter<-sales_filter(task = task,forecast_method = "lasso")
  end_time<-Sys.time()
  end_time-start_time
  
                                                              # visualization 
  filter<-result
  filter$sales_no_actions<-ifelse(filter$isMPcor==1,filter$kolvo_filter,filter$kolvo_F)
  filter$sales_no_outlier<-ifelse(filter$outlier==1,filter$kolvo_filter,filter$kolvo_F)
  filter$sales_no_out_of_stock<-ifelse(filter$out_of_stock==1,filter$kolvo_filter,filter$kolvo_F)
  filter$clear_sales<-ifelse(filter$out_of_stock==1 | filter$outlier==1 | filter$isMPcor==1,filter$kolvo_filter,filter$kolvo_F)
  
  
  library(plotly)
  
  z<-filter[filter$filid==2042,]
  q<-ggplot(z, aes(date)) + 
   # geom_line(aes(y = AVG, colour = "AVG")) + 
    geom_line(aes(y = kolvo_F, colour = "kolvo_F")) + 
    geom_line(aes(y = sales_no_actions, colour = "sales_no_actions"))+
    geom_line(aes(y = sales_no_outlier, colour = "sales_no_outlier"))+
    geom_line(aes(y = sales_no_out_of_stock, colour = "sales_no_out_of_stock"))+
    geom_line(aes(y = clear_sales, colour = "clear_sales"))+
    geom_line(aes(y = kolvoStore, colour = "kolvoStore"))+
#    geom_point(aes(y=isMPcor, colour = "isMPcor"))+
#    geom_point(aes(y=outlier, colour = "outlier"))+
    scale_x_date(date_breaks = "months",date_labels = "%b-%y") 
  ggplotly(q)
#  summasry(lm_model[["32485.2042"]])

  
 

 