################################################################################
##  Data Science and Marketing Analtics WS2022/23 - Dr. Keyvan Dehmamy        ## 
##  Susanne Reiter, Johannes Link, André Meißner, Sebastian Pabst 02/12/2023  ##
################################################################################

# General Preparation
rm(list = ls())
options(scipen=999)

# These packages must be installed in advance:
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("car")
install.packages("dplyr")
install.packages("psych")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("tidyr")
install.packages("dplyr")
install.packages("readxl")
install.packages("gender")
install.packages("tidyverse")
install_genderdata_package()
install.packages("mice")
install.packages("syuzhet")
install.packages("caret")
install.packages("kernlab")
install.packages("doParallel")
install.packages("smotefamily")
install.packages("mlbench")

# Then load the following libraries:
library(corrplot)
library(RColorBrewer)
library(car)
library(dplyr)
library(psych)
library(ggplot2)
library(stargazer)
library(tidyr)
library(dplyr)
library(readxl)
library(gender)
library(tidyverse)
library(genderdata)
library(mice)
library(syuzhet)
library(caret)
library(kernlab)
library(doParallel)
library(smotefamily)
library(mlbench)

#### Code Part 1: Data Preparation #############################################

# Read in the data batches:
  
  y401 <- read.csv(file.choose(),header = TRUE, sep = ";")
  y402 <- read.csv(file.choose())
  y403 <- read_excel(file.choose())
  y404_405 <- read_excel(file.choose())
  ySentiment <- read.csv(file.choose())
  yPhysical <- read.csv(file.choose())


# Create Key to join data batches:

  y401$key <- paste(y401$business_id,y401$date_tip)
  y402$key <- paste(y402$business_id,y402$date_tip)
  y403$key <- paste(y403$business_id, y403$date_tip)
  y404_405$key <- paste(y404_405$business_id, y404_405$date_tip)
  ySentiment$key <- paste(ySentiment$business_id, ySentiment_$date)
  yPhysical$key <- paste(yPhysical$business_id, yPhysical$date_tip)
  

# Join into single df:

  y_df1 <- y401 %>% inner_join(y402, by = 'key')
  y_df2 <- y_df1 %>% inner_join(y403, by = 'key')
  y_df3 <- y_df2 %>% inner_join(y404_405, by = 'key')
  y_df4 <- y_df3 %>% inner_join(ySentiment, by = 'key')
  y_df5 <- y_df4 %>% inner_join(yPhysical, by = 'key')


# Delete duplicate columns:
  
  y_df6<- y_df5 %>% select(- business_id.x,- business_id.y,- business_id.x.x, - business_id.y.y,
                           - ch_in.x,- ch_in.y,- ch_in.x.x,- ch_in.y.y,- date_tip.x,- date_tip.y,
                           - date_tip.x.x,- date_tip.y.y, - Source.Name)


# Select a random subset:

  set.seed(12345)
  yelp_data <- y_df6[sample(1:nrow(y_df6), 40000), ]  # 40,000 was chosen to get a clean 75% - 25% split later on


# Create the gender variable from cum_u_names:

  name_func <- function(raw_gender) {
    vector_gender <- c()
    vector_names <- c((unlist(strsplit(raw_gender, ","))))
    for (i in 1:length(vector_names)) {
      vector_gender <- append(vector_gender, gender(trimws(vector_names[i]), years = 2012, method = "ssa")[4])
    }
    vector_gender <- (c(unlist(vector_gender)))
    a <- table(vector_gender)
    if (dim(table(vector_gender))[1] == 0) {
      return ("neutral")
    } else if (dim(table(vector_gender))[1] == 1) {
      return (names(table(vector_gender))[1])
    } else {
      
      female <- a[names(a)=="female"]
      male <- a[names(a)=="male"]
      
      if (female > male) {
        return ("female")
      } else if (male > female) {
        return ("male")
      } else {
        return ("neutral")
      }}
  }
  
  yelp_data$gender <- lapply(yelp_data$cum_u_names, name_func)
  yelp_data$cum_u_names <- NULL

# Change Data Types:

  yelp_data$date <- as.Date(yelp_data$datetip)
  yelp_data$datetip <- NULL 
  yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
  yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
  yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
  yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="Noch_in")
  
  yelp_data$cum_u_gender <- as.factor(yelp_data$cum_u_gender)
  yelp_data$ch_in_last_tips <- as.factor(yelp_data$ch_in_last_tips)
  yelp_data$lot_park <- as.factor(yelp_data$lot_park)
  yelp_data$bike_park <- as.factor(yelp_data$bike_park)
  yelp_data$street_park <- as.factor(yelp_data$street_park)
  yelp_data$business_open <- as.factor(yelp_data$business_open)
  yelp_data$business_price <- as.factor(yelp_data$business_price)
  yelp_data$touristy <- as.factor(yelp_data$touristy)
  yelp_data$back_music <- as.factor(yelp_data$back_music)
  yelp_data$free_wifi <- as.factor(yelp_data$free_wifi)
  yelp_data$delivery <- as.factor(yelp_data$delivery)
  yelp_data$breakfast <- as.factor(yelp_data$breakfast)
  yelp_data$lunch <- as.factor(yelp_data$lunch)
  yelp_data$dinner <- as.factor(yelp_data$dinner)
  yelp_data$WE <- as.factor(yelp_data$WE)
  yelp_data$Quarter <- as.factor(yelp_data$Quarter)
  yelp_data$ch_in <- as.numeric(yelp_data$ch_in)


# Extract the weather data:
  
  extractweather=function(mindate=min(yelp_data$datetip),maxdate=max(yelp_data$datetip),
                          latrange=range(dataset$business_lat),longrange=range(dataset$business_long),
                          resol=.5,getdata=FALSE,
                          wear=ifelse("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData"%in%list.files(),"available","navailable")){
    wdatacond=wear=="navailable"
    if(getdata | wdatacond){
      require("doParallel")
      
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      
      # read the station names
      stations=read.delim(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"),header = F,quote="",sep="")[,1:3]
      colnames(stations)=c("Station","lat","long")
      stations=stations[strtrim(stations$Station,2)=="US",]
      stations$lat=as.numeric(stations$lat)
      stations$long=as.numeric(stations$long)
      stations=stations[!is.na(stations$lat)|!is.na(stations$long),]
      
      #mindate=min(dataset$date)#"2016-05-01"
      #maxdate=max(dataset$date)#"2016-05-02"
      #latrange=range(dataset$business_lat)
      #longrange=range(dataset$business_long)
      
      latseq=c(seq(latrange[1],latrange[2],by=resol),latrange[2])
      longseq=c(seq(longrange[1],longrange[2],by=resol),longrange[2])
      
      wear=NULL
      k=0
      torunlist=NULL
      for(lat in 1:(length(latseq)-1)){#(length(latseq)-1)
        for(lon in 1:(length(longseq)-1)){
          k=k+1
          torunlist=rbind(torunlist,c(lat,lon))
        }
      }
      wear=foreach(i=1:k,.noexport=ls(),.export=c("latseq","longseq","stations","torunlist","mindate","maxdate"))%dopar%
        {  
          # find the station(s) within the boxes
          lat=torunlist[i,1]
          lon=torunlist[i,2]
          rangelat=c(latseq[lat+1],latseq[lat])
          rangelong=c(longseq[lon],longseq[lon+1])
          indx=(stations$lat>rangelat[2])&(stations$lat<rangelat[1])&(stations$long>rangelong[1])&(stations$long<rangelong[2])
          stations_temp=stations[indx,]
          stations_t=paste(stations_temp$Station,collapse=",")
          temp=paste0("dataset=daily-summaries&dataTypes=PRCP,SNWD,SNOW,TMAX,TMIN,TOBS",
                      "&stations=",stations_t,"&startDate=",mindate,"","&endDate=",maxdate)#,
          #"","&boundingBox=",paste(latseq[lat+1],longseq[lon],latseq[lat],longseq[lon+1],sep=","))##90,-180,-90,180
          valid_url <- TRUE
          a=tryCatch(read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))),error=function(e) {valid_url<<-FALSE})
          toreturn=NULL
          if(valid_url)
            toreturn=list(range=cbind(rangelat,rangelong),data=read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))))
          return(toreturn)
          #print(c(lat,lon,valid_url))
        }
      
      
      stopCluster(cl)
      save(file="weatherPRCPSNWDSNOWTMAXTMINTOBS.RData",list=c("wear"))
    }else{
      if(wear=="available"){
        load("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData")
      }
    }
    return(wear)
  }
  
  
  weather=extractweather('2017-01-01','2018-01-04',
                         c(35.9704208,36.3138122),c(-115.4433139,-115.0796384))
  
  weardailyavg=function(wear){
    # this function converts the extracted weather data into daily level data.
    if("weather_data.RData"%in%list.files()){
      load(file="weather_data.RData")
    }else{
      require("doParallel")
      
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      clusterCall(cl,function(x) {library(dplyr)})
      wear_avg=NULL
      k=0
      wear_avg=foreach(i=1:length(wear),.noexport=ls(),.export=c("wear"),.packages = c("dplyr"))%dopar%
        {
          if(is.null(wear[[i]])){
            temp=NULL
          }else{
            temp=wear[[i]]$data %>%
              group_by(DATE) %>%
              summarize(PRCP=mean(PRCP,na.rm = T),SNOW=mean(SNOW,na.rm = T),SNWD=mean(SNWD,na.rm = T),
                        TMAX=mean(TMAX,na.rm = T),TMIN=mean(TMIN,na.rm = T),TOBS=mean(TOBS,na.rm = T))
            temp=list(range=wear[[i]]$range,data=temp)}
          return(temp)
          
        }
      stopCluster(cl)
      weather=NULL
      k=0
      for(i in 1:length(wear_avg)){
        if(is.null(wear[[i]]))
          next
        k=k+1
        weather[[k]]=wear_avg[[i]]
        weather[[k]]$data$DATE=as.Date(weather[[k]]$data$DATE)
      }
      save(file="weather_data.RData",list=c("weather"))
    }
    return(weather)
  }
  
  weatherdaily=weardailyavg(weather)
  
  summary(weatherdaily[[1]]$data)

# Add weather data to yelp_data:

  # Read the temperature data:
  wear=extractweather(yelp_data,resol=.25)

  # Take the averages across stations for each coordinate:
  weather=weardailyavg(wear)

  # Further:
  dates=sort(unique(yelp_data$date))
  weatherstations=as.data.frame(t(sapply(weather,function(x){colMeans(x$range)})))

  # Adding process:
  if(1){
      stations_by=t(apply(yelp_data[,c("business_lat","business_long")],1,
                          function(x){a=sort((x[1]-weatherstations$rangelat)^2+
                                               (x[2]-weatherstations$rangelong)^2,index.return=T)
                          return(a$ix[1:50])})) # finding the 50 closest stations

    for(i in 1:length(weather)){
      if(nrow(weather[[i]]$data)==0)
        next
      store_weather=weather[[i]]$data
      store_weather$TOBS_1=c(store_weather$TOBS[2:nrow(store_weather)],NA)
      store_weather$TOBS_2=c(store_weather$TOBS[3:nrow(store_weather)],NA,NA)
      store_weather$TOBS_3=c(store_weather$TOBS[4:nrow(store_weather)],NA,NA,NA)
      store_weather$TOBS_4=c(store_weather$TOBS[5:nrow(store_weather)],NA,NA,NA,NA)
      weather[[i]]$data=store_weather
    }
    weatherinf=colnames(store_weather)[-1] # which weather variables are available?
  
    yelp_data_weather=NULL
    for(i in 1:length(weather)){
      k=1 # start with the closest station
      stores_in=stations_by[,k]==i
      if(sum(stores_in)==0)
        next
      store_weather=weather[[i]]$data
      
      temp=yelp_data[stores_in,]
      temp=merge(temp,store_weather,by.x="date",by.y="DATE",all.x=T)
      yelp_data_weather=rbind(yelp_data_weather,temp)
      print(i)
    }
  
    # Add weekends and quarters:
    temp=weekdays(yelp_data_weather$date,abbreviate = T)
    yelp_data_weather$WE=(temp=="Sa"|temp=="So") # Be careful is R language is not english
    
    yelp_data_weather$Quarter=as.factor(quarters(yelp_data_weather$date))
    
    write.csv(yelp_data_weather,file="yelp_data_tip_weather.csv") # Saving
  
  }


# Sentiment Score calculation:
  
  score_data <- get_nrc_sentiment(yelp_data$conc_text)[9:10] # No need to lowercase or remove punctuation (package still recognizes everything)
  yelp_data$neg_words <- score_data$negative
  yelp_data$pos_words <- score_data$positive
    
  sentiment_score_dsma <- function(neg_words, pos_words) {
    if (pos_words > neg_words){
      return (round(pos_words/(pos_words+neg_words), 3))
    } else if (pos_words < neg_words){
      return (round((-(neg_words/(pos_words+neg_words)))+1, 3))
    } else {
      return (round(0.5, 3))
    }
  }
  
  yelp_data$sentiment_score <- mapply(sentiment_score_dsma, yelp_data$neg_words, yelp_data$pos_words)
  yelp_data$neg_words <- NULL
  yelp_data$pos_words <- NULL


# Replace missings by implication if possible:

  yelp_data$cum_max_u_elite[yelp_data$cum_max_u_elite == "NULL"]=0
  yelp_data$conc_text[yelp_data$conc_text == "NULL"]="neutral"
  yelp_data$n_photo[yelp_data$n_photo == "NULL"]= 0


# Treating missing values in weather data:

  yelp_data1 <- subset(yelp_data,select = c(TOBS,TOBS_1,TOBS_2,TOBS_3,TOBS_4))
  windows()
  md.pattern(yelp_data1)
  
  predictorMatrix <- matrix(0,nrow = ncol(yelp_data1), ncol = ncol(yelp_data1)) # Make a matrix of zeros
  colnames(predictorMatrix)=colnames(yelp_data1)
  row.names(predictorMatrix)=colnames(yelp_data1)
  predictorMatrix[c("TOBS"),] <- 1
  diag(predictorMatrix) <- 0 # Diagonal must be zero
  
  #impute data
  yelp_data1_data_imputed <- mice(yelp_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
  #seed should always have the same number
  #prediction matrix shows if the variables will have an impact on each other, if they can explain the other variables
  #should set these numbers to 0 if they will not explain the other variables
  
  #get one of the complete data sets (2nd out of 5)
  yelp_data_complete_data <- complete(yelp_data1_data_imputed,2)
  
  #now I merge with the new information with the rest of the data:
  yelp_data_complete_data=cbind(key=yelp_data$key,yelp_data_complete_data)
  
  yelp_data <- yelp_data%>%
    inner_join(yelp_data_complete_data,by="key")

  
# Treating missing value in business_price:
  
  yelp_data$business_price[yelp_data$business_price == "NULL"]=NA
  yelp_data$business_price <- as.numeric(yelp_data$business_price)
  
  yelp_data1 <- subset(yelp_data,select = c(business_price,dinner,lunch,breakfast,delivery,free_wifi,back_music,touristy,lot_park,street_park,bike_park))
  windows()
  md.pattern(yelp_data1)
  
  predictorMatrix <- matrix(0,nrow = ncol(yelp_data1), ncol = ncol(yelp_data1)) # Make a matrix of zeros
  colnames(predictorMatrix)=colnames(yelp_data1)
  row.names(predictorMatrix)=colnames(yelp_data1)
  predictorMatrix[c("business_price"),] <- 1
  diag(predictorMatrix) <- 0 #diagonal must be zero
  
  #impute data
  yelp_data1_data_imputed <- mice(yelp_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
  #seed should always have the same number
  #prediction matrix shows if the variables will have an impact on each other, if they can explain the other variables
  #should set these numbers to 0 if they will not explain the other variables
  
  #get one of the complete data sets ( 2nd out of 5)
  yelp_data_complete_data <- complete(yelp_data1_data_imputed,2)
  
  #now I merge with the new information with the rest of the data:
  yelp_data_complete_data=cbind(key=yelp_data$key,yelp_data_complete_data)

  yelp_data <- yelp_data%>%
    inner_join(yelp_data_complete_data,by="key")

  
# Remove unwanted columns:
  
  # Sorted by category: key, date, ch_in, user_generated_attr, physical_attr, weather_data
  yelp_data <- subset(yelp_data, select = c(key, date, ch_in, cum_n_tips, cum_max_friends, cum_max_us_elite, cum_max_us_fans, cum_max_us_tip, n_photo, cum_u_gender, sentiment_score, ch_in_last_tips,
                                            bike_park, street_park, lot_park, touristy, back_music, free_wifi, delivery, breakfast, lunch, dinner, business_open, business_price,
                                            PRCP, TMAX, TMIN, TOBS, TOBS_1, WE, Quarter))


# Export data to save csv for future
  #write.csv(yelp_data, "yelp_data_30_01_2023.csv")
  #yelp_data <- read.csv(file.choose())
  #yelp_data$X <- NULL
  # If data types change after import, repeat lines 131-154

  
# Use a subset of 10,000 observations if models don't work with bigger dataset:
  #set.seed(66)
  #yelp_data <- yelp_data[sample(nrow(yelp_data), 10000),]  # Used for smaller sample for KNN and RF

# Short code snippet  for testing:
  #m1=glm(ch_in~cum_n_tips+cum_max_friends+cum_max_u_elite+cum_max_us_fans+cum_max_us_tip+#I((male+1)/(female+1))
  #         +business_price+bike_park+lot_park+street_park+touristy+back_music+free_wifi+delivery+breakfast
  #         +lunch+dinner+business_open+n_photo+PRCP+SNOW+SNWD+TMAX+TMIN+TOBS+TOBS_1+TOBS_2+TOBS_3+TOBS_4
  #         +Quarter+WE, data = yelp_data, family = "binomial")
  #car::vif(m1)
  #summary(m1)

  
#### Code Part 2: Model preparation ############################################

  set.seed(66)
  yelp_data_na=yelp_data
  # List of variables in your model, use different varsin for analysis with only e.g., weather data:
  varsin=c("ch_in_string", "ch_in", "cum_n_tips", "cum_max_friends", "cum_max_us_elite",  "cum_max_us_fans", "cum_max_us_tip", "cum_u_gender", "n_photo", "sentiment_score", "ch_in_last_tips", "bike_park", "street_park", "lot_park", "touristy", "back_music", "free_wifi", "delivery", "breakfast", "lunch", "dinner", "business_open", "business_price", "PRCP", "TMAX", "TMIN", "TOBS", "TOBS_1", "WE", "Quarter")
  #for phys:
  #varsin=c("ch_in_string", "ch_in", "bike_park", "street_park", "lot_park", "touristy", "back_music", "free_wifi", "delivery", "breakfast", "lunch", "dinner", "business_open", "business_price")
  #for char:
  #varsin=c("ch_in_string", "ch_in", "cum_n_tips", "cum_max_friends", "cum_max_us_elite",  "cum_max_us_fans", "cum_max_us_tip", "cum_u_gender")
  #for con:
  #varsin=c("ch_in_string", "ch_in", "n_photo", "sentiment_score", "ch_in_last_tips")
  #for wea:
  #varsin=c("ch_in_string", "ch_in", "PRCP", "TMAX", "TMIN", "TOBS", "TOBS_1")
  
  yelp_data=subset(yelp_data,select=varsin)
  datasetsize=nrow(yelp_data)/1 # would you like to work only  on a subset of your data? 
  x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
  x.train <- x[1:floor(nrow(x)*.75), ]
  x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]
  
  BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
  BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))
  
  # create dummies (required for SMOTE)
  x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
  x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))
  
  # class imbalance check.
  #temp=table(x.train[,"ch_in_string"])
  #print(temp) #noch_in: 19883 ch_in: 10117 
  
  # if yes, maybe you want do random over-sampling:
  #if(0){
  #  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  #  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  #  for(m in 1:(length(temp)-1)){
  #    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
  #    minclassdat=x.train[x.train$ch_in_string==minchclass,]
  #    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
  #    oversampled=rbind(oversampled,minclassdat)
  #  }
  #  x.train=oversampled
  #}
  
  # or do SMOTE:
  #if(1){
  #  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  #  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  #  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  #  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  #  x.traindum=x.traindum_smote
  #  rm(x.traindum_smote)
  #}
  #temp=table(x.traindum[,"ch_in_string"])
  #print(temp)
  
  # normalize data (very important for ML techniques, but not for logistic regression)
  x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
  x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)
  
  # adjust Baseformula to the dummy version of the data
  varsin_dum=varsin[1:2]
  for(i in 3:length(varsin)){
    if(!is.null(levels(x[,varsin[i]]))){
      for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
        varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
      }
    }else{
      varsin_dum=c(varsin_dum,varsin[i])
    }
  }
  
  # redo the releveling:
  x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
  x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
  x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
  x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")
  
  
  BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
  BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))
  
  # set threshold probability: usually .5, but better is to set it to the portion of 1's. 
  probthres=mean(x.traindum$ch_in)
  
  # makeLiftPlot Function, F-Measure Function:
  
  makeLiftPlot <- function(Prediction, Evaluate, ModelName){
    iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
    CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
    SumChurnReal<- sum(Evaluate$ch_in_string == "ch_in") #total number of real churners in the evaluation set
    CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
    ChurnCumulative=apply(matrix(CustomersSorted=="ch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
    ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="ch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
    ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
    TDL = ProbTD / ProbOverall
    GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
    plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of customers (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
    lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
    legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
    text(0.125,0.95,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
    return(data.frame(TDL,GINI))
  }
  
  fmeasure <- function(true_pos, false_pos, false_neg){
    precision <- true_pos/(true_pos + false_pos)
    recall <- true_pos/(true_pos + false_neg)
    return(2*((precision*recall)/(precision+recall)))
  }

#### Code Part 3: Data Analysis ################################################

#### LOGIT ####
  ptm <- proc.time()
  x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"
  
  summary(x.modelLogit)
  
  x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
  x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
  x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"
  
  x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
  LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")
  
  TimeAux <- proc.time() - ptm
  #LogitOutput$summary=summary(x.modelLogit)
  LogitOutput$TimeElapsed <- TimeAux[3]
  LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
  Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
  rm(TimeAux)
  
  # Problems with confmatrix, so we calculated numbers the following way:
  logit_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionlogitclass == "ch_in",])
  logit_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionlogitclass == "ch_in",])
  logit_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionlogitclass == "Noch_in",])
  f_measure_logit <- fmeasure(logit_true_pos, logit_false_pos, logit_false_neg)
  print(f_measure_logit)
  LogitOutput$f_measure <- f_measure_logit
  
  
#### Naive Bayes ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  ptm <- proc.time()
  # tuneGrid for model:
  #set.seed(66)
  #laplace <- rep(sort(sample.int(40, 3)), 3)
  #adjust <- sort(runif(3, 0, 1))
  #adjust <- c(rep(adjust[1], 3), rep(adjust[2],3), rep(adjust[3],3))
  #nb_grid <- data.frame(laplace, usekernel = TRUE, adjust)
  
  x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes")
  
  x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")
  
  x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
  x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"
  
  x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))
  
  # the variable importance
  print(varImp(x.modelNB))
  
  # Extract the class probabilities.
  x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in'] # changed 31.1. Before: x.evaluate$predictionNB <- x.evaluate$predictionNB[,'Noch_in'] This calculated a negative GINI and put high prob. ch_in as noch_in and vice versa
  
  NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")
  
  TimeAux <- proc.time() - ptm 
  NBOutput$TimeElapsed <- TimeAux[3]
  NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
  NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
  rm(TimeAux)
  stopCluster(cl)
  
  nb_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionNBclass == "ch_in",])
  nb_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionNBclass == "ch_in",])
  nb_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionNBclass == "Noch_in",])
  f_measure_nb <- fmeasure(nb_true_pos, nb_false_pos, nb_false_neg)
  print(f_measure_nb)
  NBOutput$f_measure <- f_measure_nb
  

#### KNN ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  ptm <- proc.time()
  # tuneGrid for model:
  #set.seed(66)
  #k <- sort(sample.int(100, 9))
  #knn_grid <- data.frame(k)
  x.modelKNN <- train(BaseFormula_dum, data = x.trainnorm, method="knn")
  
  x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")
  
  x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
  x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"
  
  x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))
  
  # the variable importance
  print(varImp(x.modelKNN))
  
  # Extract the class probabilities.
  x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in'] # changed 31.1. Before: x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'Noch_in'] This calculated a negative GINI and put high prob. ch_in as noch_in and vice versa
  
  KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")
  
  TimeAux <- proc.time() - ptm 
  KNNOutput$TimeElapsed <- TimeAux[3]
  KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
  KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
  rm(TimeAux)
  stopCluster(cl)
  
  knn_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionKNNclass == "ch_in",])
  knn_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionKNNclass == "ch_in",])
  knn_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionKNNclass == "Noch_in",])
  f_measure_knn <- fmeasure(knn_true_pos, knn_false_pos, knn_false_neg)
  print(f_measure_knn)
  KNNOutput$f_measure <- f_measure_knn
  
  
#### SVM ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  ptm <- proc.time()
  # change method to "svmLinear" for tuning
  x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmRadial", cachesize=12000, tolerance=.01,
                      trControl = trainControl(classProbs =  TRUE))
  
  x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")
  
  x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
  x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"
  
  x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))
  
  # for fast trainer you can also get the variable importance
  print(varImp(x.modelSVM))
  
  # Extract the class probabilities.
  x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']
  
  SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")
  
  TimeAux <- proc.time() - ptm 
  SVMOutput$TimeElapsed <- TimeAux[3]
  SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
  SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
  rm(TimeAux)
  stopCluster(cl)
  
  svm_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionSVMclass == "ch_in",])
  svm_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionSVMclass == "ch_in",])
  svm_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionSVMclass == "Noch_in",])
  f_measure_svm <- fmeasure(svm_true_pos, svm_false_pos, svm_false_neg)
  print(f_measure_svm)
  SVMOutput$f_measure <- f_measure_svm
  
  
#### Neural network ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  library(NeuralNetTools) # required for plotting
  # fast trainer using parallel computations
  ptm <- proc.time()
  mlp_grid = expand.grid(layer1 = 5,
                         layer2 = 0,
                         layer3 = 0)

  #change mlp_grid for tuning model:
  #mlp_grid = expand.grid(layer1 = c(5,9),
  #                       layer2 = c(0,5,9),
  #                       layer3 = c(0,5,9))
  
  x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML', tuneGrid=mlp_grid) 
  
  x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")
  
  x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
  x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"
  
  x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))
  
  print(varImp(x.modelNNet))
  # plot NNet
  if(0){
    NeuralNetTools::plotnet(x.modelNNet$finalModel)
  }
  x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]
  
  NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")
  
  TimeAux <- proc.time() - ptm 
  #NNetOutput$summary=varImp(x.modelNNet)
  NNetOutput$TimeElapsed <- TimeAux[3]
  NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
  NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
  rm(TimeAux)
  
  stopCluster(cl)
  
  nnet_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionNNetclass == "ch_in",])
  nnet_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionNNetclass == "ch_in",])
  nnet_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionNNetclass == "Noch_in",])
  f_measure_nnet <- fmeasure(nnet_true_pos, nnet_false_pos, nnet_false_neg)
  print(f_measure_nnet)
  NNetOutput$f_measure <- f_measure_nnet
  
  
#### TREE ####
  # fast model using parallel computation
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  ptm <- proc.time()
  
  # tuneGrid for model:
  #set.seed(66)
  #mincriterion <- sort(runif(9, 0, 1))
  #t_grid <- data.frame(mincriterion)
  x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 
  
  x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")
  
  x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
  x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"
  
  x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))
  
  x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))
  
  x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]
  
  # to see the importance of the variables
  print(varImp(x.modelTree))
  
  # plot tree, if desired 
  if(0){
    plot(x.modelTree$finalModel)
  }
  
  TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")
  
  TimeAux <- proc.time() - ptm 
  #TreeOutput$summary <- varImp(x.modelTree)
  TreeOutput$TimeElapsed <- TimeAux[3]
  TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
  Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
  rm(TimeAux)
  
  stopCluster(cl)
  
  tree_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionTreeClass == "ch_in",])
  tree_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionTreeClass == "ch_in",])
  tree_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionTreeClass == "Noch_in",])
  f_measure_tree <- fmeasure(tree_true_pos, tree_false_pos, tree_false_neg)
  print(f_measure_tree)
  TreeOutput$f_measure <- f_measure_tree
  
  
#### Bagging ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  ptm <- proc.time()
  # fast training using parallel computation
  x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)
  
  # Use the model to predict the evaluation.
  x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")
  
  x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
  x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"
  
  x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))
  
  # Calculate the overall accuracy.
  x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))
  
  # Extract the class probabilities.
  x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]
  
  # to see the importance of the variables
  print(varImp(x.modelBagging))
  
  BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")
  
  TimeAux <- proc.time() - ptm
  #BaggingOutput$summary <- varImp(x.modelBagging)
  BaggingOutput$TimeElapsed <- TimeAux[3]
  BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
  Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
  rm(TimeAux)
  stopCluster(cl)
  
  ba_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionBaggingClass == "ch_in",])
  ba_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionBaggingClass == "ch_in",])
  ba_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionBaggingClass == "Noch_in",])
  f_measure_ba <- fmeasure(ba_true_pos, ba_false_pos, ba_false_neg)
  print(f_measure_ba)
  BaggingOutput$f_measure <- f_measure_ba

#### Boosting ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  ptm <- proc.time()
  # Create a model using boosting ensemble algorithms
  # fast trainer using parallel computation
  x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')
  
  # Use the model to predict the evaluation.
  x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")
  
  x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
  x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"
  
  x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))
  
  # Calculate the overall accuracy.
  x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))
  
  # Extract the class probabilities.
  x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]
  
  # to see the importance of the variables
  print(varImp(x.modelBoosting))
  
  # Make a lift curve
  BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")
  
  TimeAux <- proc.time() - ptm 
  #BoostingOutput$summary <- varImp(x.modelBoosting)
  BoostingOutput$TimeElapsed <- TimeAux[3]
  BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
  Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
  rm(TimeAux)
  
  stopCluster(cl)
  
  bo_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionBoostingClass == "ch_in",])
  bo_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionBoostingClass == "ch_in",])
  bo_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionBoostingClass == "Noch_in",])
  f_measure_bo <- fmeasure(bo_true_pos, bo_false_pos, bo_false_neg)
  print(f_measure_bo)
  BoostingOutput$f_measure <- f_measure_bo
  
  
#### RANDOM FOREST ####
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  ptm <- proc.time()
  
  #set.seed(66)
  #ntrees <- sort(sample.int(500, 3)) # for each of the 3 values, a random forest is calculated
  # Include parameter ntree = value for tuning
  
  # Create a model using "random forest and bagging ensemble algorithms
  # a fast trainer using parallel computation
  x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF") 
  
  # Use the model to predict the evaluation.
  x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")
  
  x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
  x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"
  
  x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))
  
  # Calculate the overall accuracy.
  x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
  print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))
  
  # Extract the class probabilities.
  x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]
  
  # to see the importance of the variables
  print(varImp(x.modelRF))
  
  RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")
  
  TimeAux <- proc.time() - ptm 
  #RFOutput$summary <- varImp(x.modelRF)
  RFOutput$TimeElapsed <- TimeAux[3]
  RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
  RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
  rm(TimeAux)
  stopCluster(cl)
  
  rf_true_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionRFClass == "ch_in",])
  rf_false_pos <- nrow(x.evaluate[x.evaluate$ch_in_string == "Noch_in" & x.evaluate$predictionRFClass == "ch_in",])
  rf_false_neg <- nrow(x.evaluate[x.evaluate$ch_in_string == "ch_in" & x.evaluate$predictionRFClass == "Noch_in",])
  f_measure_rf <- fmeasure(rf_true_pos, rf_false_pos, rf_false_neg)
  print(f_measure_rf)
  RFOutput$f_measure <- f_measure_rf
  

#### Code Part 4: Some Summarizing plots #######################################
  
  OverallTDL <- c(LogitOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
  OverallGINI <- c(LogitOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)
  
  ForGraph <- data.frame(OverallTDL,OverallGINI)
  
  myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
  myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))
  
  myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
  myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)
  
  ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
  ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)
  
  op <- par(mar = c(5,4,4,4) + 0.1)
  
  barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("Logit","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift", legend = c("TDL","GINI"), main="Performance of the Machine Learning Algorithms")
  
  axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)
  
  axis(4, at = myRightAxisAt, labels = myRightAxisLabs)
  
  mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))
  
  mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
          paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
          paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
          paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
          paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
          paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
          paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))
  mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
          paste(round(SVMOutput$PercCorrect,digits=0),"%"),
          paste(round(TreeOutput$PercCorrect,digits=0),"%"),
          paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
          paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
          paste(round(RFOutput$PercCorrect,digits=0),"%"),
          paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))
  
  mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
  mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)
  
  
  lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit,data=x.evaluate,class="ch_in")
  
  ggplot(lift_obj)