# midars

#install.packages("xlsx")

#library("xlsx")
#library("readxl")

# quarterly data: 
data_quarter<-data.matrix(read_excel("//home/weiwei/Documents/Uppsala/midars/data.xlsx",1))

# monthly data:
data_month<-data.matrix(read_excel("//home/weiwei/Documents/Uppsala/midars/data.xlsx",2))

# modify quater data:
date_quarter<-as.POSIXct(paste(data_quarter[,"YEAR"],data_quarter[,"MONTH"],data_quarter[,"DAY"], sep = "-"))
data_quarter<-data_quarter[,-1:-3]

# modify month data:
date_month<-as.POSIXct(paste(data_month[,"YEAR"],data_month[,"MONTH"],data_month[,"DAY"], sep = "-"))
data_month<-data_month[,-1:-3]


# meta data: (i)
##################################
# return
# start_year
# start_month
# start_day
# end_year
# end_month
# end_day
# type:  1: monthly data, 3: quarterly data, 12: yearly data
############################################################


get_meta_info<-function(raw_data){
  
  # Column and raw numbers  (The first four columns of raw data are Date,Year,Month,Day)
  c_num<-ncol(raw_data)
  r_num<-nrow(raw_data)
  
  # Initialized return value
  meta_info<-matrix(0,nrow=9,ncol=c_num-4)
  meta_name<-rep(0,c_num-4)
  
  # check the data information about the name, start date, and end date, property of the data
  for (i in 5:c_num){
    # using the numeric value of date to find the start date and end date of data
    
    dateRaw<-raw_data[,1]*!is.na(raw_data[,i])
    start_index<-min(which(!dateRaw==0))
    end_index<-max(which(!dateRaw==0))
    meta_info[1,i-4]<-raw_data[start_index,1]
    meta_info[2,i-4]<-raw_data[start_index,2]
    meta_info[3,i-4]<-raw_data[start_index,3]
    meta_info[4,i-4]<-raw_data[start_index,4]
    meta_info[5,i-4]<-raw_data[end_index,1]
    meta_info[6,i-4]<-raw_data[end_index,2]
    meta_info[7,i-4]<-raw_data[end_index,3]
    meta_info[8,i-4]<-raw_data[end_index,4]
    
    count_year<-meta_info[6,i-4]-meta_info[2,i-4]
    average_data_per_year<-sum(!is.na(raw_data[,i]))/count_year
    
    if (average_data_per_year<2) {
      meta_info[9,i-4]<-1
    } else if (average_data_per_year<6){ 
      meta_info[9,i-4]<-4
    } else {
      meta_info[9,i-4]<-12
    }
    
  }
  colnames(meta_info)<-colnames(raw_data)[5:c_num]
  rownames(meta_info)<-c("start_date","start_year","start_month","start_day","end_date","end_year","end_month","end_day","type")
  return(meta_info)
}

####################################################################################
# get the meta information about data (here is example)
#              BNP kibar1 kibar2 kibar3 kibar4 kibar5
#start_date  34731  36526  35096  35096  35096  36526
#start_year   1995   2000   1996   1996   1996   2000
#start_month     2      1      2      2      2      1
#start_day       1      1      1      1      1      1
#end_date    40848  41000  41000  41000  41000  41000
#end_year     2011   2012   2012   2012   2012   2012
#end_month      11      4      4      4      4      4
#end_day         1      1      1      1      1      1
#type            4      1      1      1      1      1   (1:month,4:quarter,12:year)
####################################################################################

month_meta<-get_meta_info(data_month)
quarter_meta<-get_meta_info(data_quarter)


# change the data to ts
change_to_ts<-function(single_data){
  
  # get the quarter number of the end data
  if (single_meta["type"]==4){
    na_num=ceiling(single_meta["start_month"]/3)
    end=ceiling(single_meta["end_month"]/3)
  }
  if (single_meta["type"]==12){
    na_num=ceiling(single_meta["start_month"])
    end=ceiling(single_meta["end_month"])
  }
  if (single_meta["type"]==1){
    na_num=NULL
    end=NULL
  }
  
  # change to the time series data
  #na_value<-rep(NA,na_num)
  #na_value<-NULL
  ts_data<-ts(single_data[!is.na(single_data)],start = c(single_meta["start_year"],na_num),frequency = single_meta["type"])
  
  
  return(ts_data)
}

# change the time series data to stationary

change_to_stationary<-function(single_ts_data){
  
  # check the p_value of the stationary test, null hypothesis is non-stationary
  
  p_value<-adf.test(single_ts_data[!is.na(single_ts_data)])$p.value
  
  # differentiate the data to stationary 
  i<-0
  while(p_value>0.05){
    single_ts_data<-diff(single_ts_data)
    p_value<-adf.test(single_ts_data[!is.na(single_ts_data)])$p.value
    i<-i+1
  }
  
  list(diff_order=i, stationary_data=single_ts_data)
}


# random select some data for analyse

regress_meta<-cbind(quarter_meta[,1],month_meta[,1:5])
colnames(regress_meta)[1]<-colnames(quarter_meta)[1]

# prepare data for the midas analyse and forecasting

midas_analyse<-function(data_month,data_quarter,regress_meta){
  
  quarter_index<-which(regress_meta["type",]==4)
  month_index<-which(regress_meta["type",]==12)
  
  temp_to_get_end<-regress_meta[,which.min(regress_meta["end_date",])]
  end<-c(temp_to_get_end["end_year"],temp_to_get_end["end_month"])
  
  temp_to_get_start<-regress_meta[,which.max(regress_meta["end_date",])]
  start<-c(temp_to_get_start["start_year"],temp_to_get_start["start_month"])
  
  
  
  
  for (i in 1:length(quarter_index)){
    name<-names(quarter_index[i])
    data_temp<-change_to_stationary(data_quarter[,name])$stationary_data
    assign(name,ts(data_temp,end=c(data_quarter[nrow(data_quarter),"YEAR"],ceiling(data_quarter[nrow(data_quarter),"MONTH"]/3)), frequency=regress_meta[,name]["type"]))
  }
  
  for (i in 1:length(month_index)){
    name<-names(month_index[i])
    data_temp<-change_to_stationary(data_month[,name])$stationary_data
    assign(name,ts(data_temp,end=c(data_month[nrow(data_month),"YEAR"],data_month[nrow(data_month),"MONTH"]), frequency=regress_meta[,name]["type"]))  
  }
 

}


