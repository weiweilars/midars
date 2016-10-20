# midars

#install.packages("xlsx")

#library("xlsx")
#library("readxl")

# quarterly data: 
data_quarter<-data.matrix(read_excel("//home/weiwei/Documents/Uppsala/data.xlsx",1))

# monthly data:
data_month<-data.matrix(read_excel("//home/weiwei/Documents/Uppsala/data.xlsx",2))

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
      meta_info[9,i-4]<-12
    } else if (average_data_per_year<6){ 
      meta_info[9,i-4]<-4
    } else {
      meta_info[9,i-4]<-1
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
#type            3      1      1      1      1      1   (1>month,3:quarter,12:year)
####################################################################################

month_meta<-get_meta_info(data_month)
quarter_meta<-get_meta_info(data_quarter)

# random select some data for analyse
regress_meta<-cbind(quarter_meta[,1],month_meta[,1:5])
colnames(regress_meta)[1]<-colnames(quarter_meta)[1]


# change the data to ts
change_to_ts<-function(single_data,single_meta){
  if (single_meta["type"]==4){
    order_quarter=ceiling(single_meta["end_month"]/3)
  }
  if (single_meta["type"]==12){
    order_quarter=single_meta["end_month"]
  }
  if (single_meta["type"]==1){
    order_quarter=NULL
  }
  ts_data<-ts(single_data,end = c(single_meta["end_year"],order_quarter),frequency = single_meta["type"])
  return(ts_data)
}

# change the time series data to stationaryd <- d[!is.na(d)]

change_to_stationary<-function(single_ts_data){
  p_value<-adf.test(single_ts_data[!is.na(single_ts_data)])$p.value
  
  while(p_value>0.05){
    single_ts_data<-diff(single_ts_data)
    p_value<-adf.test(single_ts_data[!is.na(single_ts_data)])$p.value
  }
  
  return(single_ts_data)
}



