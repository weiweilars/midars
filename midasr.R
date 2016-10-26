# midars

#install.packages("xlsx")

#library("xlsx")
#library("readxl")

# quarterly data: 
# YEAR, MONTH, DAY, the name of DATA (column name)
import_data_quarter<-data.matrix(read_excel("//home/weiwei/Documents/Uppsala/midars/data.xlsx",1))

# monthly data:
# YEAR, MONTH, DAY, the name of DATA (column name)
import_data_month<-data.matrix(read_excel("//home/weiwei/Documents/Uppsala/midars/data.xlsx",2))

# modify quater data:
date_quarter<-as.POSIXct(paste(import_data_quarter[,"YEAR"],import_data_quarter[,"MONTH"],import_data_quarter[,"DAY"], sep = "-"))
data_quarter<-data.frame(DATE=date_quarter,import_data_quarter[,-1:-3])

# modify month data:
date_month<-as.POSIXct(paste(import_data_month[,"YEAR"],import_data_month[,"MONTH"],import_data_month[,"DAY"], sep = "-"))
data_month<-data.frame(DATE=date_month,import_data_month[,-1:-3])


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
  
  # the columns and rows of the data
  c_num<-ncol(raw_data)
  r_num<-nrow(raw_data)
  col_names<-colnames(raw_data)

  # intialized the meta_info
  meta_info = data.frame(matrix("NA", ncol = c_num-1, nrow = 2)) 
  rownames(meta_info)=c("start_date","end_date")
  colnames(meta_info)=col_names[-1]
  
  # Get the type of the data 
  # month, quarter, year, day
  # right now we only check those four data type, not down to hours
  day_diff=as.numeric(difftime(raw_data[2,1],raw_data[1,1]))
  month_diff=ceiling(day_diff/31)
  
  if (day_diff<2) {data_type=0
  } else if (month_diff<2) {data_type=12
  } else if (month_diff<4) {data_type=4
  } else {data_type=1}
  
  
  # find the start date and end date of the data
  for (i in 2:c_num){
    start_index<-min(which(!is.na(raw_data[,i])))
    end_index<-max(which(!is.na(raw_data[,i])))
    meta_info[col_names[i]]<-c(raw_data[start_index,1],raw_data[end_index,1])
  }
  
  list(type=data_type,meta_info=as.data.frame(t(meta_info)))
}

####################################################################################
# Example of the result : get_meta_info(data_quarter)
# $type
# [1] 4                     0:daily data, 1:month data, 4:quarter data, 12:year data

# $meta_info
#      start_date   end_date
# BNP  1995-02-01 2011-11-01
# KPI  1995-02-01 2012-05-01
# KPIF 1995-02-01 2012-05-01
# HIKP 1996-02-01 2012-05-01
####################################################################################

# ge the meta information about the month data and quarter data
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
  
  data<-as.matrix(single_ts_data)
  # check the p_value of the stationary test, null hypothesis is non-stationary
  
  p_value<-adf.test(data[!is.na(single_ts_data)])$p.value
  
  # differentiate the data to stationary 
  i<-0
  while(p_value>0.05){
    data<-diff(data)
    p_value<-adf.test(data[!is.na(data)])$p.value
    i<-i+1
  }
  
  list(diff_order=i, stationary_data=data.frame(data))
}


# random select some data for analyse (choose some data)
regress_meta<-rbind(quarter_meta$meta_info[1,],month_meta$meta_info[1:5,])
time_type<-c(4,rep(12,5))
predict_type<-c("predict",rep("explain",5))                     
regress_meta<-cbind(regress_meta,time_type,predict_type)

# prepare data for the midas analyse and forecasting

midas_analyse<-function(data_month,data_quarter,regress_meta){
  
  
  quarter_names<-rownames(regress_meta)[regress_meta$type==4]
  month_names<-rownames(regress_meta)[regress_meta$type==12]
  
  # find the predict interval for X
  predict_date<-regress_meta$end_date[which(regress_meta$predict_type=="predict")]
  
  # choose the parameters for every window
  end_month<-regress_meta$end_date[which.min(regress_meta$end_date)]
  start<-regress_meta$start_date[which.max(regress_meta$start_date)]
  
  
  for (i in 1:length(quarter_names)){
    data_temp<-change_to_stationary(data_quarter[quarter_names[i]])$stationary_data
    end_year=as.numeric(format(as.Date(regress_meta[quarter_names[i],]$end_date), "%y"))
    end_quarter=ceiling(as.numeric(format(as.Date(regress_meta[quarter_names[i],]$end_date), "%m"))/3)
    assign(quarter_names[i],ts(data_temp,end=c(end_year,end_quarter), frequency=regress_meta[quarter_names[i],]$type))
  }
  
  for (i in 1:length(month_names)){
    data_temp<-change_to_stationary(data_month[month_names[i]])$stationary_data
    end_year=as.numeric(format(as.Date(regress_meta[month_names[i],]$end_date), "%y"))
    end_month=as.numeric(format(as.Date(regress_meta[month_names[i],]$end_date), "%m"))
    assign(month_names[i],ts(data_temp,end=c(end_year,end_month), frequency=regress_meta[month_names[i],]$type))
  }
  
  
}


