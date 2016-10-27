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

# small functions

# add the month to a data
addMonth <- function(date, n = 1){
  if (n == 0){return(date)}
  if (n %% 1 != 0){stop("Input Error: argument 'n' must be an integer.")}
  
  # Check to make sure we have a standard Date format
  if (class(date) == "character"){date = as.Date(date)}
  
  # Turn the year, month, and day into numbers so we can play with them
  y = as.numeric(substr(as.character(date),1,4))
  m = as.numeric(substr(as.character(date),6,7))
  d = as.numeric(substr(as.character(date),9,10))
  
  # Run through the computation
  i = 0
  # Adding months
  if (n > 0){
    while (i < n){
      m = m + 1
      if (m == 13){
        m = 1
        y = y + 1
      }
      i = i + 1
    }
  }
  # Subtracting months
  else if (n < 0){
    while (i > n){
      m = m - 1
      if (m == 0){
        m = 12
        y = y - 1
      }
      i = i - 1
    }
  }
  
  # If past 28th day in base month, make adjustments for February
  if (d > 28 & m == 2){
    # If it's a leap year, return the 29th day
    if ((y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0){d = 29}
    # Otherwise, return the 28th day
    else{d = 28}
  }
  # If 31st day in base month but only 30 days in end month, return 30th day
  else if (d == 31){if (m %in% c(1, 3, 5, 7, 8, 10, 12) == FALSE){d = 30}}
  
  # Turn year, month, and day into strings and put them together to make a Date
  y = as.character(y)
  
  # If month is single digit, add a leading 0, otherwise leave it alone
  if (m < 10){m = paste('0', as.character(m), sep = '')}
  else{m = as.character(m)}
  
  # If day is single digit, add a leading 0, otherwise leave it alone
  if (d < 10){d = paste('0', as.character(d), sep = '')}
  else{d = as.character(d)}
  
  # Put them together and convert return the result as a Date
  return(as.Date(paste(y,'-',m,'-',d, sep = '')))
}


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

info<-function(data_month,data_quarter,data_year,regress_meta,predict_step){
  
  # find the date for forecasting
  predict_date<-addMonth(regress_meta$end_date[which(regress_meta$predict_type=="predict")],n=predict_step*12/regress_meta$time_type[which(regress_meta$predict_type=="predict")])
  
  names<-names(regress_meta[,1])
  num_row<-nrow(regress_meta)
  predict_num_period<-rep(0,num_row)
  # find the predict information for every x
  for (i in 1:num_row){
     predict_num<-ceiling(difftime(predict_date,regress_meta[i,]$end_date)/(31*12/regress_meta[names[i],]$time_type))
     if (predict_num>0){
       predict_num_period[i]<-predict_num
     }
  }
  regress_meta$predict_num_period<-predict_num_period
  
  # choose the parameters for windows 
  end_window<-as.Date(regress_meta$end_date[which.min(regress_meta$end_date)])
  start_window<-as.Date(regress_meta$start_date[which.max(regress_meta$start_date)])
  
  
  # change all data to time series format
  data_list<-list()
  for(i in 1:num_row){
      type<-regress_meta[i,]$time_type
      if (type==1){data_temp<-change_to_stationary(data_year[names[i]])$stationary_data
      } else if(type==4) {data_temp<-change_to_stationary(data_quarter[names[i]])$stationary_data
      } else {data_temp<-change_to_stationary(data_month[names[i]])$stationary_data}
      
      end_year<-as.numeric(format(as.Date(regress_meta[i,]$end_date), "%y"))
      
      if (type==1){
        end_other=NULL
      }else{
        end_other<-ceiling(as.numeric(format(as.Date(regress_meta[i,]$end_date), "%m"))/(12/type))
        #assign(names[i],ts(data_temp[!is.na(data_temp)],end=c(end_year,end_other), frequency=type))
      }
      data<-ts(data_temp[!is.na(data_temp)],end=c(end_year,end_other), frequency=type)
      data_list[names[i]]<-data.frame(data)
   }
  
  list(start_window=start_window,end_window=end_window,regress_meta=regress_meta,data_list=data_list)
}

info(data_month,data_quarter,data_year,regress_meta,1)
