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
  meta_info<-matrix(0,nrow=7,ncol=c_num-4)
  meta_name<-rep(0,c_num-4)
  
  # check the data information about the name, start date, and end date, property of the data
  for (i in 5:c_num){
    # using the numeric value of date to find the start date and end date of data
    
    dateRaw<-raw_data[,1]*!is.na(raw_data[,i])
    start_index<-min(which(!dateRaw==0))
    end_index<-max(which(!dateRaw==0))
    meta_info[1,i-4]<-raw_data[start_index,2]
    meta_info[2,i-4]<-raw_data[start_index,3]
    meta_info[3,i-4]<-raw_data[start_index,4]
    meta_info[4,i-4]<-raw_data[end_index,2]
    meta_info[5,i-4]<-raw_data[end_index,3]
    meta_info[6,i-4]<-raw_data[end_index,4]
    
    count_year<-meta_info[4,i-4]-meta_info[1,i-4]
    average_data_per_year<-sum(!is.na(raw_data[,i]))/count_year
    
    if (average_data_per_year<2) {
      meta_info[7,i-4]<-12
    } else if (average_data_per_year<6){ 
      meta_info[7,i-4]<-3
    } else {
      meta_info[7,i-4]<-1
    }
  }
  
  colnames(meta_info)<-colnames(raw_data)[5:c_num]
  rownames(meta_info)<-c("start_year","start_month","start_day","end_year","end_month","end_day","type")
  return(meta_info)
}




