library(ggplot2)
library(data.table)

#----------------------------------- Open data -------------------------------------------------

path <- "C:/Users/mupraw/Documents/"  #Location of the file
pattern <- "splWeekly"                #select only file contains particular words

list <- list.files(path, pattern = pattern, full.names = T)   # list of files which consist pattern


df <- read.table(list, sep =";", 
            header = TRUE,
            stringsAsFactors = F)

summary(df) # check the data quality, whether NA is exist or no

df <- df[!is.na(df$second),]   #remove column with NAs

#-------------------------------------- Time conversion -------------------------------------------

# Available time metric  is second, convert 'seconds' column to following time format "YYYY-mm-dd H:M:S".
# Measurement started at 08:00 and end at 12:00

init <- "2018-08-01 08:00:00"
init <- as.POSIXct(init, tz = "Europe/Tallinn")


# -------------------------------------- Time Resolution -----------------------------------------------

filterTime <- function(data,gap){
  min <- c(1,seq(gap,7200,gap)) #Selected data always starts from first second and stops in two hours (7200 seconds). Gap is in second
  
  x <- data[data$second %in% min,]
  
  return(x)
}
  

df <- filterTime(df,300)

df[,1] <- df[,1] + init      #Set time from second to format "YYYY-mm-dd H:M:S"



#-------------------------------------- Plot SPL vs Time ---------------------------------------------

# Daily comparison of SPL should be visible in the figure. Even dealing with relatively small number of columns, automatic line grouping when plotting is preffered 
# rather than do it manually.
# Available data frame should be tailored to accomodate the purpose

day <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

dfNew <- c()
for(i in 1: length(day)){
  x <- data.frame(date = df[,1], spl = df[,i+1], day = day[i])
  dfNew <- rbind(dfNew,x)
}



#Create function for plotting. Users may choose particular days for comparising the SPLs. Minimum selected days is two.

plotSelectDays <- function(data, day1, day2,day3 = NULL,day4= NULL,day5= NULL,day6= NULL,day7= NULL){
  selDays <- c(day1, day2,day3,day4,day5,day6,day7)
  
  data <- data[data$day %in% selDays,]
  
a <-  ggplot(data,aes(x = date, y = spl, color = day)) + theme_bw() +
    geom_line() + theme(legend.position = c(0.9,0.9)) +
  ylab(expression ("SPL  [re. 1 \U000B5"~Pa^2~"]"))+
  xlab("Time")
  

return(a)
}


print(plotSelectDays(dfNew,"Monday","Friday"))

