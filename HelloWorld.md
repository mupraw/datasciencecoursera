## This is a markdown file

#----------------------------------- Activate required libraries ------------------------------

library(ggplot2)
library(data.table)

#----------------------------------- Open data -------------------------------------------------

path <- "C:/Users/mupraw/Documents/"  #Location of the file
pattern <- "splWeekly"                # part of file name

list <- list.files(path, pattern = pattern, full.names = T)   # list of files which consist pattern

#Import data from csv to readable R format. CSV separator is semi colon (";"), header is included, numeric format is prefered rather than factor.

df <- fread(list[j], sep =",", 
                    header = TRUE,
                    stringsAsFactors = F,
                    colClasses = c("numeric"),
                    data.table = F)
                    
summary(df) # check the data quality, whether NA is exist or no

#-------------------------------------- Time conversion -------------------------------------------

# Time metric available is second, convert 'seconds' column to following time format "YYYY-mm-dd H:M:S".
# Measurement started at 08:00

init <- "2018-08-01 08:00:00"
init <- as.POSIXct(init, tz = "EEST")

df[,1] <- df[,1] + init


#-------------------------------------- Plot SPL vs Time ---------------------------------------------

# Daily comparison of SPL should be visible in the figure. Even dealing with relatively small number of columns, automatic line grouping when plotting is preffered 
# rather than do it manually.    

day <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

dfNew <- c()
for(i in 1: length(day)){
x <- df[,c(1,i+1)]
x <- data.frame(spl = x,day = day[i])
dfNew <- rbind(dfNew,x)
}

names <- c("Time",day)
columnnames(dfNew) <- names

ggplot(dfNew,aes(x = Time, y = spl, color = day)) +
geom_line()+
scale_x_datetime( labels = "%Y-%m-%d %H:%M:%S",
                  breaks = "30 mins")








