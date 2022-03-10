
## hw2

library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyverse)
library(glue)
library(dplyr)
library(lubridate)
options(digits=4)

## reset the working directory
setwd("/Users/medhaniesolomon/programming/R/hw2")

## create a folder for outputs
output_dir <- "outputs/"

create_output_if_doesnt_exist<- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
    print(paste(folder,"folder created."))}
  else{
    print(paste(folder,"folder already exists"))}
}
create_output_if_doesnt_exist(output_dir)


## retrieve the price data from yahoo.finance
tickers = c("AAPL","GOOG","IBM","MSFT","TSLA")
stockprice = pdfetch_YAHOO(tickers,fields="adjclose",from="2018-01-01", to="2020-12-31", interval= "1wk")
print(head(stockprice))

## calculate log returns
returns = na.omit(diff(log(stockprice)))
print(head(returns))
dims = dim(returns)
dims
#returns$date = as.Date(row.names(as.data.frame(returns)))
returns = as.data.frame(returns) 
returns = returns %>%
  mutate(date=as.Date(row.names(returns)))

## You can download the 3-factor data from Dr.French website and use the read function to import the data 
## Here is the program how to pull the factor data from the website directly

# create the object to store the web path
temp = tempfile()
base = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor = "F-F_Research_Data_Factors_weekly"
format =  "_TXT.zip"
full_url =  glue(base, factor,  format,  sep ="")

# pass full_url to download.file().

download.file(full_url,temp,quiet = TRUE)

# Finally, we can read the txt file using read_table() after unzipping that data with the unz() function.

ff_3factors =  read.table(unz(temp,"F-F_Research_Data_Factors_weekly.txt"),
                          skip = 4, header=TRUE)
head(ff_3factors) 

ff_3factors <- ff_3factors %>%
  mutate(datex = rownames(ff_3factors)) %>%
  mutate(date = ymd(parse_date_time(datex,"%y%m%d"))+days(3))


ff_all <- 
  returns %>% 
  left_join(ff_3factors, by = "date")

ff_all <-na.omit(ff_all)

ff_all <- ff_all %>%
  mutate(SMB=as.numeric(as.character(SMB)),
         HML=as.numeric(as.character(HML)),
         RF=as.numeric(as.character(RF)))
str(ff_all)
dim(ff_all)

## select obs. based on quantile of market excess returns

ff_all_small = ff_all %>%
  select(AAPL,GOOG,IBM,MSFT,TSLA,Mkt.RF,SMB,HML,RF,date) %>%
  filter(Mkt.RF>=quantile(Mkt.RF,probs=0.25) & 
        Mkt.RF<=quantile(Mkt.RF,probs=0.75))
dim(ff_all_small)

## export the data to the computer
write.csv(ff_all_small, paste0(output_dir, "ff3_small.csv"), row.names = F)




