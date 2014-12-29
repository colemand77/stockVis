library(quantmod)
library(dplyr)
library(ggvis)
library(tidyr)
library(devtools)


ticka <- "XOM"

start <- as.Date("2000-01-01")
end <- as.Date("2014-12-22")
per <- "days"
window <- 60

#pull the data from the web

px_a <-getSymbols(ticka, src = "yahoo", 
           from = start,
           to = end,
           auto.assign = FALSE)

px_b <- oil[!is.na(oil[,1])]

head(px_a)
head(px_b)
index(px_a)

  #getSymbols(tickb, src = "yahoo", 
   #               from = start,
    #              to = end,
     #             auto.assign = FALSE)

oil<-getSymbols('DCOILWTICO',src='FRED',
           from = start,
           to = end,
           auto.assign = FALSE) 

px_b <- oil[!is.na(oil[,1])]
head(px_b)
names(px_b)<-c("oil.Adjusted")


dummy <- merge(px_b, px_a, join='inner')
head(dummy)
dummy_df<-data.frame(date=as.Date(index(dummy),"%Y-%m-%d"),dummy, row.names = NULL)
head(dummy_df)

dummy_zoo<-as.zoo(as.matrix(dummy_df[,-1]), as.Date(dummy_df[,1]))
head(dummy_zoo)

#next step is to get it in the right periodicity - before join the tables
#can only handle OHLC data (FUCKING WHY!!)
#and join in the same step

px_2<-merge(to.period(px_a,
                      period = per),
            to.period(px_b,
                       period = per,
                      name=c("open","high","low","oil.Adjusted")),  
            join='inner')
names(px_2)
#can then conver to a data frame to extract just the correct columns
clean_px<-  data.frame(date=index(px_2),px_2, row.names = NULL) %>%
  select(date,contains(".Adjusted")) %>%
  mutate(a_delt = (.[,2]-lag(.[,2]))/lag(.[,2])) %>%
  mutate(b_delt = (.[,3]-lag(.[,3]))/lag(.[,3])) %>%
  filter(!is.na(a_delt) & !is.na(b_delt))


clean_zoo<- as.zoo(as.matrix(clean_px[,-1]), as.Date(clean_px[,1]))
clean_zoo
rm(rollcoeff)
rollcoeff <-  rollapply(clean_zoo, width=window,
            FUN = function(z) {
              as.numeric(coef(lm(a_delt ~ b_delt, data = as.data.frame(z))))},
            by.column = FALSE,
            fill = NA,
            align="right",
            partial = FALSE) %>%
  data.frame(date = index(.),coredata(.))

#this works!!!
#px %>%
#  select(date,contains(".Adjusted")) %>%
#  mutate_each(funs( . / lag(.) -1),-date)


glimpse(rollcoeff)


rollcoeff %>%
  ggvis(~date,~X2) %>%
  layer_lines(stroke:="blue")
