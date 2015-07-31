  
  library("Rcell")
  library("lubridate")
  library("dplyr")
  library("doParallel")
  library("ggplot2")
  library("reshape2")
  library("TTR")
  library("pwr")
  
  registerDoParallel(cores=2)
  
  
  #Read all Files of keywords and merge into one file
  file_list <- list.files("./csv")
  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    path = paste(getwd(),"/csv/",file,sep = "")
    searchDate <- ymd(substr(file, 19,26))
  
      if (!exists("dataset")){
      #This files was in a UNIX format that required me to read it this way
      filetext <- readLines(con <- file(path, encoding = "UCS-2LE", ), n =752); close(con)
      dataset <- read.table(textConnection(filetext), header=TRUE, sep="\t")
      dataset$searchDate <- searchDate
    } else {
      filetext <- readLines(con <- file(path, encoding = "UCS-2LE"), n =752); close(con)
      temp_dataset <- read.table(textConnection(filetext), header=FALSE, sep="\t", skip = 1)
      temp_dataset$searchDate <- searchDate
      names(temp_dataset) <- names(dataset)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }

#Remove the collums we don't need and do other cleanup on the data
dataset <- select(dataset, Keyword,CPC=Average.CPC, Clicks, CTR, Cost, Impressions, searchDate)
abdata  <- filter(dataset, Keyword=="online trading" | Keyword=="stock trading")
abdata <- abdata[complete.cases(abdata),]

#do first test on value for one data
online.yes <-round(abdata[9,]$Clicks)
online.no  <-round(abdata[9,]$Impressions - abdata[1,]$Clicks)
stock.yes  <-round(abdata[10,]$Clicks)
stock.no   <-round(abdata[10,]$Impressions - abdata[1,]$Clicks)

Conversions <- as.table(rbind(c(online.yes, online.no ), c(stock.yes, stock.no)))
dimnames(Conversions) <- list(Keyword = c("stock trading", "online trading"),
                               Response = c("Signup","NoSignup"))
Conversions
chisq.test(Conversions)["p.value"]

abdata[10:11,]

abdata.grouped <- group_by(abdata, Keyword)
summarize(abdata.grouped, CPC=round(mean(CPC),2),Clicks=sum(Clicks), 
          Cost=sum(Cost), Impressions=sum(Impressions))

#do on all values
online.yes <-round(sum(abdata[abdata$Keyword=="online trading",]$Clicks))
online.no  <-round(sum(abdata[abdata$Keyword=="online trading",]$Impressions) - sum(abdata[abdata$Keyword=="online trading",]$Clicks))
stock.yes  <-round(sum(abdata[abdata$Keyword=="stock trading",]$Clicks))
stock.no   <-round(sum(abdata[abdata$Keyword=="stock trading",]$Impressions) - sum(abdata[abdata$Keyword=="online stock",]$Clicks))

Conversions <- as.table(rbind(c(online.yes, online.no ), c(stock.yes, stock.no)))
dimnames(Conversions) <- list(Keyword = c("online trading", "stock trading"),
                              Response = c("Signup","NoSignup"))
Conversions
chisq.test(Conversions)["p.value"]


