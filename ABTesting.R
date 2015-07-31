  
  library("Rcell")
  library("lubridate")
  library("dplyr")
  library(doParallel)
  library("ggplot2")
  library("reshape2")
  library("TTR")
  
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
      dataset <- read.table(textConnection(filetext), header=TRUE, sep="\t", stringsAsFactors = FALSE)
      dataset$searchDate <- searchDate
    } else {
      filetext <- readLines(con <- file(path, encoding = "UCS-2LE"), n =752); close(con)
      temp_dataset <- read.table(textConnection(filetext), header=FALSE, sep="\t", skip = 1, stringsAsFactors = FALSE)
      temp_dataset$searchDate <- searchDate
      names(temp_dataset) <- names(dataset)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }

#Remove the collums we don't need and do other cleanup on the data
dataset <- select(dataset, Keyword,CPC=Average.CPC, Clicks, CTR, Cost, Impressions, searchDate)
abdata  <- filter(dataset, Keyword=="online trading" | Keyword=="stock trading")

filter(dataset, Keyword=="online trading")

dataset <-dataset[as.character(dataset$MonthlySearches) != as.character("-"),] 
dataset$MonthlySearches <- as.numeric(dataset$MonthlySearches)
dataset$Revenue <- as.numeric(gsub(",", "", dataset$Revenue, fixed = TRUE)) 
dataset <- dataset[dataset$SearchDate >= "2012-02-01 UTC",] #Start from February because Jan data is not reliable
dataset <- dataset[dataset$Clicks > 0,] #Remove rows with no clicks
dataset <-dataset[complete.cases(dataset),] #Remove rows with NAs

#save this clean csv for later
write.csv(dataset,"keywords.csv")
#dataset <- read.csv("keywords.csv", stringsAsFactors=FALSE); 
#dataset$SearchDate <- as.Date(dataset$SearchDate)

#Analysis 1 summary by data
by_date <- group_by(dataset, SearchDate)
date_summary <- summarize(by_date, CPC=mean(CPC), Revenue=sum(Revenue), Clicks=sum(Clicks))

Dates <- date_summary$SearchDate
Revenue <- date_summary$Revenue/1000
Clicks <- date_summary$Clicks
CPC <- date_summary$CPC

#Near zero values were distorting the chart
#Chart is easier to read if the data is left out
date_summary$Revenue[c(18:32,85:87)] <- NA

p1 <- ggplot(date_summary, aes(x=SearchDate, y=Revenue/1000)) + 
  geom_line() + ggtitle("Revenue per Day") +
  ylab("(Thousands of GBP)") + xlab(NULL) + ylim(from=225/3, to=225)

date_summary$Clicks[c(19:32, 40:43, 85:87)] <- NA
p2 <- ggplot(date_summary, aes(x=SearchDate, y=Clicks/1000)) + 
  geom_line() + ggtitle("Clicks per Day") +
  ylab(NULL) + xlab(NULL) + ylim(from=40/3, to=40)

date_summary$CPC[c(19:32, 85:87)] <- NA
p3 <- ggplot(date_summary, aes(x=SearchDate, y=CPC)) + 
  geom_line() + ggtitle("Average CPC per Day") +
  ylab(NULL) + xlab(NULL) 

multiplot(p1, p2, p3, cols=3)

#Analys 2, indentify the top increasing and decreasing keywords
#First group by month
by_month <- group_by(dataset, Keyword, Month=months(SearchDate))
month_summary <- summarize(by_month,  Revenue=sum(Revenue))
month_summary$Month <- factor(month_summary$Month, levels = c("February", "March", "April", "May"))
month_summary <- dcast(month_summary, Keyword ~ Month, value.var = "Revenue") #Create one collumn per month
month_summary$Change <- month_summary$May - month_summary$February

top10 <- head(arrange(month_summary, desc(Change)),10)
#Reorder the factor so it plots correctly
top10$Keyword <- revFactor(factor(rev(top10$Keyword), levels = rev(top10$Keyword)))

bottom10 <- head(arrange(month_summary, Change),10)
#Reorder the factor so it plots correctly
bottom10$Keyword <- revFactor(factor(bottom10$Keyword, levels = bottom10$Keyword))

p1 <- ggplot(data=bottom10, aes(x=Keyword, y=Change/1000, fill=Change)) +
  geom_bar(colour="black", stat="identity")  + coord_flip() + 
  theme(legend.position="none") + 
  ggtitle("Top 10 Revenue Decrease\nFeb-May") +
  ylab("(Thousands of GBP)") + xlab(NULL)

p2 <- ggplot(data=top10, aes(x=Keyword, y=Change/1000, fill=Change)) +
  geom_bar(colour="black", stat="identity")  + coord_flip() + 
  theme(legend.position="none") + 
  ggtitle("Top 10 Revenue Increase\nFeb-May") +
  ylab("(Thousands of GBP)") + xlab(NULL)

multiplot(p1, p2, cols=2)


#Analysis 3 netflix only
dataset_netflix <- dataset[dataset$Keyword=="netflix",]
by_date_netflix <- group_by(dataset_netflix, SearchDate)
date_summary_netflix <- summarize(by_date_netflix, CPC=mean(CPC), Revenue=sum(Revenue), Clicks=sum(Clicks))

p1 <- ggplot(date_summary_netflix, aes(x=SearchDate, y=Revenue/1000)) + 
  geom_line() + ggtitle("Revenue per Day") +
  ylab("(Thousands of GBP)") + xlab(NULL)  +
  geom_line(col="navy")

date_summary_netflix$Clicks[c(26:28)] <- NA
p2 <- ggplot(date_summary_netflix, aes(x=SearchDate, y=Clicks/1000)) + 
  geom_line() + ggtitle("Clicks per Day") +
  ylab(NULL) + xlab(NULL) + 
  geom_line(col="navy")

#date_summary_netflix$CPC[c(19:32, 85:87)] <- NA
p3 <- ggplot(date_summary_netflix, aes(x=SearchDate, y=CPC)) + 
  geom_line() + ggtitle("Average CPC per Day") +
  ylab(NULL) + xlab(NULL) + geom_line(col="navy")

multiplot(p1, p2, p3, cols=3)

#4 month charts without Netflix
by_date <- group_by(dataset, SearchDate)
date_summary <- summarize(by_date, CPC=mean(CPC), Revenue=sum(Revenue), Clicks=sum(Clicks))
date_summary <- arrange(date_summary, SearchDate)

Revenue <- round(date_summary$Revenue)
Start <- as.Date(date_summary[1,]$SearchDate)

RevTS <- ts(Revenue)
#Fill in some of the days that had very low sales
Revenue[18:32] <- round(seq(from=211060, to=179251, length.out =15))
Revenue[85:87] <- round(seq(from=124112, to=123375, length.out =3))

Revenue
RevTS <- ts(Revenue)
plot(RevTS)

RevTS.seriesarima <- arima(RevTS, order=c(1,0,0))
RevTS.seriesarima2 <- forecast.Arima(RevTS.seriesarima, h=30)
plot.forecast(RevTS.seriesarima2, main = "Revenue Time Series with June Forcast", xaxt="n") 
axis(side=1, at=c(0,30,60,90,120), labels = c("February", "March", "April", "May", "June")) +
  abline(h=c(0,30,60,90,120), v=c(0,30,60,90,120), col="gray", lty=3)

