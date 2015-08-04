  
  library("Rcell")
  library("lubridate")
  library("dplyr")
  library("doParallel")
  library("ggplot2")
  library("reshape2")
  library("TTR")
  library("pwr")
  library(xtable)
  
  registerDoParallel(cores=2)
  
  #Read all Files of keywords and merge into one dataframe
  file_list <- list.files("./csv")
  for (file in file_list){
    path = paste(getwd(),"/csv/",file,sep = "")
    searchDate <- ymd(substr(file, 19,26)) #parse the filename to get the date
    
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      #These files are in a UNIX format that required me to read them this way
      filetext <- readLines(con <- file(path, encoding = "UCS-2LE"), n =752); close(con)
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
  abdata$CTR <- as.numeric(gsub("%", "", abdata$CTR))
  abdata <- abdata[complete.cases(abdata),]
  
  totals <- dataset[complete.cases(dataset),]
  totals <- group_by(totals, Keyword)
  totals <- summarize(totals, Clicks = sum(Clicks), CPC = mean(CPC), CTR=mean(CTR))
  totals <- totals[-1,]
  
  top50 <- arrange(totals, desc(Clicks)) 
  top50 <- top20[2:50,]
  #Reorder the factor so it plots correctly
  top50 <- arrange(top50, Clicks)
  top50$Keyword <- revFactor(factor(top50$Keyword, levels = top50$Keyword))

ggplot(data=top50, aes(x=Keyword, y=Clicks/1000, fill=CPC)) +
  geom_bar(stat="identity") +
  ggtitle("Top 10 Revenue 2Decrease\nFeb-May") +
  ylab("(Thousands of GBP)") + xlab(NULL) +
  scale_y_sqrt("Clicks") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 12))


  



#date_summary_netflix$CPC[c(19:32, 85:87)] <- NA
p2 <- ggplot(online.df, aes(x=searchDate, y=CTR)) + 
  geom_line(size=1.1) + ggtitle("Average CTR per Day") +
  ylab(NULL) + xlab(NULL) + geom_line(col="blue") +
  geom_line(data=stock.df, size=1, col="purple",aes(x=searchDate, y=stock.df$CTR)) +
stat_smooth(span=0.20)

multiplot(p1, p2, cols=2)


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
 
