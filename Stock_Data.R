library(quantmod)
library(tidyverse)
library(xts)
library(zoo)

#Retrieves stock data 
stock <- data.frame(getSymbols(Symbols = "EXR", from = "2006-01-01", auto.assign = FALSE))

#Separate Dates and Re index (much easier to work with)
EXR.Date <- as.Date(rownames(stock), format = "%Y-%m-%d")

#Add back date (in date form)
stock <- add_column(stock, EXR.Date, .before = "EXR.Open")

#Add Daily Percent Return (removes first row for containing NA)
stock$EXR.Per_Rent <- 100*(stock$EXR.Adjusted-lag(stock$EXR.Adjusted))/lag(stock$EXR.Adjusted)
stock <- stock[-1,]
rownames(stock) <- 1:nrow(stock)

#Add text for later sorting
stock$EXR.Rent <- ifelse(stock$EXR.Per_Rent > 0, "Positive", ifelse(stock$EXR.Per_Rent < 0,"Negative","Neutral"))

#Graph Adjusted Close
ggplot(stock, aes(x= EXR.Date, y = EXR.Per_Rent, color = EXR.Rent)) + 
  geom_point( size = 1) +
  ggtitle(paste("Daily Percent Returns from 2006-01-01 to", Sys.Date())) + 
  xlab("Date") + ylab("Percent Return (%)") +
  theme_light() +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  scale_color_manual(values=c("#F45532", "#999999", "#59c53b")) +
  guides(color = guide_legend(title = "Returns", reverse = TRUE))

#Conditional Probability
m <- matrix(rep(0,9), nrow = 3, ncol = 3)
colnames(m) <- c("Positive Today","Neutral Today","Negative Today")
rownames(m) <- c("Positive Tomorrow","Neutral Tomorrow","Negative Tomorrow")

#Check the current value and its next value for Return
for(i in 1:(length(stock$EXR.Rent)-1)){
  
  #First Positive
  if(stock$EXR.Rent[i] == "Positive"){
    if(stock$EXR.Rent[i+1] == "Positive"){
      m[1,1] <- m[1,1]+1
    }
    if(stock$EXR.Rent[i+1] == "Neutral"){
      m[2,1] <- m[2,1]+1
    }
    if(stock$EXR.Rent[i+1] == "Negative"){
      m[3,1] <- m[3,1]+1
    }
  }
  
  #First Neutral
  if(stock$EXR.Rent[i] == "Neutral"){
    if(stock$EXR.Rent[i+1] == "Positive"){
      m[1,2] <- m[1,2]+1
    }
    if(stock$EXR.Rent[i+1] == "Neutral"){
      m[2,2] <- m[2,2]+1
    }
    if(stock$EXR.Rent[i+1] == "Negative"){
      m[3,2] <- m[3,2]+1
    }
  }
  
  #First Negative
  if(stock$EXR.Rent[i] == "Negative"){
    if(stock$EXR.Rent[i+1] == "Positive"){
      m[1,3] <- m[1,3]+1
    }
    if(stock$EXR.Rent[i+1] == "Neutral"){
      m[2,3] <- m[2,3]+1
    }
    if(stock$EXR.Rent[i+1] == "Negative"){
      m[3,3] <- m[3,3]+1
    }
  }
}

#check because one neutral keeps flipping to positive
net <- filter(stock, EXR.Rent == "Neutral")
  
#removes i
rm(i)

prob_matrix <- m/sum(m)
