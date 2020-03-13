#load and install packages
install.packages("rvest")

library(rvest)  
library(tidyverse)
library(magrittr)

# tell cpu where url is. 'url' is just a variable. It can be called anything else
url <- "https://en.wikipedia.org/wiki/List_of_Governors_of_California_by_age"

#scraping wikipedia, finding all the tables on the page, telling it to take/extract the second table, there is no header, create it as a dataframe
govs <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  extract2(2) %>% 
  html_table(header = NA) %>% 
  as.data.frame()

#deleting vectors
govs$`Age at inauguration` <- NULL
govs$`Age.at.endof.term`  <- NULL
govs$`Length.ofretirement` <- NULL
govs$`Lifespan` <- NULL

#renaming ugly vectors - renaming columns 1 and 3
colnames(govs)[1] <- c("num_in_office")
colnames(govs)[3] <- c("rank_by_age")

#using head to delete the last row
govs <- head(govs, 40)

#using regular expression to remove footnotes
govs$`Date of birth` <- gsub("\\[.*","",govs$`Date of birth`)
govs$`End of term` <- gsub("\\[.*","",govs$`End ofterm`)

#check out data
glimpse(govs)

#making the number a number
govs$num_in_office <- as.numeric(govs$num_in_office)

glimpse(govs)

#changing dates to dates
install.packages("lubridate")
library(lubridate)  

#getting to know our new package
mdy(govs$`Date of birth`)
  
#make our dates actual dates
govs$`Date of birth` <- mdy(govs$`Date of birth`)
govs$`Date of inauguration` <- mdy(govs$`Date of inauguration`)
govs$`End ofterm` <- mdy(govs$`End ofterm`)
govs$`Date of death` <- mdy(govs$`Date of death`)
  
glimpse(govs)

#math with dates
govs$`Date of inauguration` - govs$`Date of birth`

#for more obscure charts
install.packages("ggalt")
library(ggalt)

#dumbbell chart
govs %>% 
  ggplot(aes(y = reorder(Governor, num_in_office), x = `Date of inauguration`, xend = `End ofterm`)) +
  geom_dumbbell(color = "firebrick")

#plot out their lifespans
govs$`Date of death` <- case_when(is.na(govs$`Date of death`) == TRUE ~ Sys.Date(), TRUE ~ govs$`Date of death`)
govs %>% 
  ggplot(aes(y = reorder(Governor, num_in_office), x = `Date of birth`, xend = `Date of death`)) +
  geom_dumbbell(color = "darkgreen")

#test out our question for one year, nrow counts number of rowa
govs %>% filter(`Date of birth` < mdy("01-01-1875") & `Date of death` > mdy("01-01-1875")) %>% 
  nrow()

#function
alive_stats <- data.frame()

#loop through our data
for (i in 1806:2020) {
  begin <- mdy(paste("01-01-", i,sep=""))
  end <- mdy(paste("01-01-", (i+1),sep=""))
  
  count_alive <- govs %>% filter(`Date of birth` < begin & `Date of death` > end) %>% 
    nrow() %>% 
    as.numeric()
  
  alive_stats <- rbind(count_alive, alive_stats)
  
  alive_stats$yr[1] <- i
  
}

#renaming columns
colnames(alive_stats)[1] <- c("count")

alive_stats %>% 
  ggplot(aes(x=yr, y=count)) +
  geom_bar(stat="identity") +
  ggtitle("Liveliest Year 4 Govz")








  
