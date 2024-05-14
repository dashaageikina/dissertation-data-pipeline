#create a dataset with the days when the US was "busy" with some big news
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')
if (!require(lubridate)) install.packages('lubridate')
if (!require(english)) install.packages('english')
library(tidyverse)
library(readxl)
library(lubridate)
library(english)

rm(list=ls())

path <- "/home/dashaa/wildfire_politics/data"
big_news_world_file <- paste0(path,"/build_big_news/input/built/wikipedia_world_news_timeline.xlsx") 
big_news_US_file <- paste0(path,"/build_big_news/input/built/wikipedia_US_news_timeline.xlsx") 

#append all years together
big_news <- read_excel(big_news_world_file,sheet=1)
big_news$year <- 1999
big_news_US <- read_excel(big_news_US_file,sheet=1)
big_news_US$year <- 1999

for (year in 2000:2014) {
  big_news1 <- read_excel(big_news_world_file,sheet=year-1998)
  big_news1$year <- year
  big_news <- rbind(big_news,big_news1)
  
  big_news_US1 <- read_excel(big_news_US_file,sheet=year-1998)
  big_news_US1$year <- year
  big_news_US <- rbind(big_news_US,big_news_US1)
}
rm(big_news1,big_news_US1)
big_news <- subset(big_news,select=-US_involved)

big_news <- rbind(big_news,big_news_US)

#create the variables for the start and end dates of the events
big_news$dates <- sub("\\:.*", "", big_news$Event)
big_news$dates <- sub(pattern="–",replacement="-",big_news$dates)
split_dates <- strsplit(big_news$dates,"[-]")
big_news$start_date <- sapply(split_dates,"[[",1)

big_news$end_date <- big_news$start_date
for (i in 1:nrow(big_news)) {
  start_date_i <- big_news$start_date[i]
  end_date_i <- split_dates[[i]][2]
  if (!is.na(end_date_i)) {
    if (nchar(end_date_i)<=2) {
      big_news$end_date[i] <- paste0(sub("\\ .*", "", start_date_i)," ",end_date_i)
    } else {
      big_news$end_date[i] <- end_date_i
    }
  }
}

#keep fixing the dates
big_news$start_date <- paste(big_news$start_date,big_news$year,sep=" ")
big_news$start_date <- as.Date(big_news$start_date,"%B %d %Y")
big_news$end_date <- paste(big_news$end_date,big_news$year,sep=" ")
big_news$end_date <- as.Date(big_news$end_date,"%B %d %Y")

#fixing the issue with duplicate news for Gaza War
big_news <- filter(big_news,Event!="January 1-18: Gaza War.")
big_news$end_date[big_news$Event=="December 27-31: Gaza War."] <- as.Date("January 18 2009","%B %d %Y")
#big_news$end_date <- big_news$end_date+2
#big_news$end_date[big_news$Subjective_week_extension==1] <- big_news$end_date[big_news$Subjective_week_extension==1]+5
big_news <- subset(big_news,select=c(Event,start_date,end_date,Keywords))

#add sports too
sports <- read_excel(paste0(path,"/build_big_news/input/built/sports_dates.xlsx"))
#create the variables for the start and end dates of the events
sports <- sports %>%
  mutate(start_date=make_date(year = Year, month = Month, day = Day),
         end_date=start_date+Plus_days,
         Sports=paste0(Sports," ",Year))
         #start_weekday=weekdays(start_date),
         #end_weekday=weekdays(end_date))
sports <- rename(sports, Event = Sports)
sports <- subset(sports,select=c(Event,start_date,end_date,Keywords))

big_news <- rbind(big_news,sports)

#now importing New York Times news
nyt_path <- paste0(path,"/build_NLP/input/raw/NYT_front_page_abstracts")
directories <- list.files(nyt_path,full.names=TRUE)

dfs <- list()

# Loop through each directory and import Excel files
for (dir in directories) {
  files <- list.files(path = dir, full.names = TRUE)
  
  # Import each Excel file in the directory and store in the list
  dfs_in_dir <- lapply(files, read_excel)
  
  #Keep only specific columns in each dataframe
  dfs_in_dir <- lapply(dfs_in_dir, function(df) {
    df <- subset(df,select=c(Title,Abstract,pubdate,year))
    return(df)
  })
  
  # Bind the dataframes in the directory and store in the list
  dfs <- c(dfs, dfs_in_dir)
}

nyt <- bind_rows(dfs)
#adjusting the date format
nyt$pubdate <- as.Date(nyt$pubdate, format = "%b %d, %Y")

#a function for finding all news for one event
find_news_for_event <- function(event_title) {

  date1 <- big_news$start_date[big_news$Event==event_title]-10
  date2 <- big_news$end_date[big_news$Event==event_title]+10
  
  keywords <- big_news$Keywords[big_news$Event==event_title]
  keywords <- unlist(strsplit(keywords, ","))
  
  #first filtering the news based on dates
  nyt_subset <- filter(nyt,pubdate>=date1 & pubdate<=date2)
  final_nyt_subset <- slice(nyt_subset)
  #then filtering the news based on keywords
  for (keyword in keywords) {
    
    if (grepl("\\+",keyword)==FALSE) {
      nyt_subset_temp <- nyt_subset %>%
        filter(grepl(keyword, Title, ignore.case=T) | 
                 grepl(keyword, Abstract, ignore.case=T))
    } else {
      words <- strsplit(keyword, "\\+")[[1]]
      nyt_subset_temp <- nyt_subset
      
      for (word in words) {
        nyt_subset_temp <- nyt_subset_temp %>%
          filter(grepl(word, Title, ignore.case=T) | 
                   grepl(word, Abstract, ignore.case=T))
        
      }
      
    }
    
    final_nyt_subset <- rbind(final_nyt_subset,nyt_subset_temp)
  }
  
  final_nyt_subset <- distinct(final_nyt_subset)
  final_nyt_subset$Event <- event_title
  
  if (nrow(final_nyt_subset)==0) {
    return(final_nyt_subset)
  }
  
  #now add counting of the days
  summary_table <- final_nyt_subset %>%
    group_by(Event) %>%
    summarize(n_days=n_distinct(pubdate),
              n_news=n(),
              n_days_wei=n_days/(sqrt(1+sum(as.numeric(diff(sort(unique(pubdate))))-1)))) %>%
    ungroup()
  
  final_nyt_subset <- final_nyt_subset %>%
    left_join(summary_table,by="Event") %>%
    group_by(Event) %>%
    mutate(same_day_articles = ifelse(duplicated(pubdate) | duplicated(pubdate, fromLast = TRUE), 1, 0)) %>%
    ungroup()
  
  return(final_nyt_subset)
}


big_events_in_nyt <- lapply(big_news$Event,find_news_for_event)

all_days <- data.frame(day=seq(from = ymd("1999-01-01"), to = ymd("2014-12-31"), by = "days"))
all_days$news1days_wei <- vector("list",nrow(all_days))

for (i in 2:9) {
  col_name <- paste0("news",i, "days")
  all_days[[col_name]] <- vector("list",nrow(all_days))
  
  col_name <- paste0("news",i, "days_wei")
  all_days[[col_name]] <- vector("list",nrow(all_days))
}

all_days <- all_days %>%
  mutate(news10plusdays=vector("list",nrow(all_days)),
         news10plusdays_wei=vector("list",nrow(all_days)),
         sameday_articles=vector("list",nrow(all_days)),
         all_articles=vector("list",nrow(all_days)))

for (i in 1:length(big_events_in_nyt)) {
  
  if (nrow(big_events_in_nyt[[i]])==0) {
    next
  }
  
  event_df <- big_events_in_nyt[[i]] %>%
    distinct(Event,pubdate, .keep_all=TRUE) %>%
    select(Event,pubdate,n_days,n_days_wei,same_day_articles) %>%
    arrange(Event,pubdate)
  
  for (j in 1:nrow(event_df)) {
    
    pubdate_value <- event_df$pubdate[j]
    event_value <- event_df$Event[j]
    index <- which(all_days$day==pubdate_value)
    
    n_days_value <- event_df$n_days[j]
    n_days_wei_value <- event_df$n_days_wei[j]
    same_day_articles_value <- event_df$same_day_articles[j]
    
    all_days$all_articles[[index]] <- c(all_days$all_articles[[index]],event_value)
    
    if (same_day_articles_value>0) {
      all_days$sameday_articles[[index]] <- c(all_days$sameday_articles[[index]],event_value)
    }
    
    if (n_days_value>1 & n_days_value<10) {
      for (days_num in 2:n_days_value) {
        col_name <- paste0("news",days_num, "days")
        all_days[[col_name]][[index]] <- c(all_days[[col_name]][[index]],event_value)
      }
    } else if (n_days_value>=10) {
      for (days_num in 2:9) {
        col_name <- paste0("news",days_num, "days")
        all_days[[col_name]][[index]] <- c(all_days[[col_name]][[index]],event_value)
      }
      all_days$news10plusdays[[index]] <- c(all_days$news10plusdays[[index]],event_value)
    }
    
    if (n_days_wei_value>=1 & n_days_wei_value<10) {
      for (days_num in 1:floor(n_days_wei_value)) {
        col_name <- paste0("news",days_num, "days_wei")
        all_days[[col_name]][[index]] <- c(all_days[[col_name]][[index]],event_value)
      }
    } else if (n_days_wei_value>=10) {
      for (days_num in 1:9) {
        col_name <- paste0("news",days_num, "days_wei")
        all_days[[col_name]][[index]] <- c(all_days[[col_name]][[index]],event_value)
      }
      all_days$news10plusdays_wei[[index]] <- c(all_days$news10plusdays_wei[[index]],event_value)
    }
  }
  
  
}

write_rds(all_days,paste0(path,"/build_big_news/output/big_news_days.rds"))





