
library(ical)
library(tidyverse)
library(rlist)
library(glue)
library(pdftools)



#GETTING A DATAFRAME WITH BANK HOLIDAYS FROM ICS EXTENSION FILES AS SOURCE ----------------------------------------------

years <- seq(2015,2019) %>% as.character() #Saving a vector of years for url
url <- "https://calendar.zoznam.sk/icalendar/create-vcard-multiple.php?fName=sk&hy={years}"
urls <- glue(url) #Joining url with vector n times
calendar_names <- glue("Data_Files\\Holidays\\Bank_Holiday_Calendar_{years}.ics") #Saving names of files that will be downloaded
walk2(urls, calendar_names, download.file, mode = "wb") #Downloading files
calendar_list <- map(calendar_names, ical_parse_df) #Parsing files into dfs and saving them in a list


#defining a very specific function that creates a table with the result of ical_parse_df
#to be passed to a map function
clean_table <- function(table){  
  table %>%
    select(-status, -last.modified, -uid, -description) %>% #getting rid of unnecessary columns
    drop_na() %>%
    mutate_at(vars(summary), funs(as.character(summary))) #making sure summary column is string
}

# applying the defined function to create a single dataframe from all ics files
bank_holidays_df <- map_df(calendar_list, clean_table) %>%
  as.data.frame() %>%
  mutate(start= as_date(start), end= as_date(end)) %>% #parsing to necessary date format
  mutate(Easter = str_detect(summary,"pondelok|piatok") %>% as.integer()) #creating Easter feature




#GETTING SCHOOL HOLIDAY CALENDAR DATAFRAME ------------------------------------------------------------------------------

url <- "https://cdn.calendar.zoznam.sk/print/skolsky-kalendar-2016-2017.pdf"


years <- c("2018-2019", "2017-2018", "2016-2017", "2015-2016", "2014-2015") #Saving a vector of years for url
url <- "https://cdn.calendar.zoznam.sk/print/skolsky-kalendar-{years}.pdf" #Saving URL with a variable component "years"
urls <- glue(url) #Joining url with vector n times
pdf_names <- glue("Data_Files\\Holidays\\School_Calendar_{years}.pdf") #Saving the name of the file to be saved with a variable component
walk2(urls, pdf_names, download.file, mode = "wb") #downloading all pdfs from specified url with specified name
raw_text <- map(pdf_names, pdf_text) #converting all in pdfs to string and saving into a list


#defining a very specific function that creates a table with the result of pdf_text
#to be passed to a map function
clean_table <- function(table){  
  table <- str_split(table, "\n", simplify = TRUE) #split every single line and produce a matrix
  gap_index <- str_which(table, "Jarné prázdniny") #because there are "combined cells", identify their index
  table[1,(gap_index-1)] <- gsub("^\\s{3}","    Jarné prázdniny", table[1,(gap_index-1)]) #fill in blank spaces from combined cells
  table[1,(gap_index+1)] <- gsub("^\\s{3}","    Jarné prázdniny", table[1,(gap_index+1)])
  table_start <- str_which(table, "Termíny prázdnin") #Get index of where table starts
  table_end <- ncol(table) #Get index of where table ends
  table <- table[1, (table_start +1 ):(table_end-1)] #define table using saved start/end indexes
  table <- str_replace_all(table, "\\s{3,}", "|") # #replace all occurences of three spaces for delimiter "|"
  table <- str_squish(table) # remove unnecessary characters and spaces
  data_table <- read_delim(table,delim = "|") %>% #read previous result and convert to dataframe
    select(-X1) %>% #remove empty space column
    mutate(Trvanie = str_replace_all(Trvanie, "\\s","")) %>% #remove all spaces in date column
    mutate(Trvanie = str_replace(Trvanie, "[^. |\\d]","-")) #clean all remaining special characters and replace with "-"
}


# applying the defined function to create a single dataframe from all pdfs
school_cal_df <- map_df(raw_text, clean_table) %>%
  as.data.frame() %>% 
  separate(Trvanie, c("Start", "End"), sep = "-") %>% #Splitting date column
  mutate(Start = str_replace (Start, "39", "29")) %>% #Cleaning specific date error
  mutate(Start = str_replace_all (Start,"\\.","-")) %>% #Preparing date format
  mutate(End = str_replace_all (End,"\\.","-")) %>%
  mutate(Start = dmy(Start) %>% as_date, End = dmy(End) %>% as_date) %>%
  mutate(Line = 0, Num_Days = 0) %>% #adding these for later scripts
  filter(!str_detect(Kraj, "^Pre|^Ban")) %>% #Removing rows that have specific holidays at unimportant regions
  select(-Kraj) %>% 
  drop_na() %>% #dropping single day holidays
  arrange(Start)
