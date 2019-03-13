#Script to plot historical number of e-mails per year
#By Eliette 2019.03.05

#clear environment
rm(list=ls())
dev.off()

#set working directory
setwd("~/Rwork/unloquer") #-example

#download libraries
##to read a URL
library(RCurl)
library(httr)
##to work with date-type data (among others)
library(lubridate)
##ggplot, dyplr...
library(tidyverse)

#source function 'fun-graph_combo!
source("~/Rwork/models_v2/fun-graph_combo.R") #-example

#load raw data
##get URL containing raw data (from GitHub : brolin/ViZaJe)
raw.url <- getURL("https://raw.githubusercontent.com/brolin/ViZaJe/b02a6267a632d2cc15318a0b1c86a4310a112b7d/Data/2019/CorreosConContenido.csv")
##read .csv data file from URL
raw.data <- read.csv(text = raw.url, sep=",", head = FALSE)

#data conditioning
##name columns
colnames(raw.data) <- c("subject", "from", "to", "date", "d1", "d2")
##delete mesagges that do not belong to the list ("Limpia mensajes que no corresponden directamente a mensajes de la lista" by brolin)
raw.data <- raw.data[!str_detect(raw.data$subject,"Re: \\[unloquer/AQA\\].*"),]%>% as_tibble

#-----Frequency of e-mails per year --------------
## extract the years from the date: 
###first, from the raw.data select the variable 'date',then, convert that date to dmy_hm, then extract only the year
years <- raw.data %>% select(date) %>% 
  lapply(dmy_hm) %>% sapply(year) %>% as.data.frame()
#obtain frequency of e-mails per year
###first take the list 'years', then convert it to data frame, then count the number of occurences per year
freq <- years %>% count(date) %>% rename(years=date, n=n)
##plot
a <- ggplot(freq, aes(x = years, y = n, group=1)) +
  geom_point(col = "red", size =2)+
  geom_line()+
  theme_bw()
a <- a + labs(x = "Time", y = "Number of E-Mails")
a <- a + ggtitle("Total E-mail frequency")
a <- a + scale_y_continuous(breaks = seq (0, 2400, by =200), limits = c(0,2400))

#-----Frequency of subjects in one year --------------
##create tibble of subjects per year
subject <- raw.data %>% select(subject) %>% as.data.frame() 
subxyear <- map2_dfr(years, subject, ~ tibble(years = .x, subject = .y))
##filter the peak of e-mails (identified above: 2014)
sub_2014 <- subxyear %>% filter(years ==2014) %>%
  group_by(subject) %>% arrange(years) %>% 
  summarise(year  = paste(years, collapse =","), times = length(years))  %>%
  arrange(desc(times), year)
##plot
###select only top 20 subjects
sub_2014_2 <- sub_2014[1:20, ]
###plot
b <- ggplot(sub_2014_2) +
  geom_bar(stat = "identity")+
  aes(x=reorder(subject, times, sum),y=times, label = times, fill = times)+
  theme_bw()
b <- b + labs(x = "", y = "Number of E-Mails")
b <- b + ggtitle("Top Subjects 2014")
b <- b + scale_y_continuous(breaks = seq (0, 35, by =5), limits = c(0,35))
b <- b + theme(axis.text.x = element_text(size = 6, angle = 90), legend.position = c(0.8,0.3), 
               legend.title = element_blank())
b <- b + coord_flip()

#-----Plot previous graphs together--------------------
png('e-mail_summary2.png', units="cm", width=16, height=20, res=300)
graph_combo(a, b, ygrob ="", xgrob ="", nc = 1)
dev.off()



