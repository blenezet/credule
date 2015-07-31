## ======================================================================
## Step 1: Download all log files
## ======================================================================

# Here's an easy way to get all the URLs in R
start <- as.Date('2015-07-24')
today <- as.Date('2015-07-29')

all_days <- seq(start, today, by = 'day')

year <- as.POSIXlt(all_days)$year + 1900
urls <- paste0('http://cran-logs.rstudio.com/', year, '/', all_days, '.csv.gz')

# only download the files you don't have:
missing_days <- setdiff(as.character(all_days), tools::file_path_sans_ext(dir("CRANlogs"), TRUE))

dir.create("CRANlogs")
for (i in 1:length(missing_days)) {
  print(paste0(i, "/", length(missing_days)))
  download.file(urls[i], paste0('CRANlogs/', missing_days[i], '.csv.gz'))
}

## ======================================================================
## Step 2: Load single data files into one big data.table
## ======================================================================

file_list <- list.files("CRANlogs", full.names=TRUE)

logs <- list()
for (file in file_list) {
  print(paste("Reading", file, "..."))
  logs[[file]] <- read.table(file, header = TRUE, sep = ",", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "", as.is=TRUE)
}

# rbind together all files
library(data.table)
dat <- rbindlist(logs)

# add some keys and define variable types
dat[, date:=as.Date(date)]
dat[, package:=factor(package)]
dat[, country:=factor(country)]
dat[, weekday:=weekdays(date)]
dat[, week:=strftime(as.POSIXlt(date),format="%Y-%W")]

setkey(dat, package, date, week, country)

save(dat, file="CRANlogs/CRANlogs.RData")

# for later analyses: load the saved data.table
# load("CRANlogs/CRANlogs.RData")

## ======================================================================
## Step 3: Analyze it!
## ======================================================================

library(ggplot2)
library(dplyr)
library(plyr)

subdat = dplyr::filter(dat, package == "credule")

View(subdat)

# aggregate by day
by.type <- group_by(subdat,date)
summary.by.date <- dplyr::summarise(by.type,count=n())

dplyr::aggregate(Count ~ date, data = subdat, FUN=sum)



