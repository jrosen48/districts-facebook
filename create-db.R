library(tidyverse)
library(DBI)
# library(RMySQL)
library(RSQLite)

con <- DBI::dbConnect(odbc::odbc(),
                      driver = "PostgreSQL Driver",
                      UID = "joshuarosenberg",
                      PWD = "Bdqx1KeeADYkMJ5Zhj38",
                      host = "k12institutionsonfb.clpifghpngbx.us-east-2.rds.amazonaws.com",
                      port = 5432)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "PostgreSQL Driver",
                      Server   = "k12institutionsonfb.clpifghpngbx.us-east-2.rds.amazonaws.com",
                      UID = "joshuarosenberg",
                      PWD = "Bdqx1KeeADYkMJ5Zhj38",
                      Port     = 5432)

cn <- dbConnect(RPostgres::Postgres(), 
                dbname = "postgres",
                user = "joshuarosenberg",
                password = "Bdqx1KeeADYkMJ5Zhj38",
                host     = "k12institutionsonfb.clpifghpngbx.us-east-2.rds.amazonaws.com",
                port     = 5432)

dbCreateTable(cn, "posts")

dbSendQuery(cn,"create database posts")

dbListTables(cn)

# cn <- dbConnect(drv      = RMySQL::MySQL(),
#                 username = "admin",
#                 password = "cbk2SbxpsotZ1r5tMFzz",
#                 host     = "database-1.clpifghpngbx.us-east-2.rds.amazonaws.com",
#                 port     = 3306,
#                 db       = "k12institutionsfb")
# 

cn <- dbConnect(RSQLite::SQLite(), dbname = here::here("db", "k12-institutions-fb-posts-19-20.sqlite"))
dbListTables(cn)
dbRemoveTable(cn, "posts")

sample_d <- read_csv("data/all-k12-institutions/2005-2012/2020-10-16-16-16-39-EDT-Historical-Report-all-k12-institutions-2012-01-01--2012-06-01.csv",
                     col_types = cols(
                       .default = col_character(),
                       Message = col_character(),
                       created = col_character(),
                       `Facebook Id` = col_double(),
                       Likes = col_double(),
                       Comments = col_double(),
                       Shares = col_double(),
                       Love = col_double(),
                       Wow = col_double(),
                       Haha = col_double(),
                       Sad = col_double(),
                       Angry = col_double(),
                       Care = col_double(),
                       `Post Views` = col_double(),
                       `Total Views` = col_double(),
                       `Total Views For All Crossposts` = col_double(),
                       `Overperforming Score` = col_double())
)

dbCreateTable(cn, "posts", sample_d)

prep_data <- function(d) {
  d %>% 
    janitor::clean_names() %>% 
    mutate(created = lubridate::ymd_hms(created),
           hour = lubridate::hour(created),
           day = lubridate::yday(created),
           year = lubridate::year(created),
           month = lubridate::month(created),
           day_of_week = lubridate::wday(created),
           day_of_month = lubridate::mday(created),
           created_rounded_to_day = lubridate::round_date(created, "day"),
           overperforming_score = if_else(is.na(overperforming_score), -1094, overperforming_score),
           likes_at_posting = ifelse(likes_at_posting == "N/A", NA, likes_at_posting))
}

# sample_d <- prep_data(sample_d) %>% 
#   sample_n(1000)
# 
# dbRemoveTable(cn, "posts")

dbSendQuery(cn, 
            "CREATE TABLE `posts` (
  `page_name` TEXT,
  `user_name` TEXT,
  `facebook_id` REAL,
  `likes_at_posting` INTEGER,
  `created` REAL,
  `type` TEXT,
  `likes` REAL,
  `comments` REAL,
  `shares` REAL,
  `love` REAL,
  `wow` REAL,
  `haha` REAL,
  `sad` REAL,
  `angry` REAL,
  `care` REAL,
  `video_share_status` TEXT,
  `post_views` REAL,
  `total_views` REAL,
  `total_views_for_all_crossposts` REAL,
  `video_length` TEXT,
  `url` TEXT,
  `message` TEXT,
  `link` TEXT,
  `final_link` TEXT,
  `image_text` TEXT,
  `link_text` TEXT,
  `description` TEXT,
  `sponsor_id` TEXT,
  `sponsor_name` TEXT,
  `overperforming_score` REAL,
  `hour` INTEGER,
  `day` REAL,
  `year` REAL,
  `month` REAL,
  `day_of_week` REAL,
  `day_of_month` INTEGER,
  `created_rounded_to_day` REAL
  );
")

#dbCreateTable(cn, "post", sample_d)
db_create_indexes(cn, "posts", indexes = list("url", "hour", "day", "year", "month", "day_of_week", "day_of_month", "created_rounded_to_day", "facebook_id"))
#
# dbSendQuery(cn, 
#             "CREATE TABLE `stage` (
#   `page_name` TEXT,
#   `user_name` TEXT,
#   `facebook_id` REAL,
#   `likes_at_posting` INTEGER,
#   `created` REAL,
#   `type` TEXT,
#   `likes` REAL,
#   `comments` REAL,
#   `shares` REAL,
#   `love` REAL,
#   `wow` REAL,
#   `haha` REAL,
#   `sad` REAL,
#   `angry` REAL,
#   `care` REAL,
#   `video_share_status` TEXT,
#   `post_views` REAL,
#   `total_views` REAL,
#   `total_views_for_all_crossposts` REAL,
#   `video_length` TEXT,
#   `url` TEXT,
#   `message` TEXT,
#   `link` TEXT,
#   `final_link` TEXT,
#   `image_text` TEXT,
#   `link_text` TEXT,
#   `description` TEXT,
#   `sponsor_id` TEXT,
#   `sponsor_name` TEXT,
#   `overperforming_score` REAL,
#   `hour` INTEGER,
#   `day` REAL,
#   `year` REAL,
#   `month` REAL,
#   `day_of_week` REAL,
#   `day_of_month` INTEGER,
#   `created_rounded_to_day` REAL
#   );
# ")
# 
# #dbCreateTable(cn, "post", sample_d)
# db_create_indexes(cn, "stage", indexes = list("url", "hour", "day", "year", "month", "day_of_week", "day_of_month", "created_rounded_to_day", "facebook_id"))

dbListTables(cn)

# dbRemoveTable(cn, "stage")

# index on final table 
# dbSendQuery(cn,
#             "create unique index identity_check on posts (url);")
# 
# insert_and_ignore_duplicates <- function(con, d) {
#   # wipe the stage clean
#   dbSendQuery(con,
#               "delete from stage;")
#   
#   # insert data to stage
#   dbAppendTable(con, "stage", d)
#   
#   # flip the stage over into final
# 
#   dbSendQuery(con,
#               "insert or ignore into posts select * from stage;")
#   
# }

# dbRemoveTable(cn, "time")
#tbl(cn, "fbposts")
#dbCreateTable(cn, "post", sample_d)
#db_create_indexes(cn, "post", indexes = list("facebook_id", "page_name", "hour", "day", "year", "month", "day_of_week", "day_of_month", "created_rounded_to_day"))
# page <- select(sample_d, page_name, user_name, facebook_id)
# post <- select(sample_d, likes_at_posting, type:overperforming_score)
# time <- select(sample_d, created, created_rounded_to_day, year, hour, month, day, day_of_week, day_of_month)
# dbCreateTable(cn, "post", post)
# dbCreateTable(cn, "page", page)
# dbCreateTable(cn, "time", time)

#db_create_indexes(cn, "page", indexes = list("facebook_id"))
#db_create_indexes(cn, "post", indexes = list("facebook_id"))

# tbl(cn, "posts") %>% tally()

#

add_file_to_db <- function(f, cn) {
  
  d <- read_csv(f,
                col_types = cols(
                  .default = col_character(),
                  Created = col_character(),
                  Message = col_character(),
                  `Facebook Id` = col_double(),
                  Likes = col_double(),
                  Comments = col_double(),
                  Shares = col_double(),
                  Love = col_double(),
                  Wow = col_double(),
                  Haha = col_double(),
                  Sad = col_double(),
                  Angry = col_double(),
                  Care = col_double(),
                  `Post Views` = col_double(),
                  `Total Views` = col_double(),
                  `Total Views For All Crossposts` = col_double(),
                  `Overperforming Score` = col_double())
  )
  
  prepped_data <- prep_data(d)
  
  dbWriteTable(cn, "posts", prepped_data, overwrite = FALSE, append = TRUE)
  
}

files <- list.files(here::here("data", "all-k12-institutions", "2020"), 
                    full.names = TRUE,
                    pattern = "\\.csv$", 
                    recursive = TRUE)

map(files, add_file_to_db, cn = cn)
