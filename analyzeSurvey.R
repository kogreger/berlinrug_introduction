# initialize libraries
library(googlesheets)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)


# survey data from Google Sheet
## download data
survey <- gs_key("1fpPQSD1hV15Rh5SiyyBhwePIBK0hIGWzvVyWGwvMiiM")
data <- survey %>% 
    gs_read(ws = "Form Responses 1")
## extract original questions and make pretty
questions <- names(data) %>% 
    gsub("\\.", " ", .) %>% 
    gsub("I ([dm].)", "I'\\1", .) %>% 
    gsub("    ", " ...", .)
## reorder questions
## (Google Forms remembers the order the questions were created...)
qOrder <- c(1, 2, 3, 16, 8, 9, 10, 11, 24, 25, 
            20, 21, 22, 14, 15, 18, 4, 5, 6, 23, 
            7, 12, 13, 17, 19)
data <- data %>% 
    select(qOrder)
questions <- questions[qOrder]
## generate generic question labels Q01..Q25
names(data) <- c(paste0(rep("Q", 25), 
                        sprintf("%02d", seq(1:25))))


# group stats from meetup.com
# (currently the user stats are not downloadable automatically)
# (also, while the file has an .XLS extension it's actually a horribly 
#  formatted .TSV!)
## download and preprocess
stats <- read_tsv("Berlin_R_Users_Group_Total_and_Active_Members.xls", 
                  col_types = "ccc") %>% 
    rename(date = `Date `, 
           totalMembers = `Total Members `, 
           activeMembers = `Active Members`) %>% 
    mutate(date = str_trim(date), 
           date = as.Date(date, format = "%Y-%m-%d"), 
           totalMembers = str_trim(totalMembers), 
           totalMembers = as.integer(totalMembers), 
           activeMembers = as.integer(activeMembers)) %>% 
    filter(!is.na(date))
## extract highest membership data
maxMembers <- data.frame(
    date = stats$date[which.max(stats$totalMembers)], 
    members = c(max(stats$totalMembers))
)
## add milestones
milestones <- data.frame(
    date = as.Date(c("2015-01-05", "2015-05-04", "2015-05-10", "2015-05-29"), 
                   format = "%Y-%m-%d"), 
    event = c("group founded", "leadership changed", "reboot announced\n@BerlinRUG started", "first meetup scheduled"), 
    stringsAsFactors = FALSE
)


# display data

## Q02: My R proficiency level
### table
round(prop.table(table(data$Q02)) * 100, 1)
### plot
ggplot(data = data) + 
    geom_bar(aes(x = Q02)) + 
    labs(list(title = questions[2], x = "", y = "members")) + 
    coord_flip() + 
    theme_bw()

## Q06: Group meetings would be most interesting for me if...
### unpack list of multiple answers and make pretty
q06 <- unlist(strsplit(data$Q06, split=", ")) %>% 
    gsub("\\.", " ", .) %>% 
    gsub("^[[:space:]]+", "", .) %>% 
    paste0("... ", .) %>% 
    table() %>% 
    as.data.frame() %>% 
    tbl_df()
names(q06) <- c("q", "freq")
### table
q06
### plot
ggplot(data = q06) + 
    geom_bar(aes(x = reorder(q, freq), y = freq), 
             stat = "identity") + 
    labs(list(title = questions[6], x = "", y = "mentions")) + 
    coord_flip() + 
    theme_bw()

## member stats
ggplot(data = stats) + 
    geom_line(aes(x = date, y = totalMembers), 
              color = "red") + 
    geom_line(aes(x = date, y = activeMembers), 
              color = "blue") + 
    labs(list(title = "Membership Development", x = "", y = "Members")) + 
    geom_vline(aes(xintercept = as.numeric(date)), 
               data = milestones, 
               linetype = "dashed") + 
    geom_text(aes(x = date, y = 100, 
                  label = event), 
              data = milestones, 
              angle = 90, 
              vjust = 1, 
              hjust = 1, 
              size = 4) + 
    geom_point(aes(x = date, y = members), 
               data = maxMembers, 
               size = 3) + 
    theme_bw()