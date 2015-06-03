# initialize libraries
library(googlesheets)
library(ggplot2)
library(dplyr)


# download data from Google Sheet
survey <- gs_key("1fpPQSD1hV15Rh5SiyyBhwePIBK0hIGWzvVyWGwvMiiM")
data <- survey %>% 
    gs_read(ws = "Form Responses 1")


# clean and reorder variables
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


# display data

## Q02: My R proficiency level
### table
round(prop.table(table(data$Q02)) * 100, 1)
### plot
ggplot(data = data) + 
    geom_bar(aes(x = Q02)) + 
    labs(list(title = questions[2], x = "", y = "members"))

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
    coord_flip()