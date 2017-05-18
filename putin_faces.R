library("rtweet")
library("dplyr")
library("magick")
library("httr")
library("stringr")
library("purrr")
library("gmp")
users <- search_users(q= '#Putin', n = 1000,parse = TRUE)
users <- unique(users)
users <- mutate(users, where = paste0("https://twitter.com/", screen_name))
get_piclink <- function(df){
  content <- httr::GET(df$where)
  content <- httr::content(content, as = "text")
  image <- str_extract(content,
                       "class=\"ProfileAvatar-image \" src=\"https://pbs.twimg.com/profile_images/.*\\..*\" alt")
  image <- str_replace(image, "class=\"ProfileAvatar-image \" src=\"", "")
  image <- str_replace(image, "..alt", "")
  return(image)
}
users <- by_row(users, get_piclink,
                .to = "piclink", .collate = "cols")
readr::write_csv(users, path = "users.csv")
save_image <- function(df){
  image <- try(image_read(df$piclink), silent = TRUE)
  if(class(image)[1] != "try-error"){
    image %>%
      image_scale("50x50") %>%
      image_write(paste0("C:/Users/user/Documents/putin/", df$screen_name,".jpg"))
  }
  
}
users <- filter(users, !is.na(piclink))
users <- split(users, 1:nrow(users))
walk(users, save_image)
files <- dir("C:/Users/user/Documents/putin/", full.names = TRUE)
set.seed(1)
files <- sample(files, length(files))
gmp::factorize(length(files))
## Big Integer ('bigz') object of length 7:
## [1] 2 2 2 2 2 2 7
#Based on this I chose this format:

no_rows <- 28
no_cols <- 16

# This is not useful comand make_column <- function(i, files, no_rows){
#image_read(files[(i*no_rows+1):((i+1)*no_rows)]) %>%
# image_append(stack = TRUE) %>%
#image_write(paste0("C:/Users/user/Documents/2017-03-19-facesofr_users_images/", i, ".jpg"))
#}

make_column <- function(i, files, no_rows){
  image_read(files[(i*no_rows+1):((i+1)*no_rows)]) %>%
    image_append(stack = TRUE) %>%
    image_write(paste0("C:/Users/user/Documents/putin/col/", i, ".jpg"))
}


walk(0:(no_cols-1), make_column, files = files,
     no_rows = no_rows)

image_read(dir("C:/Users/user/Documents/putin/col/", full.names = TRUE)) %>%
  image_append(stack = FALSE) %>%
  image_write("2017-03-22-putinfaces.jpg")