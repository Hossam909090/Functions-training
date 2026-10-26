# Hossam Mohamed 

library(tidyverse)

df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
)

rescale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

df |> mutate(
  a = rescale01(a),
  b = rescale01(b),
  c = rescale01(c),
  d = rescale01(d),
)

# Improve the function

rescale <- function(x){
  range <- range(x,na.rm=TRUE)
  (x-range[1])/(range[2]-range[1])
}

df2 <- df %>% 
  mutate(across(a:d,rescale))

##
clamp <- function(x,min,max){
  case_when(
    x < min ~ min,
    x > max ~ max,
    TRUE ~ x
  )
}
  
clamp(1,3,10)


clean_number <- function(x) {
  is_pct <- str_detect(x, "%")
  num <- x |> 
    str_remove_all("%") |> 
    str_remove_all(",") |> 
    str_remove_all(fixed("$")) |> 
    as.numeric()
  if_else(is_pct, num / 100, num)
}

clean_number("45%,$")

##
fix_na <- function(x) {
  if_else(x %in% c(997, 998, 999), NA, x)
}

#Given a vector of birthdates, write a function to compute the age in years.

age <- function(x){
  library(lubridate)
  Age <- ceiling((today() - as.Date(x))/365.25)
  Age
}

x <- c("2024-10-20","2020-10-20","2019-10-20")

age(x)

## Data frame functions
group_mean <- function(df,group_var,mean_var){
  out <- df %>% 
    group_by({{group_var}}) %>% 
    summarize(mean=mean({{mean_var}})) %>% 
    as.data.frame()
  #assign("out", out, envir = .GlobalEnv)
  return(list(df1=out))
}
group_mean(mtcars,cyl,disp)

results <- group_mean(mtcars,cyl,disp)
list2env(results,envir = .GlobalEnv)


my_function <- function() {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  df3 <- data.frame(c = 7:9)
  
  return(list(df1 = df1, df2 = df2, df3 = df3))
}



# Get them into global environment
result <- my_function()
list2env(result, envir = .GlobalEnv)

###
mtcars |> summarize(
  across(c(mpg, wt, hp),
         list(min    = ~min(.x, na.rm = TRUE),
              mean   = ~mean(.x, na.rm = TRUE),
              median = ~median(.x, na.rm = TRUE),
              max    = ~max(.x, na.rm = TRUE),
              n_miss = ~sum(is.na(.x))),
         .names = "{.col}_{.fn}")
) %>% 
  pivot_longer(everything(),names_to = c("variable","stat"),names_sep = "_") %>% 
  as.data.frame() <- dfout

###
summary6_multi <- function(data, ...) {
  data |> summarize(
    across(c(...),
           list(min    = ~min(.x, na.rm = TRUE),
                mean   = ~mean(.x, na.rm = TRUE),
                median = ~median(.x, na.rm = TRUE),
                max    = ~max(.x, na.rm = TRUE),
                n_miss = ~sum(is.na(.x))),
           .names = "{.col}_{.fn}")
  ) %>% 
    pivot_longer(everything(),names_to = c("variable","stat"),names_sep = "_") 
}


results <- mtcars |> summary6_multi(mpg, wt, hp)

### 19.3.1 exercises ####
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

f3(c("x","y","z"),"A")

### 19.4 conditional execution
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}
has_name(df)

ft <- function (x, y, op) {
  switch(op, plus = x + y, minus = x - y, times = x * y, divide = x/y,
         stop("Unknown op!"))
}

ft(2,3,"minus")
