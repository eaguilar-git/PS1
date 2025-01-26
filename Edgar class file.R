#+ Programming in R 
#+ Edgar Aguilar
#+ January 2025

# Packages -------
# attaching a library 
  library(tidyverse)

# access a function on the fly
df <- readr:: read_csv('trash_wheels.csv')


# Data I/O -----
# import trash wheel data
  df <- read_csv('trash_wheels.csv')

# output
 write_csv(df, 'output file.csv')
 save(df, file = 'newdata.rdata')
 
# Explore data -----
  str(df)
 summary(df)
 
## Character variables ----
 ## frequency table
 tab1 <-
   df %>%
   count(wheel, name = 'N') %>%
   mutate(Per = N / sum(N) *100)
 
 ## Beautify the table for printing ------
 tab1 %>%
   knitr::kable()
 
 ## Graphing -----
 tab1 %>%
   ggplot(aes(x = wheel, y = Per)) + geom_col()
 
 # Objects ------
 # vector/values
 a <- letters [1:10]
 b <- 100:109
 
 # Data frame/tibbles
 df1 <- data.frame(a,b)
 df2 <- tibble(b,a)
 l1 <- list(d1 = df1, d2 = df2)
 
 
 # Extract an item from list or frame
 obj1 <- df1$b
 obj2 <- df1[['b']]
 
 # Subset an object
 obj3 <- df1[2]
 obj4 <- df2[1]
 sub1 <- df1[c(1,3, 5), 2]