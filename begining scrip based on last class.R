#+ Statistical programming in R
#+ Edgar Aguilar, PS 1
#+ Spring 2024


#+ Questions from assignment:
#+ Briefly describe the dataset. What country did you choose? How many respondents
#+ are there in the survey, and when were the interviews conducted?
#+ Describe your respondents. Using appropiate descriptive statistics, tell me about
#+ their ages, distribution of male vs female, language, etc. Refer to the codebook
#+ to find questions you feel are appropiate here. BONUS: compose a single table with
#+ relevant statistics describing the sample.
#+ Describe attitudes about economic and political influence of China, Q78A in your
#+ data. Your answer should indicate a relative frequency table and a couple of quick
#+ sentences describing the results.
#+ Repeat this process for Q78 B about the influence of the United States.
#+ Use a paired t-test to evaluate the difference between perceptions. To do this, 
#+ you will need to clean both variables to exclude dk/na and refusals. See the
#+ example code below, but note that you may need to adjust depending the method
#+ you used to read in the data. Describe your findings. Use a two-tailed test and 
#+ 5% significance. 

# Packages ---------------------------------
# attaching a library (for installed package)
  library(tidyverse)

  install.packages("haven")
  
  library(haven)
  
# access a function on the fly
  df <- haven::read_sav('Congo_Round9.sav')


# Data I/O ---------------------------------
# Import our trash wheel data
  data <- read_sav('Congo_Round9.sav')

  # View the data
  head(df)  
  
# compare two alternatives:
  read_csv(file = 'trash_wheels.csv')
  df2 <- read_csv(file = 'trash_wheels.csv')

# Output
  save(df, 'newdata.RData')
  write_csv(df, 'newdata.csv')  

#+ WARNINGS: 
#+    Please avoid read.csv() 
#+    Do not use point + click for Data I/O


# Explore ----------------------------------
# Structure of a data frame
  str(df)
  summary(df)  

  ## character/factor vars -------
  ## frequency table
  tab1 <-
    df %>%
    count(wheel, name = 'N') %>%
    mutate(Per = N / sum(N) * 100)

  ## Beautify the table for printing
  tab1 %>%
    knitr::kable(digits = 1L) # optional last step

  ## Graphing
  plot1 <-
    tab1 %>%
    ggplot(aes(x = wheelName, y = percent)) +
    geom_col()  


  ## numeric vars ----------------
  ## Summary stats
  summary(df['cigarettes'])

  df %>% # get summary stats
    summarize(
      AvgCigs = mean(cigarettes, na.rm = T),
      AvgBottles = mean(plastic_bottles, na.rm = T),
      Diff = mean(cigarettes, na.rm = T) - mean(plastic_bottles, na.rm = T)
    )

  ## Plot the distribution    
  df %>%
    ggplot(aes(x = plastic_bags)) +
    geom_histogram(color = 'white')    


# Objects --------------
# Vectors/values
  a <- letters[1:10] # check type: is(a)
  b <- 100:109

# Data frames/tibbles
  df1 <- data.frame(a, b)
  df2 <- tibble(b, a)
  l1 <- list(d1 = df1, d2 = df2)

# Extract an item from list or frame
  ex1 <- df1$b
  ex2 <- df1[['b']]
  ex3 <- 
    df1 %>% 
    pull(b)

# Subset an element (maintains structure)
  sub1 <- df1[2]
  sub2 <- df1['b'] # use quotes to call by name
  sub3 <- df1[1:4, 'b'] # select rows 1-4, variable 'b'
  sub4 <- 
    df1 %>%
    select(b) %>%
    filter(b <= 103)  


#+ MAIN FUNCTIONS ENCOUNTERED
#+    read_***() to input data
#+    mutate() to create new coulmns
#+    select() to select a dataframe  
#+    filter() to select subset of cases from df
#+    = or <- to assign new objects
#+    $ and [[]] to extract element of object
#+    [] to subset object