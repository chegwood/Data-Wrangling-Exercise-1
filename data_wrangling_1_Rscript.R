#set-up file

setwd('C:\\Users\\Chegwood\\Documents\\Data Science Stuff\\Springboard - Foundations of Data Science\\Data Wrangling Exercise 1')
library(dplyr)
library(tidyr)
refine_original <- read.csv('refine_original.csv')

#define lookup table
product_cat_lookup <- data_frame(product_code = c('p','v','x','q'), product_categories = c('smartphone','tv','laptop','tablet'))

#create new data frame and clean data

refine_clean <- refine_original %>% 
  #standardize companies / fix misspellings
  mutate(company = tolower(company)) %>% 
  mutate(company = sub('ph.*','philips',company)) %>% 
  mutate(company = sub('fil.*','philips',company)) %>% 
  mutate(company = sub('ak.*','akzo',company)) %>% 
  mutate(company = sub('van.*','van houten',company)) %>% 
  mutate(company = sub('uni.*','unilever',company)) %>%
  #separate product_code and product_number then join product_categories
  separate(Product.code...number, into = c('product_code','product_number'), sep = "-") %>% 
  left_join(product_cat_lookup) %>%
  #create full_address for geocoding
  unite(full_address, address:country, sep = ',') %>% 
  #create binary variables
  mutate(company_philips = ifelse(company == 'philips',1,0)) %>% 
  mutate(company_akzo = ifelse(company == 'akzo',1,0)) %>% 
  mutate(company_van_houten = ifelse(company == 'van houten',1,0)) %>% 
  mutate(company_unilever = ifelse(company == 'unilever',1,0)) %>% 
  mutate(product_smartphone = ifelse(product_categories == 'smartphone',1,0)) %>% 
  mutate(product_tv = ifelse(product_categories == 'tv',1,0)) %>% 
  mutate(product_laptop = ifelse(product_categories == 'laptop',1,0)) %>% 
  mutate(product_tablet = ifelse(product_categories == 'tablet',1,0)) %>% 
  #remove redundant variables
  select(-c(company, product_code, product_categories))

#print .csv file

write.csv(refine_clean, 'refine_clean.csv', row.names = FALSE)
