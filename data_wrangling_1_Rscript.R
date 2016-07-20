#set-up file

setwd('C:\\Users\\Chegwood\\Documents\\Data Science Stuff\\Springboard - Foundations of Data Science\\Data Wrangling Exercise 1')
library(dplyr)
library(tidyr)
refine_original <- read.csv('refine_original.csv')

#create clean data frame
refine_clean <- refine_original

#convert companies to lowercase and fix misspellings

refine_clean$company = refine_clean$company %>% tolower()
refine_clean$company = sub('ph.*','philips',refine_clean$company)
refine_clean$company = sub('fil.*','philips',refine_clean$company)
refine_clean$company = sub('ak.*','akzo',refine_clean$company)
refine_clean$company = sub('van.*','van houten',refine_clean$company)
refine_clean$company = sub('uni.*','unilever',refine_clean$company)

#separate product code and number

refine_clean = separate(refine_clean, Product.code...number, into = c('product_code','product_number'), sep = "-")

#add product categories and full address columns

product_cat_lookup <- data_frame(product_code = c('p','v','x','q'), product_categories = c('smartphone','tv','laptop','tablet'))
refine_clean = left_join(refine_clean, product_cat_lookup)
refine_clean = refine_clean %>% unite(full_address, address:country, sep = ',')

#create binary variables

company_philips <- ifelse(refine_clean$company == 'philips',1,0)
company_akzo <- ifelse(refine_clean$company == 'akzo',1,0)
company_van_houten <- ifelse(refine_clean$company == 'van houten',1,0)
company_unilever <- ifelse(refine_clean$company == 'unilever',1,0)

product_smartphone <- ifelse(refine_clean$product_categories == 'smartphone',1,0)
product_tv <- ifelse(refine_clean$product_categories == 'tv',1,0)
product_laptop <- ifelse(refine_clean$product_categories == 'laptop',1,0)
product_tablet <- ifelse(refine_clean$product_categories == 'tablet',1,0)

#add binary columns to cleaned data
refine_clean = refine_clean %>% cbind(company_philips, company_akzo, company_van_houten, company_unilever, product_smartphone, product_tv, product_laptop, product_tablet)
write.csv(refine_clean, 'refine_clean.csv', row.names = FALSE)
