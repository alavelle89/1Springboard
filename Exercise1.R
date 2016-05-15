library(dplyr)
library(tidyr)
sampledata = read.csv("refine_original.csv")
tbl_df(sampledata)
sampledata$company[1:6] <- 'phillips'
sampledata$company[14:16] <- 'phillips'
sampledata$company[17:21] <- 'van houten'
sampledata$company[22:25] <- 'unilever'




sampledata %>% 
  separate(Product.code...number ,c("Product.Code","Product.number") , "-") %>%
  mutate(Product_Category = Product.Code) %>% unite(Full_address, address, city, country, sep=",") 






  
  


