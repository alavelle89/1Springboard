library(dplyr)
library(tidyr)
sampledata = read.csv("refine_original.csv")
sampledata1 <- as.data.frame(sampledata)
summary(sampledata1)
sampledata1
#identify the vector positions for the companies 
phillips_names <- grep("^p|^f", sampledata1$company, ignore.case = TRUE)
akzo_names <- grep("z", sampledata1$company, ignore.case = TRUE)
van_names <- grep("van", sampledata1$company, ignore.case = TRUE)
uni_names <- grep("r", sampledata1$company, ignore.case = TRUE)
sampledata1$company[phillips_names] <- "Phillips"
sampledata1$company[akzo_names] <- "Akzo"
sampledata1$company[uni_names] <- "Unilever"
sampledata1$company[van_names] <- "Van Houten"

# insert back into data frame
sampledata1$company <- sampledata1$company
head(sampledata1, n=Inf)


sampledata2 <- separate(sampledata1, Product.code...number ,c("Product.Code","Product.number") , "-") 
sampledata2 <- mutate(sampledata2, Product_Category = Product.Code) %>% unite(Full_address, address, city, country, sep=",")

p_cat <- grep("^p", sampledata2$Product_Category, ignore.case = TRUE)
sampledata2$Product_Category[p_cat] <- "Smartphone"
x_cat <- grep("^x", sampledata2$Product_Category, ignore.case = TRUE)
sampledata2$Product_Category[x_cat] <- "Laptop"
v_cat <- grep("^v", sampledata2$Product_Category, ignore.case = TRUE)
sampledata2$Product_Category[v_cat] <- "TV"
q_cat <- grep("^q", sampledata2$Product_Category, ignore.case = TRUE)
sampledata2$Product_Category[q_cat] <- "Tablet"
sampledata2

sampledata2$company_phillips <- as.numeric(sampledata2$company=="Phillips",1,0)
sampledata2$company_akzo <- as.numeric(sampledata2$company=="Akzo",1,0)
sampledata2$company_unilever <- as.numeric(sampledata2$company=="Unilever",1,0)
sampledata2$company_vanhouten <- as.numeric(sampledata2$company=="Van Houten",1,0)
sampledata2$Product_smartphone <- as.numeric(sampledata2$Product_Category=="Smartphone",1,0)
sampledata2$Product_laptop <- as.numeric(sampledata2$Product_Category=="Laptop",1,0)
sampledata2$Product_tv <- as.numeric(sampledata2$Product_Category=="TV",1,0)
sampledata2$Product_tablet <- as.numeric(sampledata2$Product_Category=="Tablet",1,0)
sampledata2
