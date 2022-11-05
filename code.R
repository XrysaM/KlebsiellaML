#This is the code 

library(readr)
k_9 <- read_csv("k_9_total_for_classification.csv")
summary(k_9)
View(k_9)

any(is.na(k_9)) #is False == no NAs
unique(k_9[c(3,4)]) #poia hosts kai posa

rm(k_9_test2)
k_9_test2 <- k_9

#alphabetical order
library(CatEncoders)
labs1 = LabelEncoder.fit(k_9_test2$host_categories)
labs2 = LabelEncoder.fit(k_9_test2$common_species_names)
k_9_test2$host_categories = transform(labs1, k_9_test2$host_categories)
k_9_test2$common_species_names = transform(labs2, k_9_test2$common_species_names)
#dixnei tis arxikes times
list1 <- unique(inverse.transform(labs1, k_9_test2$host_categories))  
list2 <- unique(inverse.transform(labs2, k_9_test2$common_species_names))
ogvalues1 <- list1[order(unlist(list1))]
ogvalues2 <- list2[order(unlist(list2))]
ogvalues1
ogvalues2