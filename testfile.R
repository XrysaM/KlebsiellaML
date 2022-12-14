#diafora tests

#encoders

rm(k_9_test)
k_9_test <- k_9[65:85, 3:4 ]

#simple
k_9_test$host_categories <- as.numeric(factor(k_9_test$host_categories)) #den 3erw poies times einai poio host

#works 
k_9_test$host_categories <- as.factor(k_9_test$host_categories)
k_9_test$common_species_names <- as.factor(k_9_test$common_species_names)
k_9_test <- data.frame(k_9_test)
k_9_test$host_categories <- encode_ordinal(k_9_test[,1,drop=FALSE], order = c('human','birds', 'pig'), out.int = TRUE)
k_9_test$common_species_names <- encode_ordinal(k_9_test[,2,drop=FALSE], order = c('human','turkey', 'pig'), out.int = TRUE)

#the best 
#alphabetical order
library(CatEncoders)
labs = LabelEncoder.fit(k_9_test$host_categories)
k_9_test$host_categories = transform(labs, k_9_test$host_categories)
#dixnei tis arxikes times
unique(inverse.transform(labs, k_9_test$host_categories)) 

#encode host_categories
lab = LabelEncoder.fit(k_9$host_categories)
k_9$host_categories = transform(lab, k_9$host_categories)
#dixnei tis arxikes times
list <- unique(inverse.transform(lab, k_9$host_categories))
ogvalues_host_categories <- list[order(unlist(list))]
ogvalues_host_categories




