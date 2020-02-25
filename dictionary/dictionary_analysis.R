###########################################
# Dictionary Analysis
# by: Isaac J. Faber
###########################################

library(wordnet) #this package is quite tricky to get working must already have wordnet installed on os

filter <- getTermFilter("StartsWithFilter", "car", TRUE)
terms <- getIndexTerms("NOUN",5,filter)

