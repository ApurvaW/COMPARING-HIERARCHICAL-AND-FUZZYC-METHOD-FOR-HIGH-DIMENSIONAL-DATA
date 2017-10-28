library(tm) #Text Mining Package
library(readr)
final <- read_excel("~/Masters Projet/EPM/Data/final_grades.xlsx")
cor<- cor(final)
plot(cor)