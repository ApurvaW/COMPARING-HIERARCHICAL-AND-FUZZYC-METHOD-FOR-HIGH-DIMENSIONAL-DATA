library(readxl)
## EPM intermediate Dataset
intermediate_grades <- read_excel("~/Masters Projet/EPM/Data/intermediate_grades.xlsx")
View(intermediate_grades)

library(e1071)
library(scatterplot3d)
x <- rbind(intermediate_grades$`Session 2`, intermediate_grades$`Session 3`, intermediate_grades$`Session 4`,intermediate_grades$`Session 5`, intermediate_grades$`Session 6`)
x <-t(x)
result <- cmeans(x,3,50,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence of Fuzzy EPM- Intermediate")
plot(intermediate_grades, col=result$cluster)
