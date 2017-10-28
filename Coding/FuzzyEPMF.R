library(readxl)
final <- read_excel("~/Masters Projet/EPM/Data/final_grades.xlsx")
View(final)

library(e1071)
library(scatterplot3d)
x <- rbind(final$`ES 1.1(2 points)`, final$`ES 1.2(3 points)`, final$`ES 2.1(2 points)`,final$`ES 2.2(3 points)`, final$`ES 3.1(1 points)`, final$`ES 3.2(2 points)`, final$`ES 3.3(2 points)`, final$`ES 3.4(2 points)`, final$`ES 3.5(3 points)`, final$`ES 4.1(15 points)`, final$`ES 4.2(10 points)`, final$`ES 5.1(2 points)`, final$`ES 5.2(10 points)`, final$`ES 5.3(3 points)`, final$`ES 6.1
(25 points)`, final$`ES 6.2(15 points)`, final$`TOTAL(100 points)`)
x <-t(x)
result <- cmeans(x,3,50,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence of Fuzzy EPM- Final")
plot(intermediate_grades, col=result$cluster)
