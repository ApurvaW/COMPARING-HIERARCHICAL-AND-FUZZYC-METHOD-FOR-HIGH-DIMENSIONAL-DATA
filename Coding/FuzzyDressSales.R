library(readxl)
## Dress Sales Dataset
Dress <- read_excel("~/Masters Projet/Dresses-attributes_sales/Dress Sales.xlsx", 
                    +     col_types = c("numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric", 
                                        +         "numeric", "numeric", "numeric"))
View(Dress)

library(e1071)
library(scatterplot3d)
x <- rbind(Dress$Style, Dress$Price, Dress$Size, Dress$Season, Dress$NeckLine, Dress$SleeveLength, Dress$waiseline, Dress$Material, Dress$FabricType, Dress$Decoration, Dress$`Pattern Type`)
x <-t(x)
result <- cmeans(x,7,500,verbose = TRUE,method = "cmeans")
print(result)
s3d <- scatterplot3d(result$membership, color = result$cluster, type = "h", angle = 55, scale.y = 0.7, pch = 16, main = "Pertinence of Fuzzy EPM- Dress_Sales")
plot(intermediate_grades, col=result$cluster)
