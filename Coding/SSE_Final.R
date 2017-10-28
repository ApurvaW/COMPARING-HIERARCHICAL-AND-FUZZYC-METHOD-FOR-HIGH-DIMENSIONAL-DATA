#data("iris")

library(readxl)
final <- read_excel("~/Masters Projet/EPM/Data/final_grades.xlsx")
View(final)

fit <- lm(final$`ES 1.1(2 points)`~ final$`ES 1.2(3 points)`, data = final)
class(fit)
summary(fit)
anova(fit)
par(mfrow= c(2,2))
plot(fit)

library(xtable)
#print(xtable(fit), type="html")
print(xtable(fit))
print(xtable(anova(fit)))