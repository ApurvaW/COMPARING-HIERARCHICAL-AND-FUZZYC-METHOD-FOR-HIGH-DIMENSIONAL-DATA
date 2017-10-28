library(readr)
Andy <- read_csv("~/Masters Projet/Amazon-book reviews/Andy-Weir-The-Martian.csv", 
                                  col_names = FALSE)

fit <- lm(X2~ X3, data = Andy)
class(fit)
summary(fit)
anova(fit)
par(mfrow= c(2,2))
plot(fit)

library(xtable)
#print(xtable(fit), type="html")
print(xtable(fit))
print(xtable(anova(fit)))