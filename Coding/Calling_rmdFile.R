
setwd("C:\Users\Owner\Documents\Coding")

#Loading packages
require(knitr)
require(markdown)

#Create .md, .html and .pdf files
knit("SSE_Final.Rmd")
markdownToHTML("SSE_Final.Rmd", "SSE_Final.html", options = c("use_xhml"))
system("pandoc -s SSE_Final.html -o SSE_Final.pdf")
