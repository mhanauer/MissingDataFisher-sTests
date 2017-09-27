---
title: "Test of Proportions with T-Test"
output:
  word_document: default
  pdf_document: default
  html_document: default
---
It is a difference in ratio of occurance http://udel.edu/~mcdonald/statfishers.html
```{r}
download.file("http://murraylax.org/datasets/loanapp.RData", "loanapp.RData")
load("loanapp.RData")

missingSexOrienGender<-matrix(c(4,50,11,569),2,2,dimnames=list(c("Missing Yes","Missing No"),c("sexorien","gender")))
missingSexOrienGender
install.packages("exact2x2")
library(exact2x2)
fisher.exact(missingSexOrienGender)
```

