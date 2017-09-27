---
title: "Test of Counts with Fisher's Exact Z-Test"
output:
  word_document: default
  pdf_document: default
  html_document: default
---
First let's create a data set with sexorien missing, gender, eth, and age variables.  Then create the missing variables

Here is a list of all the codes that I could not make sense of and therefore deleted for the following variables gender = dataGender; sex orientation = dataSexOrien; age = dataAge; ethnicity = dataEth.

For example in the dataGender variable, I could not figure out how to categorize "Professor"; therefore, I gave it the value -9, which means it will be deleted.

If you can make sense of these options and place them into categories please let me know and I can make the changes.

PropTest = http://www.sthda.com/english/wiki/two-proportions-z-test-in-r
```{r}
dataGender = ifelse(data$gender == "Professor", -9, ifelse(data$gender == "Straight", -9, ifelse(data$gender =="Nb", -9, ifelse(data$gender == "[Crossed something out]", -9, ifelse(data$gender == "\"Yes\"", -9, data$gender)))))

dataAge = ifelse(data$age == "\"?\"", -9, ifelse(data$age == "\"50+\"", -9,ifelse(data$age == "\"Old\"", -9, ifelse(data$age == "40 and fabulous",-9,ifelse(data$age == "No Pre", -9, ifelse(data$age == "\"60+\"", -9, ifelse(data$age == "\"2\"", -9, ifelse(data$age == "45+", -9, ifelse(data$age == "over 21", -9, ifelse(data$age == "\"30+\"", -9,ifelse(data$age == "\"Guess\"", -9, ifelse(data$age == "Illegible", -9, ifelse(data$age == "range 22-68", -9, ifelse(data$age == "\"4\"", -9, ifelse(data$age == "\"MYOB\"", -9, ifelse(data$age == "\"40ish\"", -9, ifelse(data$age == "\"Ø\"", -9, ifelse(data$age == "50+", -9,data$age))))))))))))))))))

dataEth = ifelse(data$eth == "\"?\"", -9, ifelse(data$eth == "\"Ø\"" , -9, data$eth))
```
Here is where I first recode all possible missing values into a "Missing" code and then delete the -9's which are the missing values I created in the step above.

For example, I believe that "Flank" is actually "Blank", which is one of the codes the Prism students to code missing values.  Therefore, I am recoding the "Flank" code to "Missing".  Let me know what you think about these codes.
```{r}
data1 = cbind(gender = dataGender, age = dataAge, sexorien = data$sexorien,eth =  dataEth, site = data$site)
data1 =as.data.frame(data1)
data1 =apply(data1, 2, function(x) {ifelse(x == "NA", "Missing", ifelse(x == -9, NA, ifelse(x == "Blank", "Missing", ifelse( x == "N/A", "Missing", ifelse(x == "Flank", "Missing",ifelse(x == "MIssing", "Missing", ifelse(x == "[Crossed something out]", "Missing", ifelse(x == "I'd need to know you better to share that =)", NA, ifelse(x =="[Crossed something out]", NA, x )))))))))})

data1 = as.data.frame(data1)
head(data1)
data1 = na.omit(data1)
sum(is.na(data1))
data1 = as.data.frame(data1)
dim(data1)
colnames(data1) = c("gender", "age", "sexorien", "eth", "site")
head(data1)
dim(data1)
library(plyr); library(dplyr)
# Create the all dummy with 1 equal to missing and 0 equal to non missing
data1 = as.data.frame(apply(data1, 2, function(x){ifelse(x == "Missing", 1, 0)}))
```
Need to gather the counts and N's but N's should all be the same
N = 1017
```{r}
testDim = dim(data1)
N = testDim[1]

countSexorien = count(data1, 'sexorien')
countSexorien = countSexorien$freq[2]

countGender = count(data1, 'gender')
countGender = countGender$freq[2]

countAge = count(data1, 'age')
countAge = countAge$freq[2]

countEth = count(data1, 'eth')
countEth = countEth$freq[2]

```

It is a difference in ratio of occurance http://udel.edu/~mcdonald/statfishers.html
Difference in rate of occurance for sexorien vs gender, age, and eth
```{r}
library(exact2x2)
sexOrienGender<-matrix(c(countSexorien,N-countSexorien,countGender,N-countGender),2,2,dimnames=list(c("Missing Yes","Missing No"),c("sexorien","gender")))

sexOrienAge<-matrix(c(countSexorien,N-countSexorien,countAge,N-countAge),2,2,dimnames=list(c("Missing Yes","Missing No"),c("sexorien","age")))

sexOrienEth<-matrix(c(countSexorien,N-countSexorien,countEth,N-countEth),2,2,dimnames=list(c("Missing Yes","Missing No"),c("sexorien","eth")))

fisher.exact(sexOrienGender)
fisher.exact(sexOrienAge)
fisher.exact(sexOrienEth)
alpha = .05/3; alpha
```

