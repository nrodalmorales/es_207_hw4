# Brandon M Genco HW 4

rm(list=ls())

#wd<-<-"/home/brandon/labserv/Personal/Brandon/Misc/Semesters/Spring_2020/es_207/hw4/" 
setwd(wd) #set wd as approriate

#### 3.1 ####
# data is Chloride concentration, in mg/L. pg 63. 
gr<-c(6,0.5,0.4,0.7,0.8,6.0,5,0.6,1.2,0.3,0.2,0.5,0.5,10,0.2,0.2,1.7,3)
hist(gr, breaks=length(gr))
summary(gr)

## Confidence Intervals - 95%

### Nonparametric CI:
#Unknown Population mean and Standard Deviation. Used Wilcox test (two tailed). Warnings related to non-normally distributed input data were surpressed. 
#I knew that already from the histogram above. 


ci.1<-wilcox.test(gr,alternative ="two.sided", conf.int = T, conf.level=0.95)
print(paste0("Nonparemetric Median Confidence interval is: ", "(", round(ci.1[8][[1]][1], 5), " , ",  round(ci.1[8][[1]][2],5), ")"))

#boot strapping
bstrap <- c()
for (i in 1:10000){
  bstrap <- c(bstrap, median(sample(gr,  replace = T)))
}
ci.2<-wilcox.test(bstrap ,alternative ="two.sided", conf.int = T, conf.level=0.95)
print(paste0("Nonparemetric Median Confidence interval from bootstrapped data (10,000 times) is:",
             "(", round(ci.2[8][[1]][1], 5), ",", round(ci.2[8][[1]][2],5), ")"))
hist(bstrap, breaks=1000, main="histogram of bootstrapped granodiorite data")

# The confidence interval shrank considerably for the bootstrapped sample. 
# However, bootstrapping produced a similar non-normal distribution as the original dataset

### Parametric CI: 
# Assumming standard deviation of 1 for the population. Perhaps std should be 0.1,  ...
me<-qnorm(0.95)*(1/sqrt(length(gr)))
low<-median(gr)-me
high<-median(gr)+me
print(paste0("Paremetric Median Confidence interval is: ", "(", round(low,3), ", ", round(high,3), ")"))

## Prediction Intervals - 95%
# For this section, used formula given in class (line 64 in "lec5_feb12.Rmd"), and updated code to reduce hardcoding

gr1<-sort(gr)
# code adaptedted from lecture 5 feb 12,  altered to run without hardcoding.

lo_i_r<-round(0.05*(length(gr1)+1),0) 
hi_i_r<-round(0.95*(length(gr1)+1),0)

lo_i<-0.05*(length(gr1)+1)
hi_i<-0.95*(length(gr1)+1)

hi_dif<-abs(hi_i-hi_i_r)
lo_dif<-abs(lo_i-lo_i_r)

low<-gr1[lo_i_r]+lo_dif*(gr1[(lo_i_r+1)] - gr1[lo_i_r])
upp<-gr1[hi_i_r]+hi_dif*(gr1[(hi_i_r-1)] - gr1[hi_i_r])

print(paste0("Non-Paremetric Median Prediction interval is: ", "(", upp, ", ", low, ")"))


### Non-parametric PI:

PI <- predict(lm(gr~1), 
              interval = "predict", 
              level = 0.95)
PI[1,]

## Answer to 3.1:
# Non-parametric seemed better for this dataset given, small sample size, unkown information on the real population, and non-normally distributed values.



#### 3.4 ####
rm(list=ls())
setwd(wd)
flo<-read_csv("Conecuh_River_apxc2.csv") # need to change dat input for your needs
head(flo)
dim(flo)
save_names<-names(flo) # saved original names
names(flo)<-c("year", "flow") # unit of flow is cfs. Names changed in r for typographical reasons
summary(flo)
sd(flo$flow, na.rm = T)
var(flo$flow, na.rm= T)
plot(flo$year, flo$flow)
hist(flo$flow, breaks =length(flo$flow))
qqnorm(flo$flow, main=" Normal Q-Q Plot: Yearly flow, CFS")
qqline(flo$flow, col="red")
# did not change graph labels, etc., for exploratory analysis

#Small dataset, yearly variabilty. Normality is hard to strongly reject or confirm from the qq plots and histrogram. 
#With high varaiblibity between years and a low smaple size this is bound to be the case. With two NA's the sample is even smaller.

## Confindence Interval - non-parametric for median - 95%

#The question asks for the appropriate interval. From looking at the basic structure of the data (above),
#I am concerned about small sample size and whether or not it accurately represents the population: ~~ yearly flow,
#for some longer time frame, (ignoring climatology state shifts, and  without information on lifespan of this particular hydrological system). 

### Nonparametric CI (sample only):
ci.1<-wilcox.test(flo$flow,alternative ="two.sided", conf.int = T, conf.level=0.95)
print(paste0("Nonparemetric Median Confidence interval for stream flow is: ", "(", round(ci.1[8][[1]][1], 1), " , ",  round(ci.1[8][[1]][2],1), ")"))

### Nonparametric CI (with bootstrapping):

bstrap <-c()
temp<-na.omit(flo$flow) # NA issuses.... try alternate methods here did not seem to work
for (i in 1:1000){
  bstrap <- c(bstrap, median(sample(temp,  replace = T)))
}

ci.2<-wilcox.test(bstrap ,alternative ="two.sided", conf.int = T, conf.level=0.95)
print(paste0("Nonparemetric Median Confidence interval from bootstrapped data (1000 times) is:",
             "(", round(ci.2[8][[1]][1], 1), ",", round(ci.2[8][[1]][2],1), ")"))
median(temp)

# The sample median lies outside this interval when boot strapping. I believe this may be a coding/sampling issues due to NAs,  as removing the NA, 
#prior to bootstrapping changes the N of the sample. It might alsow have a stastical interpreation suggesting that the small smaple gives difrrent reuslts as ooposed to teh bootstrapping, due to is small size. I don't feel that the analysis here is sufficient to make that statement.

## Answer to 3.4:
print(paste0("Nonparemetric Median Confidence interval for stream flow is: ", "(", round(ci.1[8][[1]][1], 0), " , ",  round(ci.1[8][[1]][2],0), ")"))

