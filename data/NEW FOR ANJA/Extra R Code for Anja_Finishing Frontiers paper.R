# Heres parts of script related to the remaining questions about the manuscript. Note that not all of this necessarily has to be included in the RMarkdown, but just for you to decide.

# Section 2.3 Hormone collection and analyses- welches two sample t-test. Dataset is in the data folder. 

saliva<-read.table("pig.oxt100222.for.parallelism.txt", sep="\t", header=T,stringsAsFactors = T)

# subset for t-test- here I compare the OXT concentration (column 6 in the dataset) between the standard curve of the assay ("standards" from column 2) and our in-house dilution ("pools from column 2). We want the slope of the standards and the pools to be the same. Our supplementary Fig 1 that you have with the supplemental materials does show that the slopes are parallel as expected.

pools=saliva[1:5,6]

standards=saliva[6:10,6]

xx=cbind(pools,standards)
xx=as.data.frame(xx)

t.test(xx$pools,xx$standards,paired=F)

Welch Two Sample t-test

data:  xx$pools and xx$standards
t = -0.2561, df = 7.8196, p-value = 0.8045
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -233.9449  187.3449
sample estimates:
  mean of x mean of y 
126.24    149.54 

# Below is same thing for the cortisol assay:


cort<-read.table("piglet.cort.from.bapsr.for.parallelism.txt", sep="\t", header=T,stringsAsFactors = T)


# subset for t-test

pools=cort[1:6,6]

standards=saliva[7:12,6]

xx=cbind(pools,standards)
xx=as.data.frame(xx)

t.test(xx$pools,xx$standards,paired=F)

Welch Two Sample t-test

Welch Two Sample t-test

data:  xx$pools and xx$standards
t = -2.0958, df = 4.1179, p-value = 0.1022
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -148.27581   19.89581
sample estimates:
  mean of x mean of y 
7.45     71.64 

#############################


# Section 2.4 Statistical analysis

# Lines 267-368- tests of model stability. Uses this function (I put it in the data folder):

source("/Users/liza/Desktop/Rogers Stats Course 2021/Rogers GLMM course 2021/glmm_stability.R")

# heres the script for testing stability of the cortisol model- may take a few minutes to run!

full.stab=glmm.model.stab(model.res=cortbycondition, contr=NULL, para=F,data=NULL)

round(full.stab$summary[, -1], 3)
is.re=grepl(x=rownames(full.stab$summary), pattern="@")

m.stab.plot(full.stab$summary[!is.re, -1])

# heres the script for testing stability of the oxytocin model- may take a few minutes to run!

oxt.full.stab=glmm.model.stab(model.res=oxtbycondition, contr=NULL, para=F,data=NULL)

round(oxt.full.stab$summary[, -1], 3)

is.re=grepl(x=rownames(oxt.full.stab$summary), pattern="@")

m.stab.plot(oxt.full.stab$summary[!is.re, -1])

#########################

# lines 380-384 in the text- Friedmans with post-hoc bonferronis:


library(exactRankTests)
library(coin)

behav.df=read.table(file="BAPSR.all.behav.for.friedman.txt",header=T,sep="\t")

behav.df$base.agg.per.min=as.numeric(as.character(behav.df$base.agg.per.min))
behav.df$base.prop.active=as.numeric(as.character(behav.df$base.prop.active))
behav.df$base.friendly.per.min=as.numeric(as.character(behav.df$base.friendly.per.min))
behav.df$base.play.per.min=as.numeric(as.character(behav.df$base.play.per.min))

str(behav.df)

# getting mean ranks for all behaviors:

(apply(behav.df[,2:13],2,mean,na.rm=T))
(apply(behav.df[,2:13],2,sd,na.rm=T))

# aggression between conditions:

friedman.test(as.matrix(behav.df[,2:4]))
# Friedman chi-squared = 9.5385, df = 2, p-value = 0.008487 # aggression differs significantly between the treatments

# prop scans active between conditions:

friedman.test(as.matrix(behav.df[,5:7]))

# Friedman rank sum test

data:  as.matrix(behav.df[, 5:7])
# Friedman chi-squared = 11.143, df = 2, p-value = 0.003805 # prop active differs between conditions

# amount friendly between conditions:

friedman.test(as.matrix(behav.df[,8:10]))

# Friedman chi-squared = 5.4286, df = 2, p-value = 0.06625 # friendly behavior does not differ significantly between conditions

# amount individual or group play between conditions:

friedman.test(as.matrix(behav.df[,11:13]))

# Friedman chi-squared = 10.889, df = 2, p-value = 0.00432 # play differs between conditions

# Now doing post-hoc wilcoxon tests and plotting results:

library(tidyverse)
library(rstatix)
library(ggpubr)

long.df=read.table(file="BAPSR.group.behav.for.wilcox.and.plot.txt",header=T,sep="\t")

long.df$group=as.factor(as.character(long.df$group))
long.df$context=as.factor(as.character(long.df$context))
long.df$prop.active =as.numeric(as.character(long.df$prop.active))
long.df$aggression=as.numeric(as.character(long.df$aggression))
long.df$locomotor.play=as.numeric(as.character(long.df$locomotor.play))
long.df$friendly=as.numeric(as.character(long.df$friendly))

pwc_agg=long.df %>% wilcox_test(aggression~context,paired=T,p.adjust.method="bonferroni")

pwc_active=long.df %>% wilcox_test(prop.active~context,paired=T,p.adjust.method="bonferroni")

pwc_lmplay=long.df %>% wilcox_test(locomotor.play~context,paired=T,p.adjust.method="bonferroni")

pwc_friendly=long.df %>% wilcox_test(friendly~context,paired=T,p.adjust.method="bonferroni")

long.df$context=factor(long.df$context,levels=c("weaning","play","baseline"))

###################################

# Section 3, results:

# lines 396-412- code for ANOVAs on overall effects of predictors, for final cortisol and oxytocin model. I do this to get the overall effect of a predictor on the response, before post-hoc em means or other tests:

library(lmerTest)

anova(cortbycondition, type=2)

round(summary(cortbycondition)$coefficients,3)

anova(oxtbycondition, type=2)

round(summary(oxtbycondition)$coefficients,3)


#################################