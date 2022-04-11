#########################
library("multiwayvcov")
library("lmtest")
library("stargazer")
library("dplyr")
library("ggplot2")
########################


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

##########################

load("data_final_25Jan2022.RData")
head(data_final)
nrow(data_final) ## 962 


#### main regressions shown #####


#note: from looking I discovered that year_f = factorised version of the year variable
#There is no code given for what data preprocessing was done

#Focus on dependent variable of Overall = Share of company report 
#devoted to gender equality issues

m1<-lm(prop_attn ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1, data_final$company)
m1_se <- as.matrix(coeftest(m1, vcov_company))

#Focus on dependent variable of Leadership = Share of company report devoted to gender 
#gap in leadership

m2<-lm(prop_lead ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m2, data_final$company)
m2_se <- as.matrix(coeftest(m2, vcov_company)) 

#Focus on dependent variable of Pay = Share of report 
#devoted to gender pay gap

m3<-lm(prop_pay ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m3, data_final$company)
m3_se <- as.matrix(coeftest(m3, vcov_company)) 

#Focus on dependent variable of Family Care = Share of report devoted to family 
#care (ie., childcare, birth/maternity, family leave, and scheduling flexibility)

m4<-lm(prop_family ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m4, data_final$company)
m4_se <- as.matrix(coeftest(m4, vcov_company)) 

#Focus on dependent variable of Discrim/Harass = Share of 
#report devoted to sexual discrimination and harassment

m5<-lm(prop_harass ~ year_f + company + quota + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m5, data_final$company)
m5_se <- as.matrix(coeftest(m5, vcov_company))

## Note: Stargazer code uses SE and p-values from clustered SE models

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "html",  out="Table2.doc")

# Note: Stargazer notes (beneath table) for p-values do not output correctly in Word
# -- have to add in manually "*p<0.05; **p<0.01; ***p<0.001"

### Table in Latex: 

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")))




########################################################################
######## what would an interaction have looked like? ###################


###adapting code from table 2 & 3 sections

####I have not changed variable names because it was messing with my 
#ability to get everything into a Latex output table for my presentation

#Focus on dependent variable of Overall = Share of company report 
#devoted to gender equality issues

m1 <-lm(prop_attn ~ year_f*quota + company + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m1, data_final$company)
m1_se <- as.matrix(coeftest(m1, vcov_company))

#Focus on dependent variable of Leadership = Share of company report devoted to gender 
#gap in leadership

m2<-lm(prop_lead ~ year_f*quota + company + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m2, data_final$company)
m2_se <- as.matrix(coeftest(m2, vcov_company)) 

#Focus on dependent variable of Pay = Share of report 
#devoted to gender pay gap

m3<-lm(prop_pay ~ year_f*quota + company + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m3, data_final$company)
m3_se <- as.matrix(coeftest(m3, vcov_company)) 

#Focus on dependent variable of Family Care = Share of report devoted to family 
#care (ie., childcare, birth/maternity, family leave, and scheduling flexibility)

m4<-lm(prop_family ~ year_f*quota + company + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m4, data_final$company)
m4_se <- as.matrix(coeftest(m4, vcov_company)) 

#Focus on dependent variable of Discrim/Harass = Share of 
#report devoted to sexual discrimination and harassment

m5<-lm(prop_harass ~ year_f*quota + company + sustain + pct_rev_change, 
       data=data_final)
vcov_company <- cluster.vcov(m5, data_final$company)
m5_se <- as.matrix(coeftest(m5, vcov_company))

## Note: Stargazer code uses SE and p-values from clustered SE models

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "html",  out="Table2.doc")

# Note: Stargazer notes (beneath table) for p-values do not output correctly in Word
# -- have to add in manually "*p<0.05; **p<0.01; ***p<0.001"

### Table in Latex: 

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")))



####Noticed that their stargazer template code was omitting year_f, so I took 
#that out to put that info on the output table and see what it looks like:

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")))


#####Looking at how an interaction like this changes the summary outputs#######

summary(m1)

coef(m1)

summary(m2)

summary(m3)

summary(m4)

summary(m5)

m1_se


###Comparing summary outputs to the other interaction they did between year and quota:####






################################using an anova test to see if interaction is helpful#############################################

interaction <- aov(prop_attn ~ year_f*quota, data = data_final)

summary(interaction)


stargazer(interaction) #not working so I'm going to provide a screenshot in 
#my presentation

##the 'year_f:quota' variable has a low sum-of-squares value and a high p-value,
#which means there is not much variation that can be explained by the interaction between
#year and quota.Doing the interaction in this way is therefore not very useful

#interaction2 <- aov(prop_attn ~ year_f*quota, data = data_final)

#summary(interaction)

#interaction3 <- aov(prop_attn ~ year_f*quota, data = data_final)

#summary(interaction)


##########################################################################################################################################
######## what would the output look like if we did not control for potential confounders and only looked at the quota? ###################


####I realised after doing this that they actually also did this themselves later on in their code and I 

#Missed it- I looked at what they did and put their output into latex as a table and it 

##was the same as mine, so good to know that it's replicable at least



###Interactive model excluding sustainability and Percent revenue Change#########


#Focus on dependent variable of Overall = Share of company report 
#devoted to gender equality issues

m1<-lm(prop_attn ~ year_f*quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m1, data_final$company)
m1_se <- as.matrix(coeftest(m1, vcov_company))

#Focus on dependent variable of Leadership = Share of company report devoted to gender 
#gap in leadership

m2<-lm(prop_lead ~ year_f*quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m2, data_final$company)
m2_se <- as.matrix(coeftest(m2, vcov_company)) 

#Focus on dependent variable of Pay = Share of report 
#devoted to gender pay gap

m3<-lm(prop_pay ~ year_f*quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m3, data_final$company)
m3_se <- as.matrix(coeftest(m3, vcov_company)) 

#Focus on dependent variable of Family Care = Share of report devoted to family 
#care (ie., childcare, birth/maternity, family leave, and scheduling flexibility)

m4<-lm(prop_family ~ year_f*quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m4, data_final$company)
m4_se <- as.matrix(coeftest(m4, vcov_company)) 

#Focus on dependent variable of Discrim/Harass = Share of 
#report devoted to sexual discrimination and harassment

m5<-lm(prop_harass ~ year_f*quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m5, data_final$company)
m5_se <- as.matrix(coeftest(m5, vcov_company))

## Note: Stargazer code uses SE and p-values from clustered SE models

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "html",  out="Table2.doc")

# Note: Stargazer notes (beneath table) for p-values do not output correctly in Word
# -- have to add in manually "*p<0.05; **p<0.01; ***p<0.001"

### Table in Latex: 

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")))


###Additive model excluding sustainability and Percent revenue Change#########

#Focus on dependent variable of Overall = Share of company report 
#devoted to gender equality issues

m1<-lm(prop_attn ~ year_f + quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m1, data_final$company)
m1_se <- as.matrix(coeftest(m1, vcov_company))

#Focus on dependent variable of Leadership = Share of company report devoted to gender 
#gap in leadership

m2<-lm(prop_lead ~ year_f + quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m2, data_final$company)
m2_se <- as.matrix(coeftest(m2, vcov_company)) 

#Focus on dependent variable of Pay = Share of report 
#devoted to gender pay gap

m3<-lm(prop_pay ~ year_f + quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m3, data_final$company)
m3_se <- as.matrix(coeftest(m3, vcov_company)) 

#Focus on dependent variable of Family Care = Share of report devoted to family 
#care (ie., childcare, birth/maternity, family leave, and scheduling flexibility)

m4<-lm(prop_family ~ year_f + quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m4, data_final$company)
m4_se <- as.matrix(coeftest(m4, vcov_company)) 

#Focus on dependent variable of Discrim/Harass = Share of 
#report devoted to sexual discrimination and harassment

m5<-lm(prop_harass ~ year_f + quota + company, 
       data=data_final)
vcov_company <- cluster.vcov(m5, data_final$company)
m5_se <- as.matrix(coeftest(m5, vcov_company))

## Note: Stargazer code uses SE and p-values from clustered SE models

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota", "Sustainability", "Percent Revenue Change"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "html",  out="Table2.doc")

# Note: Stargazer notes (beneath table) for p-values do not output correctly in Word
# -- have to add in manually "*p<0.05; **p<0.01; ***p<0.001"

### Table in Latex: 

stargazer(m1, m2, m3, m4, m5,
          font.size = "small",
          title="Table 2: Effects of Quota Law on Company Attention to Gender Equality",
          align=TRUE, dep.var.labels=c(
            "Overall",
            "Leadership", 
            "Pay", 
            "Family Care", 
            "Discrim/Harass"),
          se = list(m1_se[,2], 
                    m2_se[,2],
                    m3_se[,2],
                    m4_se[,2],
                    m5_se[,2]),
          p = list(m1_se[,4], 
                   m2_se[,4],
                   m3_se[,4],
                   m4_se[,4],
                   m5_se[,4]),
          covariate.labels=c("Quota"),
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE,
          omit = c("year_f", "company", "Constant"),
          column.sep.width = "-15pt", 
          digits = 3, digits.extra=0,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = "Robust standard errors clustered around company in parentheses.",
          add.lines = list(c("Company FEs", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Year FEs", "Yes", "Yes", "Yes", "Yes", "Yes")))

#########################################################################
############could/should they have included a glm?#######################


###its all about what the outcome looks like

#not a logit because the outcome is not binary


#not a glm in general because the outcome is, in fact, continuous and not categorical
#in nature

#It is not using count variables in the outcome to describe the number of words, 
#which would be discrete. The token counts are presented as a decimal and are therefore continuous.
#Because the outcome is continuous, an lm() regression is in fact appropriate here.

#Noticed a lot of null values in the data for variables such as prop_pay where in many reports 
#they were not mentioned even once, resulting in no tokens-  made me consider running another model 
#eg tobit regression, but ultimately because of the reasons above I felt the lm() method was probably more appropriate.




#########################################################################
############Regression Plots?#######################


#Can I do some regression plots?

#The original authors did not really do these and they don't seem to lend themselves to the clustering of the 
#5 different linear regressions


###replication of the plots they did give:


###Figure 1####

plotd <- subset(data_final, select = c(
  "year", 
  "company", 
  "country", 
  "prop_attn", 
  "prop_lead")) 

make_grey <- ggplot(data=plotd, aes(x=year, y=prop_attn, col=country)) +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), 
                     lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,]) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,]) + 
  xlab("Year") + 
  ylab("Attention") + 
  guides(shape=FALSE) + 
  theme(legend.title = element_blank()) +
  ggtitle("Overall attention to gender equality issues,\n before and after quota law (2011)") + 
  theme(plot.title = element_text(hjust = 0.5))

make_grey <- make_grey + scale_colour_grey()
make_grey

#######Right panel (leadership) 

make_grey2 <- ggplot(data=plotd, aes(x=year, y=prop_lead, col=country)) +
  geom_point() + 
  theme_bw() +
  scale_x_continuous(breaks=c(2007, 2011, 2017), lim = c(2007,2017)) +
  geom_smooth(method="lm", data=data_final[data_final$year < 2011,]) + 
  geom_smooth(method="lm", data=data_final[data_final$year > 2010,]) + 
  xlab("Year") + 
  ylab("Attention") + 
  guides(shape=FALSE) + 
  theme(legend.title = element_blank()) +
  ggtitle("Attention to gender gap in leadership,\n before and after quota law (2011)") + theme(plot.title = element_text(hjust = 0.5)) 

make_grey2 <- make_grey2 + scale_colour_grey()
make_grey2


####Figure 2#####

### Left panel, number of women 

##Plot female vs male members by year

plot_female_number <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_no_year = mean(fem_sum, na.rm=T), .groups = 'drop')

plot_female_number <- as.data.frame(plot_female_number)


plot_fem <- ggplot(plot_female_number,
                   aes(x=year, y=mean_no_year, col=country, group=country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 10) +
  scale_x_continuous(breaks = c( 
    2007,
    2008, 
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017)) +
  labs(y="Mean Number of Women Board Members",
       x="",
       title="",
       subtitle = "") +
  scale_y_continuous(breaks=seq(0.0, 10, 1), limits=c(0, 10))+
  theme(plot.title = element_text(size=12))+  theme_classic() + scale_colour_grey(start=0.8, end=0.2)
plot_fem


### Right panel, share of women 

plot_female_share <- data_final %>% 
  group_by(year, country) %>%
  summarize(mean_share_year = mean(fem_share, na.rm=T), .groups = 'drop')

plot_female_share <- as.data.frame(plot_female_share)

plot_fem2 <- ggplot(plot_female_share,
                    aes(x=year, y=mean_share_year, col=country, group=country)) +
  geom_point(stat="identity") + geom_line(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0, 50) +
  scale_x_continuous(breaks = c( 
    2007,
    2008, 
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017)) +
  labs(y="Mean Share of Women Board Members",
       x="",
       title="",
       subtitle = "") +
  theme(plot.title = element_text(size=12))+  theme_classic() + scale_colour_grey(start=0.8, end=0.2)
plot_fem2

###Greece actually exceeded Italy around 2008-2010 before the quotas


