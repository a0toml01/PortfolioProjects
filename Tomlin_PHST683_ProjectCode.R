library(survival)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(cobalt)
library(MatchIt)
library(vcd)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(labelled)
library(gtsummary)
library(webshot2)
library(Hmisc)
library(survminer)
#LOAD DATA
load("~/Biostatistics/AbuseStudy.Rdata")

#SUMMARIZE DF/CHECK FOR MISSING VALUES
summary(dat)
sum(is.na(dat))

#APPLY VARIABLE ATTRIBUTE LABLES TO WIDE-FORM DATA SET
var_label(dat$event) <- "Event"
var_label(dat$program) <- "Program"
var_label(dat$age) <- "Age"
var_label(dat$sex) <- "Sex"
var_label(dat$minority) <- "Minority"
var_label(dat$poverty) <- "Family Below Poverty Threshold"
var_label(dat$subst.abuse) <- "Substance Abuse in Household"
var_label(dat$crim.hist) <- "Criminal History in Household"
var_label(dat$first) <- "First CPS Contact"
var_label(dat$region) <- "Region"
var_label(dat$agency) <- "Agency"

#FACTORIZE VARIABLES (FOR CONTINGENCY TABLES)
dat$minorityf <- factor(dat$minority, labels = c("Non-minority", "Minority"))
var_label(dat$minorityf) <- "Minority"
dat$povertyf <- factor(dat$poverty, labels = c("No", "Yes"))
var_label(dat$povertyf) <- "Poverty"
dat$substf <- factor(dat$subst.abuse, labels = c("No","Yes"))
var_label(dat$substf) <- "Substance Abuse"
dat$crimhistf <- factor(dat$crim.hist, labels = c("No","Yes"))
var_label(dat$crimhistf) <- "Criminial History"
dat$firstf <- factor(dat$first, labels = c("No", "Yes"))
var_label(dat$firstf) <- "First Contact with CPS"



#ASSESSING TREATMENT BALANCE AMONG COVARIATES
#CONTINUOUS VARIABLES-AGE
#T-TEST
t.test(age~program, data = dat)
#AGE-PROGRAM DENSITY PLOT
ggplot(dat, aes(x=age, color = program))+
  geom_density() +
  labs(x = "Age", y = "Density", color = "Program")+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size=10)
  )
#BOXPLOT-AGE BY PROGRAM
ageboxplot <- ggplot(dat, aes(x=program, y=age, fill=program))+
  geom_boxplot() +  ggtitle("Age by Program") +
  labs(x = "Program", y="Age") +
  theme(legend.position = "none") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size=10)
  ) 

#CATEGORICAL VARIABLES
#PEARSON'S CHI-SQUARED TEST
chisq.test(dat$program, dat$sex, correct = F)
chisq.test(dat$program, dat$minority, correct = F)
chisq.test(dat$program, dat$poverty, correct = F)
chisq.test(dat$program, dat$subst.abuse, correct = F)
chisq.test(dat$program, dat$subst.abuse, correct = F)
chisq.test(dat$program, dat$crim.hist, correct = F)
chisq.test(dat$program, dat$first, correct = F)

#CONTIGENCY TABLES W/ CHI-SQUARED TEST
#PROGRAM X SEX
dat %>%
  tbl_cross(
    row =sex,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
#PROGRAM X MINORITY
dat %>%
  tbl_cross(
    row = minorityf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
#PROGRAM X POVERTY
dat %>%
  tbl_cross(
    row = povertyf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
#PROGRAM X SUBST ABUSE
dat %>%
  tbl_cross(
    row = substf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
#PROGRAM X CRIM HIST
dat %>%
  tbl_cross(
    row = crimhistf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()
#PROGRAM X FIRST CONTACT W/ CPS
dat %>%
  tbl_cross(
    row = firstf,
    col = program,
  ) %>%
  add_p(
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_hide(stat_0) %>%
  bold_labels()

#VAR EFFECT ON TIME TO EVENT (NON SURVIVAL CONTEXT)
#T-TEST: MEAN TIME-TO-EVENT 
t.test(dat$time~dat$program, subset = dat$event==1) # p<0.05 
t.test(dat$time~dat$sex, subset = dat$event==1)
t.test(dat$time~dat$minority, subset = dat$event==1)
t.test(dat$time~dat$poverty, subset = dat$event==1)  
t.test(dat$time~dat$subst.abuse, subset = dat$event==1)
t.test(dat$time~dat$crim.hist, subset = dat$event==1) #test in larger sample
t.test(dat$time~dat$first, subset = dat$event==1)

#BINARY LOGISTIC MODELS 
mod.base <- glm(event~1, data=dat[,3:11], family = "binomial")
mod.prog1 <- update(mod.base,~.+program, data = dat) #p<.001
summary(mod.prog1)
mod.age1 <- update(mod.base,~.+age, data = dat) #p<.001
summary(mod.age1)
mod.sex1 <- update(mod.base,~.+sex, data = dat) #p = 0.394
summary(mod.sex1)
mod.min1 <- update(mod.base,~.+minority, data = dat) #p = 0.672
summary(mod.min1)
mod.pov1 <- update(mod.base,~.+poverty, data = dat) #p = .007
summary(mod.pov1)
mod.subst1 <- update(mod.base,~.+subst.abuse, data = dat) #p = .307
summary(mod.subst1)
mod.ch1 <- update(mod.base,~.+crim.hist, data = dat) #p = .002
summary(mod.ch1)
mod.firstCPS1 <- update(mod.base,~.+first, data = dat) #p=.004
summary(mod.firstCPS1)
#MAIN EFFECTS BINARY LOGISTIC MODEL(EXCL REGION, AGENCY)
mod <- glm(event~., data = dat[,3:11], family = "binomial")
summary(mod)
#SUMMARY TABLE
tbl_regression(mod, exponentiate = T)%>%
  modify_caption("Table 3: Logistic Regression-Main Effects Model Summary")
#ANALYSIS OF VARIANCE
aov(mod)
#GOODNESS OF FIT-DEVIANCE
1-pchisq(q=mod$null.deviance - mod$deviance, df=length(coef(mod)))
#COEF COMPARISON
drop1(mod, test="Chisq")
#RESPONSE RESIDUALS
dat$rsp_resmod <- resid(mod, type = "response")
mean(dat$rsp_resmod)
ggplot(dat, aes(x=rsp_resmod))+
  geom_density()+
  geom_vline(xintercept = 0, linetype="dotted", linewidth = 1.3)+
  ggtitle("Logistic Regression Response Residuals-Main Effects Model")+
  labs(x = "Response Residuals", y = "Density")+
  geom_density(aes(x=rsp_resmod))+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size=10)
  )


#WIDE TO LONG/COUNTING FORMAT DATA FRAME TRANSFORMATION
dat.long <- data.frame(id=numeric(length=0), start=numeric(length=0), stop=numeric(length=0), 
                       outofhome=numeric(length=0), event=numeric(length=0))
for(i in 1:nrow(dat)) { 
  start <- 0
  stop <- numeric(length=0)
  outofhome <- 0
  event <- numeric(length=0)
  if (dat$removal[i]==1) {
    start <- c(start, dat$time.removal[i])
    stop <- c(stop, dat$time.removal[i])
    outofhome <- c(outofhome,1)
    event <- c(event, 0)
  }
  if (dat$return[i]==1) {
    start <- c(start, dat$time.return[i])
    stop <- c(stop, dat$time.return[i])
    outofhome <- c(outofhome,0)
    event <- c(event, 0)
  }
  stop <- c(stop, dat$time[i])
  event <- c(event, dat$event[i])
  temp.frame <- data.frame(id=dat$id[i],start,stop,event,outofhome)
  dat.long <- rbind(dat.long,temp.frame)
}
###CONSTRUCTING TIME-DEPENDENT DATA SET FOR COX MODEL
#MERGE DATA FRAMES ON ID 
dat.demo <- dat[,c(1,4:13)]
dat.new <- merge(dat.long,dat.demo, on = "id")
head(dat.new)

#SURV OBJECT
abuse.surv <- Surv(dat.new$start, dat.new$stop, dat.new$event, type="counting")
### MAIN EFFECTS COX MODEL
mod.cox.main <- coxph(abuse.surv~outofhome+program+age+sex+minority+
                        poverty+subst.abuse+crim.hist+first, data=dat.new)
summary(mod.cox.main)
mc.main<- coxph(abuse.surv~outofhome+program+age+sex+minority+
                  poverty+subst.abuse+crim.hist+first, data=dat.new)
#STEPWISE VARIABLE SELECTION
mod.cox.s <- step(mod.cox.main, scope = ~outofhome+program+age+sex+minority+poverty+subst.abuse
                  +crim.hist+first, data=dat.new, direction = "backward")
summary(mod.cox.s)
cox.zph(mod.cox.s) #poverty violates PH assumption
#STRATIFY BY POVERTY
mod.povs <- coxph(abuse.surv~outofhome+program+age+minority+subst.abuse+crim.hist+first+strata(poverty), data=dat.new)
summary(mod.povs)
#TEST REMOVAL VS STRATIFICATION
mod.cox.removepov$loglik
mod.povs$loglik
extractAIC(mod.cox.removepov)
extractAIC(mod.povs) #lower AIC
extractAIC(mod.s)
#COX MOD STRATIFIED BY REGION
mod.cox.region <- update(mod.povs, ~.+strata(region))
summary(mod.cox.region)
anova(mod.cox.s, mod.cox.region)
anova(mod.cox.s, mod.cox.region, test = "Chisq")
mod.cox.region$loglik
#COX MOD STRATIFIED BY AGENCY
mod.cox.agency <- update(mod.povs, ~.+strata(agency))
summary(mod.cox.agency)
anova(mod.cox.s,mod.cox.agency, test = "Chisq")
anova(mod.cox.region,mod.cox.agency)
mod.cox.agency$loglik
#COX MOD STRATIFIED BY REGION AND AGENCY
mod.cox.RA <- update(mod.povs, ~.+strata(region)+strata(agency))
summary(mod.cox.RA)
anova(mod.cox.s, mod.cox.RA)
mod.cox.RA$loglik
anova(mod.povs, mod.cox.RA, test = "Chisq")
#(RENAME)
mod.cox.main <- update(mod.povs, ~.+strata(region)+strata(agency))
summary(mod.cox.main)
cox.zph(mod.cox.main)
#INTERACTIONS
#PROGRAM X AGE
cox.int.age <- update(mod.cox.main, ~.+program*age)
summary(cox.int.age)
anova(mod.cox.main, cox.int.age)
anova(mod.cox.main, cox.int.age, test = "Chisq")
#PROGRAM X OUTOFHOME
cox.int.out <- update(mod.cox.main, ~.+program*outofhome)
summary(cox.int.out)
anova(mod.cox.main, cox.int.out)
anova(mod.cox.main, cox.int.out, test = "Chisq")
#PROGRAM X SEX* 
cox.int.sex <- update(mod.cox.main, ~.+program*sex)
summary(cox.int.sex)
anova(mod.cox.main, cox.int.sex)
anova(mod.cox.main, cox.int.sex, test = "Chisq")
#PROGRAM X MINORITY
cox.int.min <- update(mod.cox.main, ~.+program*minority)
summary(cox.int.min)
anova(mod.cox.main, cox.int.min)
anova(mod.cox.main, cox.int.min, test = "Chisq")
#PROGRAM X SUBST.ABUSE
cox.int.subst <- update(mod.cox.main, ~.+program*subst.abuse)
summary(cox.int.subst)
anova(mod.cox.main,cox.int.subst)
anova(mod.cox.main, cox.int.subst, test = "Chisq")
#PROGRAM X CRIM.HIST
cox.int.crim <- update(mod.cox.main, ~.+program*crim.hist)
summary(cox.int.crim)
anova(mod.cox.main,cox.int.crim)
anova(mod.cox.main, cox.int.crim, test = "Chisq")
#PROGRAM X FIRST
cox.int.first <- update(mod.cox.main, ~.+program*first)
summary(cox.int.first)
anova(mod.cox.main,cox.int.first)
anova(mod.cox.main, cox.int.first, test = "Chisq")

tbl_regression(mod.cox.main, exponentiate = T)%>%
  modify_caption("Table 4: Time-Dependent Cox Model Summary<br>Stratified by Poverty, Region, and Agency")


newdat <- data.frame(program=levels(dat.new$program), outofhome = 0, age = 0, minority = 0, subst.abuse = 0, chrim.hist = 0, first = 0)
survfit(abuse.surv~program, data = newdat)

#SURVIVAL & CUMHAZ PLOTS BY PROGRAM
ggsurvplot(progfit, data=dat.new, ggtheme = theme_bw())
ggsurvplot(progfit, data=dat.new, ggtheme = theme_bw(), fun = "cumhaz",
           axis.title = element_text(size=14),
           axis.text = element_text(size = 12)
)