# Name: Hisham
# Date: 2024
# Project name: acupuncture BMJ

# - Project Description: 
# Acupuncture for chronic headache in primary care: large, pragmatic, RCT
# - Aim: detect and quantify effect of regular acupuncture treatment on headache
# - Hypothesis: there is a decreased headache score after 12 months
#               in treatment group with acupuncture sessions for 3 months 
#               compared to control group without acupuncture
Sys.setenv(LANG = "en") 

# reset R session
rm(list = ls()) # remove all objects that have been generated before
layout(matrix(1)) # reset plotting environment to display one plot

# install.packages("openxlsx")
install.packages('openxlsx')
library(openxlsx) # allows to read and write xlsx files

# install.packages("tableone")
install.packages('tableone')
library(tableone) # tables for description of baseline characteristics

dat.raw <- read.xlsx(xlsxFile = "data/data_BMJ.xlsx", sheet = "Data")
class(dat.raw)
nrow(dat.raw)
ncol(dat.raw)

head(dat.raw)
tail(dat.raw)
range(dat.raw$pk5, na.rm = TRUE)

# select variables: only the necessary ones for analysis of the primary endpoint
dat <- dat.raw[, c("id", "age", "sex", "migraine", "chronicity",
                   "acupuncturist","group", "pk1", "pk2", "pk5",
                   "completer", "withdrawal_reason")]
colnames(dat)

dat <- dat.raw[, c("id", "age", "sex", "migraine", "chronicity",
                   "acupuncturist","group", "pk1", "pk2", "pk5",
                   "completer", "withdrawal_reason")]
dat$withdrawal_reason <- factor(dat$withdrawal_reason)
dat$group <- factor(dat$group, levels = c(1, 0), labels = c("acupuncture", "control"))
dat$migraine <- factor(dat$migraine, levels = c(0, 1), labels = c("NO", "YES"))
dat$completer <- factor(dat$completer, levels = c(0, 1), labels = c("NO", "YES"))
dat$sex <- factor(dat$sex, levels = c(0, 1), labels = c("male", "female"))

summary(dat$completer)
dat = dat[dat$completer == 'YES' & dat$withdrawal_reason== 'no reason',]


##    Hypothetical: In other studies it was found that the headache score 
##    of patients below 21 years is not always reliable. 
##    So we would like to do a second analysis with only patients that are 
##    at least 21 years old. For this analysis we need a special dataset. 
##    Please create this dataset. It should be named dat.atleast21.
##    How many patients are in this dataset?
dat.atleast21 <- dat[dat$age >= 21, ]
nrow(dat.atleast21)

##    look at all the variables at once and to check, 
##    whether the ranges of the numeric variables are plausible.
summary(dat)

tab1 <- CreateTableOne(vars = c("age", "sex", "migraine", "chronicity"),
                       strata ='group',
                       test = FALSE,
                       data = dat)
tab1.print <- print(tab1, nonnormal = c("chronicity"))
class(tab1)
class(tab1.print)
?print.TableOne
# save the created table 1 as csv file 
write.csv2(x = tab1.print, file = "tables/tab1.csv", row.names = TRUE)

install.packages("beeswarm")
library(beeswarm) ## nice addition for boxplots

install.packages("Epi")
library(Epi) ## tables for binary data

dat$pkchange <- (dat$pk1 - dat$pk5) / dat$pk1 * 100

dat$responder <- ifelse(test = dat$pkchange > 35,
                        yes = "responder", no = "non-responder")
dat$responder <- factor(dat$responder,
                        levels = c("responder", "non-responder"))

# or in one step
dat$responder <- factor(ifelse(test = dat$pkchange > 35,
                               yes = "responder", no = "non-responder"),
                        levels = c("responder", "non-responder"))

dat$responder <- factor(dat$pkchange > 35, levels = c(TRUE, FALSE),
                        labels = c("responder", "non-responder"))

# check if dataset is correct
summary(dat$withdrawal_reason)
summary(dat$pkchange)
summary(dat$responder)
summary(dat$pkchange[dat$responder == "responder"])
summary(dat$pkchange[dat$responder == "non-responder"])

dim(dat)
str(dat)

# create responder3m variable
dat$pkchange3m <- (dat$pk1 - dat$pk2) / dat$pk1 * 100
dat$responder3m <- factor(ifelse(test = dat$pkchange3m > 35,
                                 yes = "responder", no = "non-responder"),
                          levels = c("responder", "non-responder"))

# check for missing values
summary(dat$responder3m)
summary(dat$responder)
# the missing values are shown as an additional category "NA's"

# compare original variables to see where NA's come from
summary(dat$pk1)
summary(dat$pk2)
summary(dat$pk5)
# pk2 has NA values inside
mean(dat$pk1)
mean(dat$pk2)
mean(dat$pk5)

# the mean of pk2 is not calculated. Instead the output is NA, so missing.
CreateTableOne(vars = c("pk1", "pk2", "pk5"), data = dat)
# missing values are handled differently with different functions

# look at variable with missing values
head(dat$pk2)

# the first value is missing
# in excel, the cell was empty
# when importing data into R, empty cells are automatically replaced by NA

# detection of missing values with is.na function
is.na(dat$pk2)
summary(is.na(dat$pk2))

# remove missing values from vector
summary(dat$pk2[!is.na(dat$pk2)])

# apply mean function on vector without missing values
mean(dat$pk2[!is.na(dat$pk2)])
summary(dat$pk2)
# gives the same mean value
# alternative way to remove missing values from analysis
mean(dat$pk2, na.rm = TRUE)

# best reporting of "responder3m" variable:
# either do complete case analysis (ignoring missing values) and report missing values
# or impute missing values and do analysis including imputed values

# example for reporting of missing values with CreateTableOne function:
tabpk <- CreateTableOne(vars = c("responder", "responder3m"), strata = "group", 
                        data = dat, test = FALSE)
print.tabpk <- print(tabpk, missing = TRUE)
# shows missing values in percent in last column

write.csv2(print.tabpk, file = "tables/tableresp.csv")

## repeat analysis with dplyr
library(dplyr)
dat.dplyr <- dat.raw %>% 
  select(id, age, sex, migraine, chronicity, acupuncturist,
         group, pk1, pk2, pk5, completer, withdrawal_reason) %>%
  mutate(withdrawal_reason = factor(withdrawal_reason),
         group = factor(group, levels = c(1, 0), labels = c("acupuncture", "control")),
         migraine = factor(migraine, levels = c(0, 1), labels = c("no migraine", "migraine")),
         completer = factor(completer, levels = c(0, 1), labels = c("NO", "YES")),
         sex = factor(sex, levels = c(0, 1), labels = c("male", "female"))) %>%
  filter(completer == "YES") %>%
  mutate(pkchange = (pk1 - pk5) / pk1 * 100,
         responder = factor(ifelse(test = pkchange > 35,
                                   yes = "responder", no = "non-responder"),
                            levels = c("responder", "non-responder")))
# 1. Histogram ----------------------------------------------------------------
hist(x = dat$pk5[dat$group== "acupuncture"], xlab= 'Headache score at 12 Months', main = 'Histogram', xlim = c(0,100), col=rgb(red=0, green = 0, blue= 1, alpha = 0.3) , breaks = 20)

hist(x = dat$pk5[dat$group == "control"], xlim = c(0, 100), breaks = 20, add = TRUE,
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3))

legend("topright", title = "Treatment group",
       legend = c("acupuncture", "control"),
       fill = c(rgb(0,0,1,0.3), rgb(1, 0, 0, .3)),
       bty = "n")

pdf("./figures/hitogram.pdf")
dev.off()

sqrt_pk5 <- sqrt(dat$pk5)
lines(density(sqrt_pk5), col = "black", lwd = 4)

# 2. Boxplot ------------------------------------------------------------------
boxplot(pk5 ~ group, data = dat,
        xlab = "Treatment group",
        ylab = "Headache score 12 Months",
        main = "2. Boxplot",
        ylim = c(0, 100),
        col = c("blue", "red"),
        outline = FALSE)
beeswarm(pk5 ~ group, data = dat, add = TRUE,
         pch = 20, cex = .8)
png("./figures/boxplot.png")
dev.off()

# 3. Scatterplot -------------------------------------------------------------
plot(pk5 ~ pk1, data = dat, type = "p",
     main = "Scatterplot",
     xlab = "Headache Score Baseline",
     ylab = "Headache Score 12 Months",
     xlim = c(0,100), ylim = c(0,100),
     pch = 20,
     col = c("blue", "red")[group])
abline(a = 0, b = 1)


##add legend to colors. 

legend("bottomright",
       title = "Treatment group",
       legend = levels(dat$group),
       col = c("blue", "red"),
       pch = 20)

jpeg("./figures/scatterplot.jpeg")
dev.off()
(pty= 'square')

# 4. Lines plot -------------------------------------------------------------
par(mfrow = c(1, 2))
matplot(t(dat[dat$group == "acupuncture", c("pk1", "pk5")]),
        type = "b", pch = 20,
        xlab = "Time point", ylab = "Headache score",
        ylim = c(0, 100),
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3), lty = 1,
        xaxp = c(1, 2, 1),
        main = "4. Acupuncture")
matplot(t(dat[dat$group == "control", c("pk1", "pk5")]),
        type = "b", pch = 20,
        xlab = "Time point", ylab = "Headache score ",
        ylim = c(0, 100),
        col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3), lty = 1,
        xaxp = c(1, 2, 1),
        main = "Control")
par(mfrow = c(1, 1))

tiff("./figures/lineplot.tiff")
dev.off()


#reproduce analysis with ggplot
install.packages("ggbeeswarm")
library(ggplot2)
library(ggbeeswarm)

#Boxplot
ggplot(data = dat, mapping = aes(x = group, y = pk5, color = group)) + 
  geom_boxplot(outlier.color = NA) + 
  geom_beeswarm(cex = 2) +
  facet_grid(~ migraine) + # facet plot by migraine
  labs(x = "", y = "Headache Score 12 Months")

png("./figures/box2.png")
dev.off()

# Scatterplot
ggplot(data = dat, mapping = aes(x = pk1, y = pk5, color = group)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 100)) +
  theme(aspect.ratio = 1) +
  facet_grid(sex ~ migraine) + # facet plot by sex on y-axis and migraine on x-axis
  labs(x = "Headache Score Baseline", y = "Headache Score 12 Months")
png("./figures/scatter2.png")
dev.off()

#What is the null hypothesis for the primary outcome of the acupuncture study?
#  no difference between acunpture and control in relieving headache

## check for normality
hist(dat$pk5, breaks=20)
boxplot(dat$pk5)

##    The best way to investigate whether the normality assumption is met
##    is to look at a Quantile-Quantile plot.

qqnorm(dat$pk5, pch = 20, main = "Normal Q-Q Plot")
qqline(dat$pk5, col = "red") 


## transform the data then Check afterwards if the situatuin changed
sqrt_pk5 <- sqrt(dat$pk5)
qqnorm(sqrt_pk5, pch = 20, main = "Normal Q-Q Plot for Square-root Transformed pk5")
qqline(sqrt_pk5, col = "red")

## 2 sample t test on transformed data
t_test_result <- t.test(sqrt_pk5 ~ dat$group)
t_test_result

## check a similar shape of the distributions of the two groups
hist(dat$pk5[dat$group == "acupuncture"], main = "Histogram of pk5 for Acupuncture Group", xlab = "pk5", col = "lightblue")
hist(dat$pk5[dat$group == "control"], main = "Histogram of pk5 for Control Group", xlab = "pk5", col = "lightgreen")

## Wilcoxon test
wilcox_test_result <- wilcox.test(pk5 ~ group, data = dat)
wilcox_test_result


##check whether the headache score in the control group changed from baseline to 12 months.

# Calculate the differences
control_group <- dat[dat$group == "control", ]
differences <- control_group$pk5 - control_group$pk1

# Q-Q plot for the differences
qqnorm(differences, pch = 20, main = "Normal Q-Q Plot for Differences")
qqline(differences, col = "red")

paired_t_test_result <- t.test(control_group$pk1, control_group$pk5, paired = TRUE)
paired_t_test_result


## compare the treatment groups wrt. responder

responder_table <- table(dat$group, dat$responder)
chi_squared_test_result <- chisq.test(responder_table)
chi_squared_test_result

fisher_test_result <- fisher.test(responder_table)
fisher_test_result

## Summarizing the association of the response and the treatment
out.responder <- twoby2(exposure = dat$group, outcome = dat$responder)
out.responder


str(out.responder)

out.responder$table

write.xlsx(as.data.frame(out.responder$table), file = "tables/tab_responder.xlsx", 
           rowNames = TRUE)


