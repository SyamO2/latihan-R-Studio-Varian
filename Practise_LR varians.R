# prepare data set:
getwd()

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(nortest)
library(agricolae)
library(DescTools)
library(xlsx)
library(rstatix)
library(car)
data<-read.table('gudhiv.dat', header = TRUE, na.strings = 'X')

# Data ini merupakan Analisis data penyakit HIV & faktor risikonya yang berhubungan, pada subjek
# pria dewasa yang tinggal di Gambia (n=435 laki-laki)

# Setup variable and input the definition
names(data)<- base::tolower(names(data))
print(data)
head(data)
# rename variables
install.packages('expss')
library(expss)
var_lab(data$married) = 'marital status'
var_lab(data$gambian) = 'gambian (urban vs rural'
var_lab(data$gud)     = 'hist. of syphilis'
var_lab(data$utigc)   = 'hist. uretral discharge'
var_lab(data$cir)     = 'circumcised'
var_lab(data$travout) ='traveller'
var_lab(data$sexpro)  = 'hist. of sexual w prostitute'
var_lab(data$injl2m)  = 'injection within prev 12m'
var_lab(data$partners)= 'number of sexual partner'
var_lab(data$hiv)     = 'serologycal test (positive vs negative)'
head(data)

# insert value label
data$married      <- factor(data$married, levels = c(0,1), labels = c('single','married'))                                                   
data$gambian      <- factor(data$gambian, levels = c(0,1), labels = c('rural','urban')) 
data$gud          <- factor(data$gud,     levels = c(0,1), labels = c('not-gud','gud'))
data$utigc        <- factor(data$utigc,   levels = c(0,1), labels = c('not-utigc','utigc'))
data$cir          <- factor(data$cir,     levels = c(0,1), labels = c('not-circumsized','circumsize'))
data$travout      <- factor(data$travout, levels = c(0,1), labels = c('not-travel','traveller'))
data$sexpro       <- factor(data$sexpro,  levels = c(0,1), labels = c('not-use','used')) 
data$inj12m       <- factor(data$inj12m,  levels = c(0,1), labels = c('not-injected','injected'))
data$hiv          <- factor(data$hiv,     levels = c(0,1), labels = c('negative','positive'))

# if the name of variable gambian changed into residence:
names(data) [names(data) == 'gambian'] <- 'residence'
head(data)

# Descriptive Statistics Analysis
install.packages('epiDisplay')
install.packages('readxl')
install.packages('kableExtra')
install.packages('epicalc', repos = 'https://medipe.psu.ac.th/epicalc')

library(epiDisplay)
library(readxl)
library(kableExtra)
library(epicalc)
codebook(data)

# Tabulasi Silang - Cross Tabulation::
# Performing cross-tabulation for assessing the association of each predictors candidate
install.packages('gmodels')
library(gmodels)

# 1) The associations of marital ststus and HIV
CrossTable(data$married, data$hiv, digits = 3, max.width = 5, expected = TRUE,
           prop.c = FALSE, prop.r = TRUE, prop.t = FALSE, prop.chisq = TRUE, fisher = TRUE,
           chisq = TRUE, format = 'SPSS')
# In other way to show the cross-tabulation using total row by group for each category (since we apply cross-sectional study):
install.packages('misty')
library(misty)
crosstab(data[,c('married', 'hiv')], print = 'row')
crosstab(data[,c('married', 'hiv')], print = 'col') # if we more consider case-control study

# how to assess the magnitude associations by the odds-ratio
library(epiDisplay)
cc(data$married, data$hiv)
(321*8)/(13*93)
# Conclusion: marital status is the candidate of the risk factors of HIV (p<0.25)

# Performing simple logistic regression:
lr_marital <- glm(data$hiv ~ data$married, data = data, family = 'binomial')
summary(lr_marital)
library(epiDisplay)
epiDisplay::logistic.display(lr_marital)