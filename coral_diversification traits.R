## ==================
##  Load Libraries
## ==================

library(dplyr)
library(knitr)
library(reshape2)
library(ggplot2)
library(gdata)
library(tools)
library(corrplot)
library(car)

## ==================
##  Load Traits
## ==================

#diameter - 320 species
max_diam <- read.csv("https://coraltraits.org/traits/90.csv", as.is=TRUE)
max_diam2 <- max_diam[,c("specie_id","specie_name","trait_name",
                         "standard_unit","value")]
unique(max_diam2$specie_name)


#convert cm to m to mm
unique(max_diam2$standard_unit)
#10 mm in a cm
#1 cm = 0.01 m
max_diam2$orig.value <- max_diam2$value
max_diam2$orig.unit <- max_diam2$standard_unit

max_diam2$new_value <- ifelse(max_diam2$standard_unit == "mm", max_diam2$value * 0.1, 
                              ifelse(max_diam2$standard_unit == "m", max_diam2$value * 100,
                                     max_diam2$value))
max_diam2$value <- max_diam2$new_value
max_diam2$standard_unit <- "cm"
unique(max_diam2$standard_unit)
max_diam2 <- subset(max_diam2, trait_name == "Colony maximum diameter")
str(max_diam2)

#polyp fecundity - 10 species
polyp_fecund <- read.csv("https://coraltraits.org/traits/12.csv", as.is=TRUE)
polyp_fecund2 <- polyp_fecund[,c("specie_id","specie_name","trait_name",
                                 "standard_unit","value")]
unique(polyp_fecund2$trait_name)
unique(polyp_fecund2$standard_unit)

polyp_fecund3 <- subset(polyp_fecund2, trait_name == "Polyp fecundity")
unique(polyp_fecund3$specie_name)
unique(polyp_fecund3$standard_unit)
polyp_fecund3$value <- as.numeric(polyp_fecund3$value)
str(polyp_fecund3)

#colony fecundity - only 6 species
col_fecund <- read.csv("https://coraltraits.org/traits/216.csv", as.is=TRUE)
col_fecund2 <- col_fecund[,c("specie_id","specie_name","trait_name",
                             "standard_unit","value")]
col_fecund3 <- subset(col_fecund2, trait_name == "Colony fecundity")
unique(col_fecund3$standard_unit)
unique(col_fecund3$specie_name)

col_fecund3$value <- as.numeric(col_fecund3$value)
str(col_fecund3)

#growth and calcification
growth_rate <- read.csv("https://coraltraits.org/traits/60.csv", as.is=TRUE)
growth_rate2 <- growth_rate[,c("specie_id","specie_name","trait_name",
                               "standard_unit","value")]
growth_rate3 <- subset(growth_rate2, trait_name == "Growth rate")

#growth rate - 130 species with common units
#UNITS!!
unique(growth_rate3$standard_unit)
growth_rate3$value <- as.numeric(growth_rate3$value)
growth_rate3$orig.value <- growth_rate3$value
growth_rate3$orig.unit <- growth_rate3$standard_unit

growth_rate3$new_unit <-ifelse(growth_rate3$standard_unit=="mm yr^-1", 
                               growth_rate3$value / 12,
                               ifelse(growth_rate3$standard_unit=="mm d^-1", 
                                      growth_rate3$value * 30, 
                                      ifelse(growth_rate3$standard_unit=="mm per 7 months^-1",
                                             growth_rate3$value / 7,
                                             ifelse(growth_rate3$standard_unit=="mm per 4 months^-1",
                                                    growth_rate3$value / 4,
                                                    ifelse(growth_rate3$standard_unit=="mm per 3 months^-1",
                                                           growth_rate3$value / 3,
                                                           ifelse(growth_rate3$standard_unit=="mm per 8 months^-1",
                                                                  growth_rate3$value / 8,
                                                                  ifelse(growth_rate3$standard_unit=="mm per 6 months^-1",
                                                                         growth_rate3$value / 6, 
                                                                         growth_rate3$value)))))))
hist(growth_rate3$new_unit)                                    
growth_rate3$value <- growth_rate3$new_unit
growth_rate3$standard_unit <- "mm month^-1"

unique(growth_rate3$specie_name)
subset(growth_rate3, value > 20)

growth_rate3$value <- as.numeric(growth_rate3$value)
str(growth_rate3)

#19 species with g/cm2/year units
calc_rate <- read.csv("https://coraltraits.org/traits/127.csv", as.is=TRUE)
calc_rate2 <- calc_rate[,c("specie_id","specie_name","trait_name",
                           "standard_unit","value")]
calc_rate3 <- subset(calc_rate2, trait_name == "Calcification rate")

unique(calc_rate3$standard_unit)
calc_rate4 <- subset(calc_rate2, standard_unit == "g cm ^-2 yr^-1")
unique(calc_rate4$specie_name)

calc_rate4$value <- as.numeric(calc_rate4$value)
str(calc_rate4)
#remove outlier
hist(calc_rate4$value)

#swimming speed - 13 species, mm/second
swim_speed <- read.csv("https://coraltraits.org/traits/169.csv", as.is=TRUE)
swim_speed2 <- swim_speed[,c("specie_id","specie_name","trait_name",
                             "standard_unit","value")]
swim_speed3 <- subset(swim_speed2, trait_name == "Larval swimming speed")
unique(swim_speed3$standard_unit)
unique(swim_speed3$specie_name)

swim_speed3$value <- as.numeric(swim_speed3$value)
str(swim_speed3)

#tissue thickness - mm - 16 species
tis_thick <- read.csv("https://coraltraits.org/traits/132.csv", as.is=TRUE)
tis_thick2 <- tis_thick[,c("specie_id","specie_name","trait_name",
                           "standard_unit","value")]
unique(tis_thick2$trait_name)
tis_thick3 <- subset(tis_thick2, trait_name == "Tissue thickness")
unique(tis_thick3$standard_unit)
unique(tis_thick3$specie_name)

tis_thick3$value <- as.numeric(tis_thick3$value)
str(tis_thick3)

#symbiodinium density - 35 species - units/cm2
sym_dens <- read.csv("https://coraltraits.org/traits/135.csv", as.is=TRUE)
sym_dens2 <- sym_dens[,c("specie_id","specie_name","trait_name",
                         "standard_unit","value")]
unique(sym_dens2$trait_name)
sym_dens3 <- subset(sym_dens2, trait_name == "Symbiodinium density")
unique(sym_dens3$standard_unit)
unique(sym_dens3$specie_name)

sym_dens3$value <- as.numeric(sym_dens3$value)
str(sym_dens3)

#growth form - 857 species
growth_form <- read.csv("https://coraltraits.org/traits/183.csv", as.is=TRUE)
growth_form2 <- growth_form[,c("specie_id","specie_name","trait_name",
                               "standard_unit","value")]
unique(growth_form2$trait_name)

unique(growth_form2$standard_unit)
unique(growth_form2$specie_name)
unique(growth_form2$value)

#life history - 143 species
lh <- read.csv("https://coraltraits.org/traits/233.csv", as.is=TRUE)
lh2 <- lh[,c("specie_id","specie_name","trait_name",
             "standard_unit","value")]
unique(lh2$trait_name)
unique(lh2$value)
unique(lh2$specie_name)


#skeletal density - 61
#54 species, not bad
skel_dens <- read.csv("https://coraltraits.org/traits/61.csv", as.is=TRUE)
skel_dens2 <- skel_dens[,c("specie_id","specie_name","trait_name",
                           "standard_unit","value")]
unique(skel_dens2$trait_name)

skel_dens3 <- subset(skel_dens2, trait_name == "Skeletal density")
#cm3 = ml, all units are good
unique(skel_dens3$standard_unit)
unique(skel_dens3$specie_name)

#recode skel dens unit because equal
skel_dens3$standard_unit <- recode(skel_dens3$standard_unit, 
                                   "'g mL^-1' = 'g cm-3'")
unique(skel_dens3$standard_unit)
skel_dens3$value <- as.numeric(skel_dens3$value)
str(skel_dens3)

#brood/spawn - 5
rep_mode <- read.csv("https://coraltraits.org/traits/5.csv", as.is=TRUE)
rep_mode2 <- rep_mode[,c("specie_id","specie_name","trait_name",
                         "standard_unit","value")]
unique(rep_mode2$trait_name)
unique(rep_mode2$value)

#all species are either brooder OR spawner - no 'both' records
head(rep_mode2)
rep_mode3 <- rep_mode2 %>% 
  group_by(specie_id, specie_name,trait_name, value) %>% 
  summarize(n = n())
unique(rep_mode2$specie_name)

#any duplicates? nope, good.
rep_mode4 <- rep_mode3 %>% 
  group_by(specie_id, specie_name) %>% 
  summarize(n = n())

#coloniality - 104
coloniality <- read.csv("https://coraltraits.org/traits/104.csv", as.is=TRUE)
coloniality2 <- coloniality[,c("specie_id","specie_name","trait_name",
                               "standard_unit","value")]
unique(coloniality2$trait_name)
unique(coloniality2$value)
unique(coloniality2$specie_name)

coloniality3 <- coloniality2 %>% 
  group_by(specie_id, specie_name,trait_name, value) %>% 
  summarize(n = n())

subset(coloniality3, value == "both")
subset(coloniality3, value == "solitary")

## ==================
# Combine all continuous traits togethers
## ==================

names(calc_rate4)
names(col_fecund3)
names(growth_rate3[,1:5])
names(max_diam2[,1:5])
names(polyp_fecund3)
names(swim_speed3)
names(sym_dens3)
names(tis_thick3)
names(skel_dens3)

data <- bind_rows(calc_rate4, col_fecund3, growth_rate3[,1:5],
                  max_diam2[,1:5], polyp_fecund3, swim_speed3, sym_dens3, 
                  tis_thick3,skel_dens3) 
head(data)
names(data)

unique(data$trait_name)
unique(data$standard_unit)

levels(as.factor(data$specie_name))


## ==================
# Bind in life history, growth form, coloniality, reproductive mode
## ==================
head(growth_form2)
growth_form3 <- growth_form2[,c(1:2,5)]
names(growth_form3)[3] <- "growth_form"
str(growth_form3)

head(lh2)
lh3 <- lh2[,c(1:2,5)]
names(lh3)[3] <- "life_history"

head(rep_mode3)
rep_mode4 <- rep_mode3[,c(1:2,4)]
names(rep_mode4)[3] <- "larval_development"

head(coloniality3)
coloniality4 <- coloniality3[,c(1:2,4)]
names(coloniality4)[3] <- "coloniality"

data2 <- left_join(data, growth_form3)
data2 <- left_join(data2, lh3)
data2 <- left_join(data2, rep_mode4)
data2 <- left_join(data2, coloniality4)
head(data2)

unique(data2$trait_name)

data3 <- data2 %>% 
  group_by(specie_id, specie_name, growth_form, life_history, 
           larval_development, coloniality, trait_name, standard_unit) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))
names(data3)

unique(data3$standard_unit)

data4 <- dcast(data3, specie_id+specie_name+growth_form+
                 life_history+larval_development+coloniality ~ trait_name+standard_unit)
head(data4)
names(data4)

#select to mouillot traits
data5 <- data4[,-c(7,8,11,12,14,15)]
names(data5)
head(data5)

data6 <- data5 %>% 
  arrange(specie_name)

#recode growth_form to simple ELE 2012 classes
unique(data6$growth_form)
data6$growth_form <- recode(data6$growth_form, 
                            "'branching_open' = 'branching';
                            'branching_closed' = 'branching';
                            'corymbose' = 'branching';
                            'hispidose' = 'branching';
                            'digitate' = 'branching';
                            'laminar' = 'tables_or_plates';
                            'columnar' = 'branching';
                            'encrusting_long_uprights' = 'encrusting'")

data6$growth_form <- recode(data6$growth_form, 
                            "'encrusting' = 'domed';
                            'submassive' = 'domed';
                            'massive' = 'domed'")
unique(data6$growth_form)
head(data6)

names(data6)
data6 <- data6[,c(1:3,5:8,4)]

setwd("/Users/emilydarling/Dropbox/1-On the go/Co-author papers/Mouillot_coral FD PD diversification")
write.csv(data6, "Scleractinia traits_30June2016.csv", row.names = FALSE)


## ==================
# Quick check for any errors
## ==================
names(data6)

hist(data6$`Colony maximum diameter_cm`)
hist(data6$`Skeletal density_g cm-3`)
hist(data6$`Growth rate_mm month^-1`)

subset(data6, `Growth rate_mm month^-1` > 10)


