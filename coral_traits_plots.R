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

## ==================
# Combine all traits togethers
## ==================

names(calc_rate4)
names(col_fecund3)
names(growth_rate3)
names(growth_rate3[,1:5])
names(max_diam2[,1:5])
names(polyp_fecund3)
names(swim_speed3)
names(sym_dens3)
names(tis_thick3)

data <- bind_rows(calc_rate4, col_fecund3, growth_rate3[,-c(6:7)],
                  max_diam2[,-c(6:7)], polyp_fecund3, swim_speed3, sym_dens3, tis_thick3) 
head(data)

unique(data$trait_name)
unique(data$standard_unit)

levels(as.factor(data$specie_name))


## ==================
# Bind in life history, growth form
## ==================
head(growth_form2)
growth_form3 <- growth_form2[,c(1:2,5)]
names(growth_form3)[3] <- "growth_form"
str(growth_form3)

head(lh2)
lh3 <- lh2[,c(1:2,5)]
names(lh3)[3] <- "life_history"

data2 <- left_join(data, growth_form3)
data2 <- left_join(data2, lh3)
head(data2)


## ==================
# Calculate species averages for traits
## ==================
head(data2)

d <- data2 %>% 
  select(specie_name,life_history,growth_form,trait_name,standard_unit,value) %>% 
  group_by(specie_name,life_history,growth_form,trait_name,standard_unit) %>%
  summarize(mean = mean(value), n = n())
head(d)

write.csv(d, "species_avg_traits.csv", row.names = FALSE)

#how to code and remove outliers? 

d$life_history[is.na(d$life_history)] <- "n/a"
head(d$life_history)
head(d)

levels(as.factor(d$life_history))
d$life_history <- factor(d$life_history, 
                         levels = c("competitive","generalist","stress-tolerant",
                                    "weedy","n/a"))
colours <- c("red","darkblue","blue","limegreen","grey50")

#remove a few outliers

#box plot of traits by life histories
ggplot(data = d, aes(x = life_history, y = mean)) +
  geom_point(aes(colour = life_history), shape = 21, alpha = 0.5) +
  geom_boxplot(aes(colour = life_history)) +
  facet_wrap(~trait_name, scales="free_y") +
  scale_colour_manual(values = colours) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("traits x life history boxplots_draft.pdf", height = 4.5, width = 6)

#corrplot of traits - tradeoffs??
head(d)
d_wide <- dcast(d, specie_name ~ trait_name, value.var = "mean")
head(d_wide)
nrow(d_wide)

row.names(d_wide) <- d_wide$specie_name
d_wide <- d_wide[,-1]
head(d_wide)

d_cor <- cor(d_wide, use = "pairwise.complete.obs")
head(d_cor)

corrplot(d_cor, diag = FALSE)



