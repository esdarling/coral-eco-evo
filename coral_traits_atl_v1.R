## ==================
##  Load Libraries
## ==================

library(ggplot2)
library(reshape2)
library(gdata)

## ==================
##  Load Traits
## ==================

setwd("~/Dropbox/Projects/In Progress/Adaptation Portfolios/Code/coraltraits")
traits<-read.csv("ctdb_1.1.1_data.csv")
darling<-read.csv("darling_corals_atl.csv")

## ==================
##  List of all Traits
## ==================

sort(unique(traits$trait_name))


## ==================
##  Subset out Atlantic Species and traits from Darling et al. 
## ==================

atlsp<-drop.levels(subset(traits,trait_name=="Ocean basin"))
atlsp<-drop.levels(subset(atlsp,value=="atlantic"))
atlsp<-unique(atlsp$specie_name)

#total number of species
length(unique(traits$specie_name))

traits_atl<-subset(traits,specie_name %in% c(as.character(darling$species2)))

#show there traits for all 32 of Darling et al species
length(unique(traits_atl$specie_name))

## ==================
##  Subset out Focal Traits
## ==================

#107 traits
all_traits<-unique(traits_atl$trait_name)

as.character(all_traits[13:14])

#Darling Had slightly differnt ones or the naming is changed? 
#consider adding max and min depth
#FECUNDITY: eggs per poplyp
#lipid content
#symbiodinium diversity

focal_traits<-c(
                "Colony area",
                "Colony maximum diameter",
                "Corallite width maximum",
                "Depth lower",
                "Depth upper",
                "Polyp fecundity",
                "Eggs per area",
                "Generation time",
                "Growth rate",
                "Calcification rate",
                "Skeletal density", 
                "Polyps per area",
			        	"Larval swimming speed",
                "Protein biomass",
                "Lipid content",
                "Tissue thickness",
                "Symbiodinium density",
                "Symbiodinium sp. in propagules",
                "Irradiance",
                "Water temperature",
                "Total biomass",
                "Growth form",
                "Life history strategy"
                )



#with the exception of symbiont diversity these are Darling Traits
                
focal_traitsED<-c(
                "Colony area",
                "Colony maximum diameter",
                "Corallite width maximum",
                "Depth lower",
                "Depth upper",
                "Polyp fecundity",
                "Eggs per area",
                "Generation time",
                "Growth rate",
                "Calcification rate",
                "Skeletal density"
                                )

#look at specific trait
drop.levels(subset(traits_atl,trait_name==all_traits[64])$value)

#subset out all focal traits
traits_atl2<-drop.levels(subset(traits_atl,trait_name %in% focal_traitsED)) #plug in whatever trait list you want


#subset just values out
traits_atl3<-traits_atl2[,c(5,14,20)]
traits_atl3$value<-as.numeric(as.character(traits_atl3$value))

trait_tab<-as.matrix(tapply(traits_atl3$value,list(traits_atl3$specie_name,traits_atl3$trait_name),mean))

pairs(trait_tab)

sort(focal_traitsED)

colnames(trait_tab)<-c("CalcificationRate","ColonyArea","ColonyMaximumDiameter",
					   "CoralliteWidthMaximum","DepthLower","DepthUpper","GrowthRate","SkeltalDensity")
ggpairs(trait_tab)


#w <- reshape(traits_atl3, 
#             timevar   = "trait_name",
#             idvar     = c("specie_id","specie_name","trait_id","trait_class",
#             "standard_unit","methodology_id","value"),
#             direction = "wide")


















