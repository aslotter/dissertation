#### Basics ####

library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)
library(Morpho)
library(MASS)
#loading libraries

setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation")
#setting the working directory 

#### Landmark Data ####
landmarkdata <- readland.tps("maxillarymolars_joined.tps", specID = ("imageID"))

sliders <- define.sliders(9:48, nsliders = 40)

#### GPA ####

gpa <- gpagen(landmarkdata, PrinAxes = TRUE, curves = sliders)
#GPA for joined

specimeninfo <- data.frame(
  Specimen = c(rep("M1", 12), rep("M2", 14), rep("M3", 20), rep(NA, 9)),
  stringsAsFactors = TRUE
)
#creating a list of what each specimen is (M1, M2, M3, or unknown)

known <- which(!is.na(specimeninfo$Specimen))
unknown <- which(is.na(specimeninfo$Specimen))
#identifying which species in the joined list are known vs. unknown

#### Known PCA ####
knownpca_results <- gm.prcomp(gpa$coords[,, known])
knownpca_results

known_custom_labels <- c("KNM-ER 20427", "KNM-ER 30745", "AL 199-1", "AL 200-1a",
                         "AL 200-1a", "AL 486-1", "KNM-ER 30200a", "KNM-KP 35842",
                         "KNM-WT 38337", "ARI-VP-1/484", "ARI-VP-3/659", "BRT-VP-3/1",
                         "KNM-ER 35235", "AL 199-1", "AL 200-1a (left)", "AL 200-1a (right)",
                         "AL 417d", "AL 486-1", "KNM-ER 30200", "ARI-VP-3/435", 
                         "ARI-VP-3/80", "BDU-VP-1/3", "BDU-VP-1/50", "BRT-VP-3/1",
                         "LLG-VP-3/77", "MSD-VP-1/53",
                         "KNM-ER 20421", "KNM-ER 35236", "GLL 1666", "GLL 1717",
                         "GLL 1800", "AL 161-40", "AL 199-1", "AL 200-1a (left)", 
                         "AL 200-1a (right)", "AL 333x-1", "AL 417d (left)", "AL 417d (right)",
                         "AL 486-1", "KNM-KP 58761", "KNM-WT 16003", "KNM-WT 77329f",
                         "ARI-VP-1/215", "ARI-VP-1/90", "MSD-VP-3/58c", "MSD-VP-3/58d")

#define groups for coloring points
known_groups <- c("A. anamensis", "A. anamensis", "A. afarensis", "A. afarensis", 
                  "A. afarensis", "A. afarensis", "A. anamensis", "A. anamensis",
                  "Lomekwi", "WORMIL", "WORMIL", "A. deyiremeda",
                  "A. anamensis", "A. afarensis", "A. afarensis", "A. afarensis", 
                  "A. afarensis", "A. afarensis", "A. anamensis", "WORMIL",
                  "WORMIL", "WORMIL", "WORMIL", "A. deyiremeda", 
                  "WORMIL", "WORMIL",
                  "A. anamensis", "A. anamensis", "Galili", "Galili", 
                  "Galili", "A. afarensis", "A. afarensis", "A. afarensis", 
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis",
                  "A. afarensis", "A. anamensis", "Lomekwi", "Lomekwi",
                  "WORMIL", "WORMIL", "WORMIL", "WORMIL")

known_position <- c("M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", 
                    "M1", "M1", "M1", "M1", 
                    "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", 
                    "M2", "M2", "M2", "M2", "M2", "M2", 
                    "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", 
                    "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", 
                    "M3", "M3", "M3", "M3")

pca_data <- data.frame(Specimen = known_custom_labels,
                       PC1 = knownpca_results$x[,1],
                       PC2 = knownpca_results$x[,2],
                       PC3 = knownpca_results$x[,3],
                       Group = known_groups,
                       Position = known_position)

known_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="PC 1 (39.7%)", y = "PC 2 (16.4%)") +
  guides(col= guide_legend(title= "Position")) +
  theme_minimal() 
known_plot1

known_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="PC 1 (39.7%)", y = "PC 3 (8.9%)") +
  guides(col= guide_legend(title= "Position")) +
  theme_minimal() 
known_plot2


#### Combined PCA ####
combinedpca_results <- gm.prcomp(gpa$coords)
combinedpca_results

#combined_custom_labels <- c()

#define groups for coloring points
#combined_groups <- c()

#combined_position <- c()

#combinedpca_data <- data.frame(Specimen = combined_custom_labels,
#                               PC1 = combinedpca_results$x[,1],
#                               PC2 = combinedpca_results$x[,2],
#                               PC3 = combinedpca_results$x[,3],
#                               Group = combined_groups,
#                               Position = combined_position)

#combined_plot1 <- ggplot(combinedpca_data, aes(x=PC1, y=PC2, label=Specimen)) +
#  geom_point(aes(color=Group, shape=Position), size=3) +
#  labs(x="PC 1 (x%)", y = "PC 2 (x%)") +
#  guides(col= guide_legend(title= "Population")) +
#  geom_text_repel(max.overlaps = 28) +
#  theme_minimal() 
#combined_plot1

#### CVA ####
cva_PCAdata <- combinedpca_results$x[1:46, 1:9] 

cva_results <- CVA(cva_PCAdata, groups = specimeninfo$Specimen[known])
cva_results

unknown_classification <- classify(cva_results, newdata = combinedpca_results$x[47:55, 1:9])
unknown_classification$class
unknown_classification$posterior


#### CVA Plotted ####
cva_data <- data.frame(Specimen = known_custom_labels,
           CV1 = cva_results$CVscores[,1],
           CV2 = cva_results$CVscores[,2],
           Group = known_groups,
           Position = known_position)
View(cva_data)

cva_plot <- ggplot(cva_data, aes(x=CV1, y=CV2)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="CV 1", y = "CV 2") +
  guides(col= guide_legend(title= "Position")) +
  theme_minimal() 
cva_plot
