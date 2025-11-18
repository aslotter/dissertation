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
landmarkdata <- readland.tps("mandibularmolars_joined.tps", specID = ("imageID"))

sliders <- define.sliders(9:48, nsliders = 40)

#### GPA ####

gpa <- gpagen(landmarkdata, PrinAxes = TRUE, curves = sliders)
#GPA for joined

specimeninfo <- data.frame(
  Specimen = c(rep("M1", 26), rep("M2", 25), rep("M3", 30), rep(NA, 24)),
  stringsAsFactors = TRUE
)
#creating a list of what each specimen is (M1, M2, M3, or unknown)

known <- which(!is.na(specimeninfo$Specimen))
unknown <- which(is.na(specimeninfo$Specimen))
#identifying which species in the joined list are known vs. unknown

#### Known PCA ####
knownpca_results <- gm.prcomp(gpa$coords[,, known])
knownpca_results

known_custom_labels <- c("KNM-ER 20422", "KNM-ER 30201", "KNM-ER 35232", "GLL 1400", 
                         "AL 128-23", "AL 125-35", "AL 200-1b", "AL 266-1 (left)", 
                         "AL 266-1 (right)", "AL 333-1b", "AL 333w-1a", "AL 996-1",
                         "KNM-KP 29286", "KNM-KP 31712", "KNM-KP 31712", "KNM-KP 34735",
                         "KNM-KP 53160", "KNM-KP 31712", "KNM-KP 34725", "KNM-KP 47953",
                         "KNM-WT 38342", "KNM-WT 38359", "KNM-WT 77322", "KNM-WT 8556",
                         "ARI-VP-1/462", "KSA-VP-1/39",
                         "KNM-ER 35233", "AL 128-23", "AL 145-35", "AL 207-13", 
                         "AL 241-14", "AL 266-1", "AL 333-1b", "AL 333w-1a", 
                         "AL 333w-48", "AL 417-1a", "AL 996-1", "KNM-KP 29286", 
                         "KNM-KP 29286", "KNM-KP 30500", "KNM-KP 34735", "KNM-KP 53160",
                         "KNM-KP 53160", "KNM-KP 29287", "KNM-KP 58760", "KNM-WT 38359",
                         "ARI-VP-3/80", "KER-VP-1/36", "LDD-VP-1/672", "MSD-VP-5/16",
                         "NFR-VP-1/29",
                         "GLL 33", "GLL 555", "AL 1583-1", "AL 266-1", 
                         "AL 333w-32", "AL 333w-60", "AL 366-1", "AL 400-1a (right)",
                         "AL 400-1a (left)", "KNM-KP 29286", "KNM-KP 29286",
                         "KNM-KP 29281", "KNM-KP 29286", "KNM-KP 29286", "KNM-KP 47953",
                         "KNM-WT 16006", "KNM-WT 8556", "ARI-VP-1/352", "BDU-VP-1/126",
                         "BRT-VP-1/256", "KER-VP-1/36", "KER-VP-1/36", "LAD-VP-2/190",
                         "LDD-VP-1/732", "LLG-VP-2/192", "LLG-VP-2/192", "MKM-VP-1/885",
                         "MSD-VP 8/65", "NFR-VP-1/29", "NFR-VP-1/2")

#define groups for coloring points
known_groups <- c("A. anamensis", "A. anamensis", "A. anamensis", "Galili", 
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", 
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", 
                  "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis", 
                  "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis",
                  "Lomekwi", "Lomekwi", "Lomekwi", "Lomekwi", 
                  "WORMIL", "WORMIL",
                  "A. anamensis", "A. afarensis", "A. afarensis", "A. afarensis",
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis",
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. anamensis",
                  "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis",
                  "A. anamensis", "A. anamensis", "A. anamensis", "Lomekwi",
                  "WORMIL", "WORMIL", "WORMIL", "WORMIL",
                  "WORMIL",
                  "Galili", "Galili", "A. afarensis", "A. afarensis", 
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis",
                  "A. afarensis", "A. anamensis", "A. anamensis",
                  "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis",
                  "Lomekwi", "Lomekwi", "WORMIL", "WORMIL",
                  "WORMIL", "WORMIL", "WORMIL", "WORMIL",
                  "WORMIL", "WORMIL", "WORMIL", "WORMIL",
                  "WORMIL", "WORMIL", "WORMIL")

known_position <- c("M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", 
                    "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M1", 
                    "M1", "M1", "M1", "M1", "M1", "M1", 
                    "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", 
                    "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2", 
                    "M2", "M2", "M2", "M2", "M2", 
                    "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", 
                    "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", 
                    "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3", "M3")

pca_data <- data.frame(Specimen = known_custom_labels,
                       PC1 = knownpca_results$x[,1],
                       PC2 = knownpca_results$x[,2],
                       PC3 = knownpca_results$x[,3],
                       Group = known_groups,
                       Position = known_position)

known_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="PC 1 (26.2%)", y = "PC 2 (16.8%)") +
  guides(col= guide_legend(title= "Position")) +
  theme_minimal() 
known_plot1

known_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="PC 1 (26.2%)", y = "PC 3 (11.4%)") +
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
                               #PC1 = combinedpca_results$x[,1],
                              # PC2 = combinedpca_results$x[,2],
                              # PC3 = combinedpca_results$x[,3],
                              # Group = combined_groups,
                              # Position = combined_position)

#combined_plot1 <- ggplot(combinedpca_data, aes(x=PC1, y=PC2, label=Specimen)) +
#  geom_point(aes(color=Group, shape=Position), size=3) +
#  labs(x="PC 1 (x%)", y = "PC 2 (x%)") +
#  guides(col= guide_legend(title= "Population")) +
#  geom_text_repel(max.overlaps = 28) +
#  theme_minimal() 
#combined_plot1

#### CVA ####
cva_PCAdata <- combinedpca_results$x[1:81, 1:12] 

cva_results <- CVA(cva_PCAdata, groups = specimeninfo$Specimen[known])
cva_results

unknown_classification <- classify(cva_results, newdata = combinedpca_results$x[82:105, 1:12])
unknown_classification$class
unknown_classification$posterior

#### CVA Plotted ####
cva_data <- data.frame(Specimen = known_custom_labels,
                       CV1 = cva_results$CVscores[,1],
                       CV2 = cva_results$CVscores[,2],
                       Group = known_groups,
                       Position = known_position)

cva_plot <- ggplot(cva_data, aes(x=CV1, y=CV2)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="CV 1", y = "CV 2") +
  guides(col= guide_legend(title= "Position")) +
  theme_minimal() 
cva_plot
