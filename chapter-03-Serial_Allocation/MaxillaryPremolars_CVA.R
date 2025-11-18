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
landmarkdata <- readland.tps("maxillarypremolars_joined.tps", specID = ("imageID"))

sliders <- define.sliders(5:44, nsliders = 40)

#### GPA ####

gpa <- gpagen(landmarkdata, PrinAxes = TRUE, curves = sliders)
#GPA for joined

specimeninfo <- data.frame(
  Specimen = c(rep("P3", 8), rep("P4", 17), rep(NA, 3)),
  stringsAsFactors = TRUE
)
#creating a list of what each specimen is (P3, P4, or unknown)

known <- which(!is.na(specimeninfo$Specimen))
unknown <- which(is.na(specimeninfo$Specimen))
#identifying which species in the joined list are known vs. unknown

#### Known PCA ####
knownpca_results <- gm.prcomp(gpa$coords[,, known])
knownpca_results

known_custom_labels <- c("KNM-ER 30745", "AL 200-1a (left)", "AL 200-1a (right)", 
        "AL 486-1", "KNM-WT 77329", "BRT-VP-3/1", "LDD-VP-1/167 (p3)", "LLG-VP-3/77c", 
        "KNM-ER 30745", "GAL 43-1", "GAL 44-7", "GAL 45-31", "AL 200-1a",
        "AL 200-1a", "AL 333w-42", "AL 417d", "AL 486-1", "KNM-KP 30498C",
        "ARI-VP-3/248", "BDU-VP-1/83", "BRT-VP-2/89", "LDD-VP-1/167 (p4)", "MSD-VP-3/58",
        "MSD-VP-3/97", "MSD-VP-8/107")

#define groups for coloring points
known_groups <- c("A. anamensis", "A. afarensis", "A. afarensis", "A. afarensis", 
        "Lomekwi", "WORMIL", "WORMIL", "WORMIL", 
        "A. anamensis", "Galili", "Galili", "Galili", "A. afarensis", 
        "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", "A. anamensis",
        "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL")

known_position <- c("P3", "P3", "P3", "P3", "P3", "P3", "P3", "P3", "P4", "P4", "P4", "P4", 
                    "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", 
                    "P4")

pca_data <- data.frame(Specimen = known_custom_labels,
                       PC1 = knownpca_results$x[,1],
                       PC2 = knownpca_results$x[,2],
                       PC3 = knownpca_results$x[,3],
                       Group = known_groups,
                       Position = known_position)

known_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="PC 1 (50.2%)", y = "PC 2 (17.6%)") +
  guides(col= guide_legend(title= "Tooth Position")) +
  theme_minimal() 
known_plot1

known_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3)) +
  geom_point(aes(color=Position), size=2) +
  labs(x="PC 1 (50.2%)", y = "PC 3 (x=8.2%)") +
  guides(col= guide_legend(title= "Tooth Position")) +
  theme_minimal() 
known_plot2


#### Combined PCA ####
combinedpca_results <- gm.prcomp(gpa$coords)
combinedpca_results

combined_custom_labels <- c("KNM-ER 30745", "AL 200-1a (left)", "AL 200-1a (right)", 
                         "AL 486-1", "KNM-WT 77329", "BRT-VP-3/1", "LDD-VP-1/167 (p3)", "LLG-VP-3/77c", 
                         "KNM-ER 30745", "GAL 43-1", "GAL 44-7", "GAL 45-31", "AL 200-1a",
                         "AL 200-1a", "AL 333w-42", "AL 417d", "AL 486-1", "KNM-KP 30498C",
                         "ARI-VP-3/248", "BDU-VP-1/83", "BRT-VP-2/89", "LDD-VP-1/167 (p4)", "MSD-VP-3/58",
                         "MSD-VP-3/97", "MSD-VP-8/107", "AL 1256-1", "KNM-KP 47953", 
                         "KNM-WT 38361")

#define groups for coloring points
combined_groups <- c("A. anamensis", "A. afarensis", "A. afarensis", "A. afarensis", 
                  "Lomekwi", "WORMIL", "WORMIL", "WORMIL", 
                  "A. anamensis", "Galili", "Galili", "Galili", "A. afarensis", 
                  "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", "A. anamensis",
                  "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL",
                  "A. afarensis", "A. anamensis", "Lomekwi")

combined_position <- c("P3", "P3", "P3", "P3", "P3", "P3", "P3", "P3", "P4", "P4", "P4", "P4", 
                    "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", "P4", 
                    "P4", "unknown", "unknown", "unknown")

combinedpca_data <- data.frame(Specimen = combined_custom_labels,
                       PC1 = combinedpca_results$x[,1],
                       PC2 = combinedpca_results$x[,2],
                       PC3 = combinedpca_results$x[,3],
                       Group = combined_groups,
                       Position = combined_position)

combined_plot1 <- ggplot(combinedpca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group, shape=Position), size=3) +
  labs(x="PC 1 (x%)", y = "PC 2 (x%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 28) +
  theme_minimal() 
combined_plot1

#### CVA ####
cva_PCAdata <- combinedpca_results$x[1:25, 1:8] 

cva_results <- CVA(cva_PCAdata, groups = specimeninfo$Specimen[known])
cva_results

unknown_classification <- classify(cva_results, newdata = combinedpca_results$x[26:28, 1:8])
unknown_classification$class
unknown_classification$posterior


#### CVA Histogram ####

cv_histogram <- ggplot(cva_results$CVscores, aes(x=cva_results$CVscores, fill=cva_results$groups)) +
  geom_histogram(position="identity", bins=20) +
  labs(x="CV Scores", y="Count", fill="Tooth Position") +
  theme_minimal() 
cv_histogram
