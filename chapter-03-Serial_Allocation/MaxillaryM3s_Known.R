#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Maxillary/Maxillary M3s")

#### Max M3 Landmarks ####
#read in landmark data
MaxM3_landmarkdata <- readland.tps("maxillarym3_landmarks_appended.tps", specID = c("imageID"), 
                                   negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(9:48, nsliders = 40)

#generalized Procrustes analysis
MaxM3_gpa <- gpagen(MaxM3_landmarkdata, curves = sliders, ProcD = FALSE, print.progress = FALSE)

#running a PCA
pca_results <- gm.prcomp(MaxM3_gpa$coords)
pca_results

#define custom specimen labels
custom_labels <- c("KNM-ER 20421", "KNM-ER 35236", "GLL 1666", "GLL 1717",
                   "GLL 1800", "AL 161-40", "AL 199-1", "AL 200-1a (left)", 
                   "AL 200-1a (right)", "AL 333x-1", "AL 417d (left)", "AL 417d (right)",
                   "AL 486-1", "KNM-KP 58761", "KNM-WT 16003", "KNM-WT 77329f",
                   "ARI-VP-1/215", "ARI-VP-1/90", "MSD-VP-3/58c", "MSD-VP-3/58d")

#define groups for coloring points
groups <- c("A. anamensis", "A. anamensis", "Galili", "Galili", 
            "Galili", "A. afarensis", "A. afarensis", "A. afarensis", 
            "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis",
            "A. afarensis", "A. anamensis", "Lomekwi", "Lomekwi",
            "WORMIL", "WORMIL", "WORMIL", "WORMIL")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3= pca_results$x[,3],
                       Group = groups)

#plotting the PCA results
maxM3_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (38.6%)", y = "PC 2 (15.3%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 21) +
  theme_minimal() 
maxM3_plot1

maxM3_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (38.6%)", y = "PC 3 (11.0%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 21) +
  theme_minimal() 
maxM3_plot2

ggarrange(maxM3_plot1, maxM3_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
