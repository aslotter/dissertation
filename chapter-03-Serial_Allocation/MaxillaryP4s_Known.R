#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Maxillary/Maxillary P4s")

#### Max P4 Landmarks ####
#read in landmark data
MaxP4_landmarkdata <- readland.tps("maxillaryp4_landmarks_appended.tps", specID = c("imageID"), 
                                   negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(5:44, nsliders = 40)

#generalized Procrustes analysis
MaxP4_gpa <- gpagen(MaxP4_landmarkdata, curves = sliders)

#running a PCA
pca_results <- gm.prcomp(MaxP4_gpa$coords)
pca_results

#define custom specimen labels
P4_custom_labels <- c("KNM-ER 30745", "GAL 43-1", "GAL 44-7", "GAL 45-31", "AL 200-1a",
                      "AL 200-1a", "AL 333w-42", "AL 417d", "AL 486-1", "KNM-KP 30498C",
                      "ARI-VP-3/248", "BDU-VP-1/83", "BRT-VP-2/89", "LDD-VP-1_167", "MSD-VP-3/58",
                      "MSD-VP-3/97", "MSD-VP-8/107")

#define groups for coloring points
P4_groups <- c("A. anamensis", "Galili", "Galili", "Galili", "A. afarensis", 
            "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", "A. anamensis",
            "WORMIL", "WORMIL", "WORMIL", "WORMIL", "WORMIL", 
            "WORMIL", "WORMIL")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = P4_custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3 = pca_results$x[,3],
                       Group = P4_groups)

#plotting the PCA results
maxp4_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=2) +
  labs(x="PC 1 (60.8%)", y = "PC 2 (10.6%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 20) +
  theme_minimal() 
maxp4_plot1

maxp4_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=2) +
  labs(x="PC 1 (60.8%)", y = "PC 3 (8.0%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 20) +
  theme_minimal() 
maxp4_plot2

ggarrange(maxp4_plot1, maxp4_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
