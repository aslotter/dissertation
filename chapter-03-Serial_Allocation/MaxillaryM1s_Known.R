#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Maxillary/Maxillary M1s")

#### Max P3 Landmarks ####
#read in landmark data
MaxM1_landmarkdata <- readland.tps("maxillaryM1_landmarks_appended.tps", specID = c("imageID"), 
                                   negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(9:48, nsliders = 40)

#generalized Procrustes analysis
MaxM1_gpa <- gpagen(MaxM1_landmarkdata, curves = sliders, ProcD = FALSE, print.progress = FALSE)

#running a PCA
pca_results <- gm.prcomp(MaxM1_gpa$coords)
pca_results

#define custom specimen labels
custom_labels <- c("KNM-ER 20427", "KNM-ER 30745", "AL 199-1", "AL 200-1a",
                   "AL 200-1a", "AL 486-1", "KNM-ER 30200a", "KNM-KP 35842",
                   "KNM-WT 38337", "ARI-VP-1/484", "ARI-VP-3/659", "BRT-VP-3/1")

#define groups for coloring points
groups <- c("A. anamensis", "A. anamensis", "A. afarensis", "A. afarensis", 
            "A. afarensis", "A. afarensis", "A. anamensis", "A. anamensis",
            "Lomekwi", "WORMIL", "WORMIL", "A. deyiremeda")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3= pca_results$x[,3],
                       Group = groups)

#plotting the PCA results
maxM1_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (43.2%)", y = "PC 2 (22%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 20) +
  theme_minimal() 
maxM1_plot1

maxM1_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (43.2%)", y = "PC 3 (9.6%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 20) +
  theme_minimal() 
maxM1_plot2

ggarrange(maxM1_plot1, maxM1_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

