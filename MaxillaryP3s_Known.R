#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Maxillary/Maxillary P3s")

#### Max P3 Landmarks ####
#read in landmark data
MaxP3_landmarkdata <- readland.tps("maxillaryp3_landmarks_appended.tps", specID = c("imageID"), 
                                negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(5:44, nsliders = 40)

#generalized Procrustes analysis
MaxP3_gpa <- gpagen(MaxP3_landmarkdata, curves = sliders, ProcD = FALSE, print.progress = FALSE)

#running a PCA
pca_results <- gm.prcomp(MaxP3_gpa$coords)
pca_results

#define custom specimen labels
custom_labels <- c("KNM-ER 30745", "AL 200-1a (left)", "AL 200-1a (right)", 
       "AL 486-1", "KNM-WT 77329", "BRT-VP-3/1", "LDD-VP-1/167", "LLG-VP-3/77c")

#define groups for coloring points
groups <- c("A. anamensis", "A. afarensis", "A. afarensis", "A. afarensis", 
      "Lomekwi", "WORMIL", "WORMIL", "WORMIL")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3= pca_results$x[,3],
                       Group = groups)

#plotting the PCA results
maxp3_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (71.8%)", y = "PC 2 (14.1%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 20) +
  theme_minimal() 

maxp3_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (71.8%)", y = "PC 3 (5.2%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 20) +
  theme_minimal() 

ggarrange(maxp3_plot1, maxp3_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)