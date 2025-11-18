#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Mandibular/Mandibular M2s")

#### Mand M2 Landmarks ####
#read in landmark data
MandM2_landmarkdata <- readland.tps("mandibularm2_landmarks_appended.tps", specID = c("imageID"), 
                                    negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(9:48, nsliders = 40)

#generalized Procrustes analysis
MandM2_gpa <- gpagen(MandM2_landmarkdata, curves = sliders, ProcD = FALSE, print.progress = FALSE)

#running a PCA
pca_results <- gm.prcomp(MandM2_gpa$coords)
pca_results

#define custom specimen labels
custom_labels <- c("KNM-ER 35233", "AL 128-23", "AL 145-35", "AL 207-13", 
                   "AL 241-14", "AL 266-1", "AL 333-1b", "AL 333w-1a", 
                   "AL 333w-48", "AL 417-1a", "AL 996-1", "KNM-KP 29286", 
                   "KNM-KP 29286", "KNM-KP 30500", "KNM-KP 34735", "KNM-KP 53160",
                   "KNM-KP 53160", "KNM-KP 29287", "KNM-KP 58760", "KNM-WT 38359",
                   "ARI-VP-3/80", "KER-VP-1/36", "LDD-VP-1/672", "MSD-VP-5/16",
                   "NFR-VP-1/29")

#define groups for coloring points
groups <- c("A. anamensis", "A. afarensis", "A. afarensis", "A. afarensis",
            "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis",
            "A. afarensis", "A. afarensis", "A. afarensis", "A. anamensis",
            "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis",
            "A. anamensis", "A. anamensis", "A. anamensis", "Lomekwi",
            "WORMIL", "WORMIL", "WORMIL", "WORMIL",
            "WORMIL")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3= pca_results$x[,3],
                       Group = groups)

#plotting the PCA results
mandM2_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (29.7%)", y = "PC 2 (24.3%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 26) +
  theme_minimal() 
mandM2_plot1

mandM2_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (29.7%)", y = "PC 3 (11.3)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 26) +
  theme_minimal() 
mandM2_plot2

ggarrange(mandM2_plot1, mandM2_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)