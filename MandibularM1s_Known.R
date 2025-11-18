#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Mandibular/Mandibular M1s")

#### Mand M1 Landmarks ####
#read in landmark data
MandM1_landmarkdata <- readland.tps("mandibularm1_landmarks_appended.tps", specID = c("imageID"), 
                                   negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(9:48, nsliders = 40)

#generalized Procrustes analysis
MandM1_gpa <- gpagen(MandM1_landmarkdata, curves = sliders, ProcD = FALSE, print.progress = FALSE)

#running a PCA
pca_results <- gm.prcomp(MandM1_gpa$coords)
pca_results

#define custom specimen labels
custom_labels <- c("KNM-ER 20422", "KNM-ER 30201", "KNM-ER 35232", "GLL 1400", 
                   "AL 128-23", "AL 125-35", "AL 200-1b", "AL 266-1 (left)", 
                   "AL 266-1 (right)", "AL 333-1b", "AL 333w-1a", "AL 996-1",
                   "KNM-KP 29286", "KNM-KP 31712", "KNM-KP 31712", "KNM-KP 34735",
                   "KNM-KP 53160", "KNM-KP 31712", "KNM-KP 34725", "KNM-KP 47953",
                   "KNM-WT 38342", "KNM-WT 38359", "KNM-WT 77322", "KNM-WT 8556",
                   "ARI-VP-1/462", "KSA-VP-1/39")

#define groups for coloring points
groups <- c("A. anamensis", "A. anamensis", "A. anamensis", "Galili", 
            "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", 
            "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis", 
            "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis", 
            "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis",
            "Lomekwi", "Lomekwi", "Lomekwi", "Lomekwi", 
            "WORMIL", "WORMIL")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3= pca_results$x[,3],
                       Group = groups)

#plotting the PCA results
mandM1_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (23.8%)", y = "PC 2 (23.8%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 27) +
  theme_minimal() 
mandM1_plot1

mandM1_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (23.8%)", y = "PC 3 (13.8%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 27) +
  theme_minimal() 
mandM1_plot2

ggarrange(mandM1_plot1, mandM1_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)