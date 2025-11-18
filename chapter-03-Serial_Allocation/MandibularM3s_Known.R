#### Basics ####

#loading libraries
library(geomorph)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)

#setting working directory
setwd("C:/Users/aslot/Documents/Graduate School/Dissertation/Data/Serial Allocation/Known/Mandibular/Mandibular M3s")

#### Mand M2 Landmarks ####
#read in landmark data
MandM3_landmarkdata <- readland.tps("mandibularm3_landmarks_appended.tps", specID = c("imageID"), 
                                    negNA = TRUE, readcurves= TRUE)

#defining the semisliding landmarks
sliders <- define.sliders(9:48, nsliders = 40)

#generalized Procrustes analysis
MandM3_gpa <- gpagen(MandM3_landmarkdata, curves = sliders, ProcD = FALSE, print.progress = FALSE)

#running a PCA
pca_results <- gm.prcomp(MandM3_gpa$coords)
pca_results

#define custom specimen labels
custom_labels <- c("GLL 33", "GLL 555", "AL 1583-1", "AL 266-1", 
                   "AL 333w-32", "AL 333w-60", "AL 366-1", "AL 400-1a (right)",
                   "AL 400-1a (left)", "KNM-KP 29286", "KNM-KP 29286",
                   "KNM-KP 29281", "KNM-KP 29286", "KNM-KP 29286", "KNM-KP 47953",
                   "KNM-WT 16006", "KNM-WT 8556", "ARI-VP-1/352", "BDU-VP-1/126",
                   "BRT-VP-1/256", "KER-VP-1/36", "KER-VP-1/36", "LAD-VP-2/190",
                   "LDD-VP-1/732", "LLG-VP-2/192", "LLG-VP-2/192", "MKM-VP-1/885",
                   "MSD-VP 8/65", "NFR-VP-1/29", "NFR-VP-1/2")

#define groups for coloring points
groups <- c("Galili", "Galili", "A. afarensis", "A. afarensis", 
            "A. afarensis", "A. afarensis", "A. afarensis", "A. afarensis",
            "A. afarensis", "A. anamensis", "A. anamensis",
            "A. anamensis", "A. anamensis", "A. anamensis", "A. anamensis",
            "Lomekwi", "Lomekwi", "WORMIL", "WORMIL",
            "WORMIL", "WORMIL", "WORMIL", "WORMIL",
            "WORMIL", "WORMIL", "WORMIL", "WORMIL",
            "WORMIL", "WORMIL", "WORMIL")

#putting together PCA results, specimen labels, group labels
pca_data <- data.frame(Specimen = custom_labels,
                       PC1 = pca_results$x[,1],
                       PC2 = pca_results$x[,2],
                       PC3= pca_results$x[,3],
                       Group = groups)

#plotting the PCA results
mandM3_plot1 <- ggplot(pca_data, aes(x=PC1, y=PC2, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (28.5%)", y = "PC 2 (17.6%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 31) +
  theme_minimal() 
mandM3_plot1

mandM3_plot2 <- ggplot(pca_data, aes(x=PC1, y=PC3, label=Specimen)) +
  geom_point(aes(color=Group), size=3) +
  labs(x="PC 1 (28.5%)", y = "PC 3 (12.1%)") +
  guides(col= guide_legend(title= "Population")) +
  geom_text_repel(max.overlaps = 31) +
  theme_minimal() 
mandM3_plot2

ggarrange(mandM3_plot1, mandM3_plot2, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)
