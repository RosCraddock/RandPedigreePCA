rm(list = ls())
library(randPedPCA)
library(readr)
library(tidyverse)
library(viridisLite)

Labrador <- read_csv(file = "/Users/roscraddock/Documents/University\ of\ Edinburgh/HighlanderLab - Data_2025/LabPed2025.csv")
Labrador<- Labrador[order(Labrador$gen),]

Labrador$Coat_Colour <- Labrador$Colour
labcolours <- c("BLACK", "YELLOW", "LIVER", "CHOCOLATE")
Labrador <- Labrador %>% 
  transform(Coat_Colour = ifelse(Coat_Colour %in% labcolours, Coat_Colour, "Unknown/nonStandard"))
Labrador <- Labrador %>%
  transform(Coat_Colour = ifelse(Coat_Colour == "BLACK", "Black", Coat_Colour))
Labrador <- Labrador %>%
  transform(Coat_Colour = ifelse(Coat_Colour == "YELLOW", "Yellow", Coat_Colour))
Labrador <- Labrador %>%
  transform(Coat_Colour = ifelse(Coat_Colour == "LIVER", "Chocolate", Coat_Colour))
Labrador <- Labrador %>%
  transform(Coat_Colour = ifelse(Coat_Colour == "CHOCOLATE", "Chocolate", Coat_Colour))

# Prep for PCA
# Create Pedigree Object
ped <- pedigree(Labrador$fatherID,
                Labrador$motherID,
                Labrador$id)

# Obtain centred estimate from L inverse using Hutch++
li <- sparse2spam(getLInv(ped)) # generate L inverse and convert to spam format
set.seed(123345) # set random seed as Hutch++ uses random numbers
hp <- hutchpp(li,num_queries = 100, center = T) # estimate

# Run randPedPCA
pc <- rppca(ped, center=T, totVar = hp)
summary(pc)

# Collect proportion of variances explained by PC1 and PC2
var <- pc[["varProps"]]*100
# Labels for x and y axis
pc1 <- paste("PC1 (",round(var[1], 2), "%)", sep = "")
pc2 <- paste("PC2 (",round(var[2], 2), "%)", sep = "")

# Collect PC in t
t <- pc[["x"]]

# ggplot to get colour gradient for generations
p0 <- ggplot(data = t, aes(x = t[,1], y = t[,2], colour = Labrador$gen)) +
  geom_point() +
  theme_minimal() +
  labs(
    x = pc1,
    y = pc2,
    colour = "Generation") +
  scale_shape(solid = FALSE) +
  scale_colour_viridis_c(
    option = "viridis",
    name = "Generation"
  ) +
  theme(
    axis.title.x = element_text(size = 22),  # Increase x-axis label text size
    axis.title.y = element_text(size = 22),  # Increase y-axis label text size
    plot.title = element_text(size = 24),    # Increase plot title text size
    legend.title = element_text(size = 22),  # Increase legend title text size
    legend.text = element_text(size = 20),
    axis.text.x = element_text(size = 20),   # Increase x-axis tick labels text size
    axis.text.y = element_text(size = 20))


# 3D plots

# By coat colour
Labrador$coatCol <- Labrador$Coat_Colour
Labrador <- Labrador %>% 
  transform(coatCol = ifelse(coatCol == "Unknown/nonStandard", "grey", coatCol))
Labrador <- Labrador %>% 
  transform(coatCol = ifelse(coatCol == "Chocolate", "#5c3e35", coatCol))
Labrador <- Labrador %>% 
  transform(coatCol = ifelse(coatCol == "Yellow", "#ffee8c", coatCol))


plot3DWithProj(pc, col = as.factor(Labrador$coatCol))

# By generation
col <- ggplot_build(p0)$data
plot3DWithProj(pc, col = as.factor(col[[1]]$colour))