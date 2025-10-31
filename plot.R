library(ggplot2)
library(dplyr)
library(tidyr)

#### Fonction d'appel

generate_plot <- function(data, var){
  if(var =='hbmass'){
    return(plothbmass(data))
  }
  if(var == 'hbmass_rel'){
    return(plothbmass_rel(data))
  }
  if(var == 'total_blood_volume'){
    return(plottbv(data))
  }
  if(var == 'total_blood_volume_rel'){
    return(plottbv_rel(data))
  }
  if(var == 'plasma_volume'){
    return(plotplasma(data))
  }
  if(var == 'plasma_volume_rel'){
    return(plotplasma_rel(data))
  }
  if(var == 'red_blood_volume'){
    return(plotrbv(data))
  }
  if(var == 'red_blood_volume_rel'){
    return(plotrbv_rel(data))
  }
  if(var == 'Hematocrit'){
    return(plothematocrit(data))
  }
  if(var == 'hb'){
    return(plothb(data))
  }
  if(var == 'weight'){
    return(plotweight(data))
  }
}

#### Fonctions par type de grpahique
plothbmass <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))

  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$hbmass)*1.1
    )
  
  ggplot(data, aes(x = iter, y = hbmass, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(hbmass, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution de la masse d'hémoglobine",
      x = "Mesure",
      y = "HBmass (g)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}


plothbmass_rel <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$hbmass_rel)*1.1
    )
  
  ggplot(data, aes(x = iter, y = hbmass_rel, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(hbmass_rel, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution de la masse d'hémoglobine (relative)",
      x = "Mesure",
      y = "HBmass (g/kg)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plottbv <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$total_blood_volume)*1.1
    )
  
  ggplot(data, aes(x = iter, y = total_blood_volume, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(total_blood_volume, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume Totale de Sang",
      x = "Mesure",
      y = "Volume Totale de Sang (ml)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plottbv_rel <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$total_blood_volume_rel)*1.1
    )
  
  ggplot(data, aes(x = iter, y = total_blood_volume_rel, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(total_blood_volume_rel, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume Totale de Sang (relatif)",
      x = "Mesure",
      y = "Volume Totale de Sang (ml/kg)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plotplasma <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$plasma_volume)*1.1
    )
  
  ggplot(data, aes(x = iter, y = plasma_volume, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(plasma_volume, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume de Plasma",
      x = "Mesure",
      y = "Volume Totale de Plasma (ml)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}


plotplasma_rel <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$plasma_volume_rel)*1.1
    )
  
  ggplot(data, aes(x = iter, y = plasma_volume_rel, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(plasma_volume_rel, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume de Plasma (relatif)",
      x = "Mesure",
      y = "Volume Totale de Plasma (ml/kg)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plotrbv <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$red_blood_volume)*1.1
    )
  
  ggplot(data, aes(x = iter, y = red_blood_volume, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(red_blood_volume, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume de Globule Rouge",
      x = "Mesure",
      y = "Volume Globule Rouge (ml)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plotrbv_rel <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$red_blood_volume_rel)*1.1
    )
  
  ggplot(data, aes(x = iter, y = red_blood_volume_rel, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(red_blood_volume_rel, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume de Globule Rouge (relatif)",
      x = "Mesure",
      y = "Volume Globule Rouge (ml/kg)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plothematocrit <- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$Hematocrit)*1.1
    )
  
  ggplot(data, aes(x = iter, y = Hematocrit, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(Hematocrit, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Volume d'Hématocrite",
      x = "Mesure",
      y = "Volume Hématocrite (%)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plothb<- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$hb)*1.1
    )
  
  ggplot(data, aes(x = iter, y = hb, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(hb, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black", 
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Taux d'Hémoglobine",
      x = "Mesure",
      y = "Taux d'Hémoglobine (g/dl)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

plotweight<- function(data){
  data <- data %>%
    arrange(iter) %>%
    mutate(change_group = c(TRUE, diff(as.numeric(factor(stage_ref))) != 0))
  
  line_positions <- data$iter[data$change_group]-1
  
  group_labels <- data %>%
    group_by(stage_ref) %>%
    summarise(
      x_pos = mean(iter),
      y_pos = max(data$weight)*1.1
    )
  
  ggplot(data, aes(x = iter, y = weight, fill = stage_ref)) +
    geom_col(width = 0.5) +
    geom_text(
      aes(label = round(weight, 1)),
      vjust = -0.5,
      size = 3.5,
      color = "black"
    ) +
    geom_text(
      aes(label = measurement_time, y = 0),
      vjust = 1.5,   # Décalage vertical pour placer le texte en dessous
      size = 3.5,    # Taille du texte
      color = "black",
    ) +
    geom_vline(
      xintercept = line_positions + 0.5,  # Décale légèrement pour centrer entre les barres
      linetype = "dashed",
      color = "gray50",
      linewidth = 0.5
    ) +
    # Ajoute les labels de groupe
    geom_text(
      data = group_labels,
      aes(x = x_pos, y = y_pos, label = stage_ref),
      vjust = -0.5,
      size = 1.5,
      color = "black",
      fontface = 'bold'
    ) +
    labs(
      title = "Évolution du Poids",
      x = "Mesure",
      y = "Poids (kg)",
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}








