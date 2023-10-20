setwd("C:/DRN-analysis")
source("./R/Libraries/Libraries.R")
source("./R/Libraries/Libraries_plot.R")
source("./R/Libraries/Functions.R")
source("./R/Libraries/Functions_MC.R")

## New functions -----

create_SPI_df <- function(dates, SPI_list, start, end, labels = NULL, relabel = FALSE){
  idx <- dates %in% create_date_vector(start, end)
  df <- as.data.frame(array(dim = c(sum(idx), length(SPI_list)+1)))
  names(df) <- c("Date", names(SPI_list))
  df$Date <- dates[idx]
  for (res in colnames(df)[2:length(colnames(df))]){
    for (i in seq_len(nrow(df))){
      d <- as.Date(df[i,"Date"], "%d-%m-%Y")
      df[i, res] <- SPI_list[[res]][["fitted"]][dates %in% d]
    }
  }
  mean <- data.frame(df$Date, rowMeans(df[2:ncol(df)]))
  names(mean) <- c("Date", "value")
  if(relabel){
    cols <- c("Date")
    for (c in colnames(df)[2:ncol(df)]){
      cols <- append(cols, labels[[c]])
    }
    names(df) <- cols
    new_order <- sort(colnames(df))
    df <- df[, new_order]
  }
  df <- reshape2::melt(df, id = "Date")
  return(list(df, mean))
}

plot_box_sub <- function(df, breaks, limits){
  p <- ggplot(data = df, aes(x = variable, y = value, group = variable)) +
    geom_boxplot(fill = "#F24C4C", color = "#293462") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title.y = element_text(size = 24, face = "bold"),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50")
    ) +
    #Add the drought indicator lines
    geom_hline(yintercept = 0, color = 'lightblue', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1, color = 'yellow', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1.5, color = 'orange', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -2, color = 'red', linetype = "dashed", size = 1.25, alpha = 0.8) +
    #Modify the axis
    scale_y_continuous(name = 'SPI-12', limits = c(min(df$value), max(df$value)), breaks = seq(-4, 4, 1))
  return(p)
}

plot_violin_sub <- function(df, breaks, limits){
  p <- ggplot(data = df, aes(x = variable, y = value)) +
    # geom_violin(color = "#293462", fill = "#293462") +
    geom_violin(color = "#F24C4C", fill = "#F24C4C", alpha = 0.5) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 26, face = "bold"),
          axis.text.y = element_text(size = 28),
          axis.title.y = element_text(size = 26, face = "bold"),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.major.x = element_line(color = "grey",
                                            size = 0.5,
                                            linetype = 2)
    ) +
    # Add the drought indicator lines
    geom_hline(yintercept = 0, color = 'lightblue', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1, color = 'yellow', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1.5, color = 'orange', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -2, color = 'red', linetype = "dashed", size = 1.25, alpha = 0.8) +
    # Modify the axis
    scale_y_continuous(name = 'SPI-12', limits = c(min(df$value), max(df$value)), breaks = seq(-4, 4, 1))
  return(p)
}

## Load data -------
sub <- readOGR("./Data/Shapefile/subbasins_cut_geomfix.shp")
SPI_list <- list.load("./Data/Analysis/DCA/SPI_list.RData")
DCA_AR <- list.load("./Data/Analysis/DCA/DCA_AR.RData")

labels <- list("152" = "A",
               "154" = "B",
               "159" = "C",
               "158" = "D",
               "150" = "E",
               "155" = "F",
               "145" = "G",
               "144" = "H",
               "146" = "I",
               "149" = "J",
               "153" = "K",
               "151" = "L",
               "160" = "M",
               "157" = "N",
               "142" = "O",
               "147" = "P",
               "148" = "Q",
               "143" = "R",
               "156" = "S",
               "127" = "T",
               "123" = "U",
               "137" = "V",
               "126" = "W",
               "134" = "X",
               "139" = "Y",
               "125" = "Z",
               "138" = "ZA"
)

## Create dataframes ---------

df92 <- create_SPI_df(DCA_AR[["123"]]$date, SPI_list, 1992, 1993, labels, TRUE)
df18 <- create_SPI_df(DCA_AR[["123"]]$date, SPI_list, 2012, 2018, labels, TRUE)

## Relabel the shapefile -----

nlabels <- c()
for (s in sub$SubbasinID){
  nlabels <- append(nlabels, labels[[as.character(s)]])
}
sub$labels <- nlabels

writeOGR(sub, dsn = "./Data/Shapefile/analysis", layer = "subbasin_SPI_all_labels", driver = "ESRI Shapefile")

#Plot the shapefile
sub_sf = st_as_sf(sub)

ggplot() +
  geom_sf(data = sub_sf) +
  theme_void()

## Violin plots -----------
# Subbasins on x axis, SPI-12 on y axis

p <- plot_violin_sub(df92[[1]])
plot.save(p, width = 1920, height = 1080, filename = "./Figures and tables/paper/fix_figure4/1992_violin_subs_pink.png")

p <- plot_violin_sub(df18[[1]])
plot.save(p, width = 1920, height = 1080, filename = "./Figures and tables/paper/fix_figure4/2012_violin_subs_pink.png")

## Trash ---------------------

# 2 - 1992 drought event violin plot with dots showing subbasins' labels

plot_box <- function(df, breaks, limits){
  p <- ggplot() +
    geom_boxplot(data = df, aes(x = Date, y = value, group = Date, fill = "#F24C4C")) + #, color = "#112B3C"
    scale_x_date(date_breaks = breaks, date_labels = "%m-'%y", limits = as.Date(limits), expand = c(0,0)) +    
    theme(legend.position = "none",
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title.y = element_text(size = 24, face = "bold"),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50")
    ) +
    #Add the drought indicator lines
    geom_hline(yintercept = 0, color = 'lightblue', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1, color = 'yellow', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1.5, color = 'orange', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -2, color = 'red', linetype = "dashed", size = 1.25, alpha = 0.8) +
    #Modify the axis
    scale_y_continuous(name = 'SPI-12', limits = c(min(df$value), max(df$value)), breaks = seq(-2, 2, 1))
  return(p)
}

plot_violin <- function(df, breaks, limits){
  p <- ggplot() +
    geom_violin(data = df, aes(x = Date, y = value, group = Date, color = "#F24C4C", fill = "#F24C4C")) +
    scale_x_date(date_breaks = breaks, date_labels = "%m-'%y", limits = as.Date(limits), expand = c(0,0)) +    
    theme(legend.position = "none",
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title.y = element_text(size = 24, face = "bold"),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50")
    )+
    theme(legend.position = "none",
          axis.text.x = element_text(size = 22),
          axis.text.y = element_text(size = 22),
          axis.title.y = element_text(size = 24, face = "bold"),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50")
    ) +
    #Add the drought indicator lines
    geom_hline(yintercept = 0, color = 'lightblue', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1, color = 'yellow', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -1.5, color = 'orange', linetype = "dashed", size = 1.25, alpha = 0.8) +
    geom_hline(yintercept = -2, color = 'red', linetype = "dashed", size = 1.25, alpha = 0.8) +
    #Modify the axis
    scale_y_continuous(name = 'SPI-12', limits = c(min(df$value), max(df$value)), breaks = seq(-2, 2, 1))
  return(p)
}

p <- plot_box(df92[[1]], breaks = "2 months", limits = c("1991-12-01", "1994-01-01"))
p
plot.save(p, width = 1920, height = 1080, filename = "./Figures and tables/paper/fix_figure4/1992_box.png")

p <- plot_violin(df92[[1]], breaks = "2 months", limits = c("1991-12-01", "1994-01-01"))
p
plot.save(p, width = 1920, height = 1080, filename = "./Figures and tables/paper/fix_figure4/1992_violin.png")

# 3 - 2012 drought event violin plot with dots showing subbasins' labels

p <- plot_violin(df18[[1]], breaks = "6 months", limits = c("2011-12-01", "2019-01-01"))
p
plot.save(p, width = 1920, height = 1080, filename = "./Figures and tables/paper/fix_figure4/2012_violin.png")

