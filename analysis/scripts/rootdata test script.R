library(tidyverse)
library(magick)
library(png)
library(ggplot2)
library(cowplot)
source("analysis/scripts/packages_and_functions.R")

getwd()
rootdata <- rio::import("analysis/data/19-04-18 raw root data.csv")
summary(rootdata)

mainroots=rootdata[rootdata$root_order==0,] 
#I select from the root order only the ones with a "0" and put them in mainroots
head(mainroots)

LRroots=rootdata[rootdata$root_order==1,] 
#I select from the root order only the ones with a "1" and put them in LRroots
head(LRroots)

#werkwijze#2 deze werkwijze rockt, maar je kan geen typo's maken in je originele data

tl=strsplit(as.vector(mainroots$root_name),"_") #here it separates genotype and treatment
#we splitten de string op de underscore, gooien hem naar een temp table (tl)

mainroots$genotype <- sapply(tl, function(x)x[1])
#Dan gooien we via sapply de 1e kolom naar het genotype

c2 <- sapply(tl, function(x)x[2])
#Dan de tweede kolom naar een temp c2 variable

mainroots$treatment <- gsub("[1234567890]", "", c2)
#en dan selecteren we de getallen en vervangen we ze door niks

trimws(mainroots$genotype,c("both"))
trimws(mainroots$treatment,c("both"))

mainroots$genotype #is all ok?
mainroots$treatment #is all ok?

mainroots$lrdensity = (mainroots$n_child / mainroots$length)
#calculate the lrdensity

# plotting ----------------------------------------------------------------

args(theme)



plot_LRdens <- mainroots %>%
  ggplot(aes(x = genotype, y = lrdensity, fill = treatment, na.rm = TRUE)) +
  geom_violin() +
  geom_point( position=position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), alpha = 0.5, size = 0.4) +
  scale_fill_manual(values = c("#D55E00", "#E69F00", "#aaaaaa", "#dddddd")) +
  guides(fill = guide_legend(title = "Treatment")) +
  #coord_flip() +
  ylim(0,NA)
#scale_y_log10()

theme_plots <- theme_bw() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key.size = unit(7, "mm"),
    legend.title.position = "top",
    legend.background = element_rect(color = "grey"),
    plot.title.position = "panel"
  )

plot_LRdens <- plot_LRdens +
  theme_plots
plot_LRdens

ggsave( "analysis/pictures/plot_rootdatabbox.jpg",
        limitsize = FALSE,
        units = c("px"), plot_LRdens,
        width = 2000, height = 1500
)


# hypocotyl length --------------------------------------------------------
library(xlsx)
hypocotyl_length = read.xlsx("analysis/data/19-04-24 hypocotyl length form.xlsx",2)#filename and sheet nr.

plot_hyplength <- hypocotyl_length %>%
  ggplot(aes(x = genotype, y = length, fill = treatment, na.rm = TRUE)) +
  geom_violin() +
  geom_point( position=position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), alpha = 0.5, size = 0.4) +
  scale_fill_manual(values = c("#D55E00", "#E69F00", "#aaaaaa", "#dddddd")) +
  guides(fill = guide_legend(title = "Treatment")) +
  #coord_flip() +
  ylim(0,NA)

plot_hyplength <- plot_hyplength +
  theme_plots
plot_hyplength

plot_LRdens <- plot_LRdens +
  theme_plots
plot_LRdens

ggsave( "analysis/pictures/plot_hyplengthbbox.jpg",
        limitsize = FALSE,
        units = c("px"), plot_hyplength,
        width = 2000, height = 1500
)

#introduce gap in layout
layout2 <- "A#B"

#assemble multipanel figure based on layout
Figure_bboxroot2 <- plot_hyplength + plot_LRdens +
  plot_layout(design = layout2, widths = c(1, 0.03, 1)) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12, face='plain'))


#save figure as png
ggsave(
  "manuscript/figures/Figure_roots2.jpg", 
  limitsize = FALSE,
  units = c("px"), Figure_bboxroot2, 
  width = 4000, height = 1800,
  bg = "white"
)

image_read("manuscript/figures/Figure_roots2.jpg")


# bigger figure -----------------------------------------------------------

panel_graphs <- ggdraw() + draw_image(image_read("manuscript/figures/Figure_roots2.jpg"))
panel_col0_wl <- ggdraw() + draw_image(image_read("analysis/pictures/Col-0_WL.jpg"))
panel_col0_wlfr <- ggdraw() + draw_image(image_read("analysis/pictures/Col-0_WLFR.jpg"))
panel_bbox20ox_wl <- ggdraw() + draw_image(image_read("analysis/pictures/BBox20ox_WL.jpg"))
panel_bbox20ox_wlfr <- ggdraw() + draw_image(image_read("analysis/pictures/BBox20ox_WLFR.jpg"))
panel_bbox202122ox_wl <- ggdraw() + draw_image(image_read("analysis/pictures/bbox20-21-22_WL.jpg"))
panel_bbox202122ox_wlfr <- ggdraw() + draw_image(image_read("analysis/pictures/bbox20-21-22_WLFR.jpg"))

#introduce gap in layout
layout <- "
AAAAAA
######
BCDEFG
"


#assemble multipanel figure based on layout
Figure_roots_bigger <- panel_graphs + 
  panel_col0_wl +  panel_col0_wlfr + panel_bbox20ox_wl + panel_bbox20ox_wlfr + panel_bbox202122ox_wl + panel_bbox202122ox_wlfr +
  plot_layout(design = layout, heights = c(1, 1, 0.2, 2)) +
  plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(size = 12, face='plain'))

#save figure as png
ggsave(
  "manuscript/figures/Figure_roots_complex.png",
  units = c("px"), Figure_roots_bigger, 
  width = 2600, height = 1700, bg = "white"
)

image_read("manuscript/figures/Figure_roots_complex.png")
