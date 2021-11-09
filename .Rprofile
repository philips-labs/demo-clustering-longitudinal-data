suppressPackageStartupMessages({
    library(data.table)
    library(magrittr)
    library(assertthat)
    library(ggplot2)
    library(scales)
    library(matrixStats)
    require(IMIFA)
})

source('data.R')
source('plot.R')
source('metrics.R')

theme_minimal(base_size = 9) %+replace%
    theme(
        plot.background = element_rect(colour = NA),
        plot.margin = unit(c(2,1,1,1), 'mm'),
        panel.background = element_rect(colour = NA),
        panel.spacing = unit(1, 'mm'),
        strip.background = element_rect(colour=NA, fill=NA),
        strip.text = element_text(face='plain', size=7, margin=margin()),
        legend.text = element_text(size=7),
        legend.title = element_text(size=9),
        legend.position = 'right',
        legend.spacing = unit(1, 'cm'),
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(9, 'pt'),
        legend.box.margin = margin(0,0,-5,0),
        axis.line.x = element_line(color='black', size = .1),
        axis.line.y = element_line(color='black', size = .1)
    ) %>%
    theme_set()

bicPlotSize = c(6, 5)
groupPlotSize = c(9, 5)
