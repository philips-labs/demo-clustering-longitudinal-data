library(cluster)
library(ggdendro)
library(cluster)
data = generate_osa_data()
dataMat = transformToRepeatedMeasures(data)

computeMeanTrajectories = function(clusters) {
    assert_that(is.factor(clusters), length(clusters) == uniqueN(data$Id))
    data[, .(Usage=mean(Usage)), by=.(Group=clusters[as.integer(Id)], Time)]
}

# Estimation ####
start = Sys.time()
D = dist(dataMat, method='euclidean')
htree = hclust(D, method='average')

Ks = 1:8
ahcsLabels = lapply(Ks, function(k) cutree(htree, k=k)) %>%
    set_names(Ks)
runTime = Sys.time() - start
# Solutions ####
dendro = dendro_data(htree, type='rectangle')
ggplot(segment(dendro)) +
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=.001) +
    scale_x_continuous(breaks=pretty_breaks(4)) +
    coord_flip() +
    labs(x='Patient', y='Height') +
    theme(panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())
# ggsave('save/ahc_tree.pdf', width=15, height=3, units='cm')

# Compute silhouette widths
sils = lapply(ahcsLabels[as.character(setdiff(Ks, 1))], silhouette, dist=D) %>%
    lapply(summary) %>%
    sapply('[[', 'avg.width')

# Compute residual sum of squares
rsss = sapply(ahcsLabels, function(clusters) {
    clusters = factor(clusters, labels=LETTERS[1:uniqueN(clusters)])
    dtPatClus = data.table(Id=levels(data$Id), Group=clusters)
    dtTraj = computeMeanTrajectories(clusters)
    dtFitted = data[dtPatClus, on='Id'] %>%
        merge(dtTraj, by.x=c('i.Group', 'Time'), by.y=c('Group', 'Time'))
    dtFitted[, sum((Usage.x - Usage.y)^2)]
})

plotMetric(c(NA, sils), Ks, 'Silhouette width')
# ggsave('save/ahc_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

plotMetric(rsss, Ks, 'RSS')

# Assess result ####
k = 4
clusters = ahcsLabels[[as.character(k)]] %>%
    factor(labels=LETTERS[1:k])
groupProps = table(clusters) %>% prop.table() %>% as.numeric()

computeMeanTrajectories(clusters) %>% plotGroupTrajectories(groupProps)
# ggsave('save/ahc_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')
