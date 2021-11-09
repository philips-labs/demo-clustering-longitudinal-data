library(ggdendro)
library(MASS)
library(cluster)
data = generate_osa_data() %>%
    .[, NormTime := (Time - min(Time)) / (max(Time) - min(Time))]

computeMeanTrajectories = function(clusters) {
    assert_that(is.factor(clusters), length(clusters) == uniqueN(data$Id))
    data[, .(Usage=mean(Usage)), by=.(Group=clusters[as.integer(Id)], Time)]
}

# Compute patient representation ####
{
    start = Sys.time()
    patientFeatures = data[, {
        mod = lm(Usage ~ poly(NormTime, 2), data=.SD)
        as.list(coef(mod)) %>% c(logN=log(sum(Usage > 0)), sigma=sigma(mod))
        }, keyby=Id]

    X = as.matrix(patientFeatures[, -c('Id', 'sigma')]) %>%
        set_rownames(patientFeatures$Id) %>%
        scale()

    # Hierarchical clustering
    D = dist(X, method='euclidean')

    htree = hclust(D, method='average')

    Ks = 1:8
    ahcs = lapply(Ks, function(k) pam(D, k=k)) %>%
        set_names(Ks)
    sils = lapply(ahcs, function(ahc) ahc$silinfo$avg.width) %>%
        sapply(function(x) ifelse(is.null(x), NA, x))
    runTime = Sys.time() - start
}
max(sils, na.rm=TRUE)
which.max(sils)

# Solutions ####
dendro = dendro_data(htree, type='rectangle')

ggplot(segment(dendro)) +
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
    scale_x_continuous(breaks=pretty_breaks(10)) +
    coord_flip() +
    labs(x='Trajectories\n\n', y='Height') +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())

plotMetric(sils, Ks, 'Silhouette width')
# ggsave('save/featurebased_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

# Assess the best solution ####
k = 7
bestAhc = ahcs[[as.character(k)]]
clusters = factor(bestAhc$clustering, labels=LETTERS[1:k])
groupProps = table(clusters) %>% prop.table() %>% as.numeric()

computeMeanTrajectories(clusters) %>% plotGroupTrajectories(groupProps)
# ggsave('save/featurebased_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')
