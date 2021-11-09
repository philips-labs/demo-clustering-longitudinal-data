library(kml)
data = generate_osa_data()

dataMat = transformToRepeatedMeasures(data)
cld = clusterLongData(dataMat, idAll=rownames(dataMat), time=sort(unique(data$Time)), varNames='Usage')
par = parALGO(startingCond='kmeans++')

computeKmlTrajectories = function(k) {
    calculTrajMeanC(cld@traj, clust=getClusters(cld, bestK, asInteger=TRUE)) %>%
        set_rownames(LETTERS[seq_len(nrow(.))]) %>%
        melt(varnames=c('Group', 'Time'), value.name='Usage') %>%
        as.data.table() %>%
        .[, Time := cld@time[Time]]
}

# Evaluate for all clusters ####
# cld = readRDS('save/kml.rds')
start = Sys.time(); kml(cld, nbClusters=1:8, nbRedrawing=20, parAlgo=par); runTime = Sys.time() - start
# saveRDS(cld, file='save/kml.rds')

# Select best solution ####
evalGroups = mapply(slot, list(cld), paste0('c', 1:26)) %>%
    lengths() %>%
    {which(. > 0)}
kmls = mapply(slot, list(cld), paste0('c', evalGroups), SIMPLIFY=FALSE) %>%
    lapply(first) %>%
    set_names(evalGroups)

bics = -1 * sapply(kmls, function(part) part@criterionValues['BIC'])
plotMetric(bics, evalGroups, 'BIC')+ expand_limits(y=6e4)
# ggsave('save/kml_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

# Assess the best solution ####
bestK = 7
bestKml = kmls[[as.character(bestK)]]
print(bestKml)

pp = bestKml@postProba %>%
    set_colnames(levels(bestKml@clusters))
groupProps =colMeans(pp) %T>% print()

computeKmlTrajectories(bestK) %>% plotGroupTrajectories(groupProps)
# ggsave('save/kml_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')

relativeEntropy(pp)
confusionMatrix(pp) %>% round(4)
