source('MixTVEM.R')
data = generate_osa_data() %>%
    .[, NormTime := (Time - min(Time)) / (max(Time) - min(Time))]

fitMixTvem = function(k) {
    start = Sys.time()
    if(k == 1) {
        # needed to prevent consistent estimation errors for the single-group solution
        y = ifelse(data$Usage == 0, rnorm(nrow(data), sd=.1), data$Usage)
    } else {
        y = data$Usage
    }
    model = TVEMMixNormal(dep=y,
                  id=data$Id,
                  numInteriorKnots=6,
                  deg=3,
                  doPlot=FALSE,
                  numClasses=k,
                  numStarts=20,
                  gridSize=365,
                  maxVarianceRatio=NA,
                  convergenceCriterion=1e-4,
                  getSEs=FALSE,
                  tcov=rep(1, nrow(data)),
                  time=data$NormTime)
    model$runTime = Sys.time() - start
    return(model)
}

computeMixTvemTrajectories = function(model) {
    k = ncol(model$fittedValues)
    times = model$timeGrid * (max(data$Time) - min(data$Time)) + min(data$Time)
    data.table(model$betaByGrid[[1]], Time=times) %>%
        setnames(paste0('V', 1:k), LETTERS[1:k]) %>%
        melt(id.vars='Time', value.name='Usage', variable.name='Group')
}

# mixtvems = readRDS(file='save/mixtvems.rds')
mixtvems = list()
mixtvems[['1']] = fitMixTvem(1)
mixtvems[['2']] = fitMixTvem(2)
mixtvems[['3']] = fitMixTvem(3)
mixtvems[['4']] = fitMixTvem(4)
mixtvems[['5']] = fitMixTvem(5)
mixtvems[['6']] = fitMixTvem(6)
mixtvems[['7']] = fitMixTvem(7)
mixtvems[['8']] = fitMixTvem(8)
# saveRDS(mixtvems, file='save/mixtvems.rds')

# Solutions ####
plotMetric(sapply(mixtvems, function(m) m$bestFit$bic), as.integer(names(mixtvems)), 'BIC')
# ggsave('save/mixtvem_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

# Assess the best solution ####
k = 4
bestMixTvem = mixtvems[[as.character(k)]]

pp = bestMixTvem$bestFit$postProbsBySub
groupProps = colMeans(pp) %T>% print()

computeMixTvemTrajectories(bestMixTvem) %>% plotGroupTrajectories(groupProps)
# ggsave('save/mixtvem_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')

relativeEntropy(pp)
confusionMatrix(pp) %>% {round(. * 100, .1)}
