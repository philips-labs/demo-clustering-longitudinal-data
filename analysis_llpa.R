library(mclust)
data = generate_osa_data()
dataMat = transformToRepeatedMeasures(data)

fitLlpa = function(k) {
    cat('Fitting for k =', k, '\n')
    start = Sys.time()
    bestModel <<- NULL
    for(n in 1:10) {
        cat(sprintf('fit %d ', n))
        model <<- Mclust(dataMat, G=k, modelNames='VVI',
                         prior=priorControl(functionName='defaultPrior'),
                         verbose=FALSE)

        if(!is.null(model)) {
            if(is.null(bestModel) || model$bic > bestModel$bic) {
                bestModel <<- model
            }
        } else {
            cat('conv error')
        }
        cat('\n')
    }
    if(is.null(bestModel)) {
        bestModel = list(bic=NA)
    }
    bestModel$runTime = Sys.time() - start
    return(bestModel)
}

computeLlpaTrajectories = function(model) {
    times = sort(unique(data$Time))
    model$parameters$mean %>%
        set_rownames(NULL) %>%
        set_colnames(LETTERS[seq_len(model$G)]) %>%
        melt(varnames=c('Time', 'Group'), value.name='Usage') %>%
        as.data.table() %>%
        .[, Time := times[Time]] %>%
        .[]
}

# Estimation ####
llpas = list()
# llpas = readRDS('save/llpa.rds')
llpas[['1']] = fitLlpa(1)
llpas[['2']] = fitLlpa(2)
llpas[['3']] = fitLlpa(3)
llpas[['4']] = fitLlpa(4)
llpas[['5']] = fitLlpa(5)
llpas[['6']] = fitLlpa(6)
llpas[['7']] = fitLlpa(7)
llpas[['8']] = fitLlpa(8)

# saveRDS(llpas, file='save/llpa.rds')

# Solutions ####
plotMetric(-1 * sapply(llpas, '[[', 'bic'), as.integer(names(llpas)), 'BIC')
# ggsave('save/llpa_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

# Assess the best solution ####
k = 5
bestLlpa = llpas[[as.character(k)]]

pp = bestLlpa$z %>%
    set_colnames(LETTERS[1:k])
groupProps = colMeans(pp) %T>% print()

computeLlpaTrajectories(bestLlpa) %>% plotGroupTrajectories(groupProps)
# ggsave('save/llpa_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')


appa(pp)
relativeEntropy(pp)
