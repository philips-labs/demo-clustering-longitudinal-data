library(lcmm)
data = generate_osa_data() %>%
    .[, Id := as.integer(Id)] %>%
    .[, NormTime := (Time - min(Time)) / (max(Time) - min(Time))]

makeGmmCall = function(k) {
    substitute(
        hlme(fixed=Usage ~ NormTime + I(NormTime^2),
             mixture=~NormTime + I(NormTime^2),
             random=~NormTime,
             idiag=TRUE,
             nwg=TRUE,
             subject='Id', ng=k, data=data),
        env=list(k=k)
    )
}

computeHlmeTrajectories = function(model) {
    times = sort(unique(data$Time))
    normTimes = sort(unique(data$NormTime))
    predictY(model, newdata=data.frame(NormTime=normTimes))$pred %>%
        data.table(Time=times) %>%
        melt(id.vars='Time', value.name='Usage', variable.name='Group') %>%
        .[, Group := factor(Group, levels=paste0('Ypred_class', 1:model$ng), labels=LETTERS[1:model$ng])] %>%
        .[]
}

# Estimation ####
gmms = list()
# gmms = readRDS('save/gmm.rds')
gmms[['1']] = hlme(fixed=Usage ~ NormTime + I(NormTime^2), random=~NormTime, subject='Id', idiag=TRUE, ng=1, data=data)

fitGmm = function(k) {
    set.seed(1)
    start = Sys.time()
    model = do.call(gridsearch, list(m=makeGmmCall(k), rep=20, maxiter=1, minit=gmms[['1']]))
    model$runTime = Sys.time() - start
    return(model)
}

gmms[['2']] = fitGmm(2)
gmms[['3']] = fitGmm(3)
gmms[['4']] = fitGmm(4)
gmms[['5']] = fitGmm(5)
gmms[['6']] = fitGmm(6)
gmms[['7']] = fitGmm(7)
gmms[['8']] = fitGmm(8) #nwg=TRUE takes 60s per iter as opposed to 7s
# saveRDS(gmms, file='save/gmm.rds')

# Solutions ####
plotMetric(sapply(gmms, '[[', 'BIC'), as.integer(names(gmms)), 'BIC')
# ggsave('save/gmm_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

# Assess the best solution ####
k = 7
bestGmm = gmms[[as.character(k)]]

pp = bestGmm$pprob[paste0('prob', 1:k)] %>%
    as.matrix() %>%
    set_colnames(LETTERS[1:k])
groupProps = colMeans(pp) %T>% print()

computeHlmeTrajectories(bestGmm) %>% plotGroupTrajectories(groupProps)
# ggsave('save/gmm_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')

appa(pp)
relativeEntropy(pp)
