library(lcmm)
library(splines)
data = generate_osa_data() %>%
    .[, Id := as.integer(Id)] %>%
    .[, NormTime := (Time - min(Time)) / (max(Time) - min(Time))]

makeGbtmCall = function(k) {
    substitute(
        hlme(fixed=Usage ~ NormTime + I(NormTime^2),
             mixture=~NormTime + I(NormTime^2),
             random=~-1,
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

# Single-group analysis ####
mod00 = hlme(fixed=Usage ~ 1, random=~1, subject='Id', ng=1, data=data)
summary(mod00)
residuals(mod00) %T>% qqnorm %>% qqline

mod11 = hlme(fixed=Usage ~ NormTime, random=~NormTime, subject='Id', ng=1, data=data)
summary(mod11)
residuals(mod11) %T>% qqnorm %>% qqline

mod22 = hlme(fixed=Usage ~ poly(NormTime, 2, raw=TRUE), random=~poly(NormTime, 2, raw=TRUE), subject='Id', ng=1, data=data)
summary(mod22)
residuals(mod22) %T>% qqnorm %>% qqline

mod33 = hlme(fixed=Usage ~ poly(NormTime, 3, raw=TRUE), random=~poly(NormTime, 3, raw=TRUE), subject='Id', ng=1, data=data)
summary(mod33)
residuals(mod33) %T>% qqnorm %>% qqline

modbs = hlme(fixed=Usage ~ bs(NormTime), random=~bs(NormTime), subject='Id', ng=1, data=data)
summary(modbs)
residuals(modbs) %T>% qqnorm %>% qqline

# Estimation ####
gbtms = list()
# gbtms = readRDS('save/gbtm.rds')
gbtms[['1']] = hlme(fixed=Usage ~ NormTime + I(NormTime^2), random=~-1, subject='Id', ng=1, data=data)

fitGbtm = function(k) {
    start = Sys.time()
    model = do.call(gridsearch, list(m=makeGbtmCall(k), rep=20, maxiter=1, minit=gbtms[['1']]))
    model$runTime = Sys.time() - start
    return(model)
}

gbtms[['2']] = fitGbtm(2)
gbtms[['3']] = fitGbtm(3)
gbtms[['4']] = fitGbtm(4)
gbtms[['5']] = fitGbtm(5)
gbtms[['6']] = fitGbtm(6)
gbtms[['7']] = fitGbtm(7)
gbtms[['8']] = fitGbtm(8)
# saveRDS(gbtms, file='save/gbtm.rds')

# Solutions ####
plotMetric(sapply(gbtms, '[[', 'BIC'), as.integer(names(gbtms)), 'BIC')
# ggsave('save/gbtm_bic.pdf', width=bicPlotSize[1], height=bicPlotSize[2], units='cm')

# Assess the best solution ####
k = 4
bestGbtm = gbtms[[as.character(k)]]

pp = bestGbtm$pprob[paste0('prob', 1:k)] %>%
    as.matrix() %>%
    set_colnames(LETTERS[1:k])
groupProps = colMeans(pp) %T>% print()

computeHlmeTrajectories(bestGbtm) %>% plotGroupTrajectories(groupProps)
# ggsave('save/gbtm_groups.pdf', width=groupPlotSize[1], height=groupPlotSize[2], units='cm')

appa(pp)
relativeEntropy(pp)
