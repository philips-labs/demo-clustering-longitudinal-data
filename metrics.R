# Average posterior probability of assignments (APPA)
appa = function(pp) {
    rowMaxs(pp) %>% mean()
}

entropy = function(pp) {
    assert_that(is.matrix(pp), min(pp) >= 0, max(pp) <= 1)
    pp = pmax(pp, .Machine$double.xmin)
    -sum(rowSums(pp * log(pp)))
}

relativeEntropy = function(pp) {
    N = nrow(pp)
    K = ncol(pp)
    1 - entropy(pp) / (N * log(K))
}

confusionMatrix = function(pp) {
    IMIFA::post_conf_mat(pp)
}
