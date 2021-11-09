data = generate_osa_data()
plotTrajectories(data)
# ggsave('save/trajectories.pdf', width=10.5, height=6, units='cm')

plotGroupTrajectories(data) +
    guides(shape=FALSE)
# ggsave('save/groups.pdf', width=6, height=6, units='cm')

plotGroupTrajectories(data)

ggplot(data, aes(x=Usage)) +
    geom_histogram(aes(y=..count../sum(..count..)), breaks=seq(0, 12, by=.5), color='black', fill='gray') +
    scale_x_continuous(breaks=seq(0, 12, by=2)) +
    scale_y_continuous(breaks=pretty_breaks(8), labels=percent) +
    labs(x='Hours of Use', y='Observations')


# View individual trajectories
plotTrajectory(data)
