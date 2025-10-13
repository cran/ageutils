litedown::reactor(print = NA)

library(ageutils)

cut_ages(ages = 0:9, breaks = c(0, 3, 5, 10))
cut_ages(ages = 0:9, breaks = c(0, 5))

cut_ages(ages = 0:10, breaks = c(0, 5), max_upper = 7)

ages <- seq.int(from = 0, by = 10, length.out = 10)
breaks <- c(0, 1, 10, 30)
cut_ages(ages, breaks)
cut(ages, right = FALSE, breaks = c(breaks, Inf))

breaks_to_interval(breaks = c(0, 1, 5, 15, 25, 45, 65))
breaks_to_interval(
    breaks = c(0, 1, 5, 15, 25, 45, 65),
    max_upper = 100
)

head(pop_dat, 20)

dat <- subset(pop_dat, select = c(age_category, value))
dat <- transform(
    dat,
    lower_bound = as.integer(sub("\\[([0-9]+), .+)", "\\1", age_category))
)

with(
    dat,
    reaggregate_counts(
        bounds = lower_bound,
        counts = value,
        new_bounds = c(0, 1, 5, 15, 25, 45, 65)
    )
)

reaggregate_counts(
    bounds             = c(0, 60),
    counts             = c(400, 600),
    new_bounds         = seq(from = 0, to = 90, by = 10),
    population_bounds  = dat$lower_bound,
    population_weights = dat$value
)

reaggregate_rates(
    bounds = c(0, 5, 10),
    rates = c(0.1, 0.2, 0.3),
    new_bounds = c(0, 2, 7, 10),
    population_bounds = dat$lower_bound,
    population_weights = dat$value
)
reaggregate_rates(
    bounds = 0:99,
    rates = rep(seq(25, 5, -5), each = 20),
    new_bounds = c(0, 5, 15, 45, 65),
    population_bounds = dat$lower_bound,
    population_weights = dat$value
)

