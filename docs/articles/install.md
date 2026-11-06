# Installing cityClimateHealth

To access “cityClimateHealth”, users must first install CmdStanR, a
command-line interface for Stan in R that translates statistical models
into R code to execute Bayesian inference methods.

To avoid memory limits or conflicts from other packages, it is best to
run a new R session, or restart the current R session.

``` r

install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))
```

After you’ve install `cmdstanr` try the `install_cmdstan()` function to
download STAN.

If you’re on MAC, you may need to install xcode.

``` bash
xcode-select --install
```

Now, install from CRAN

``` r

install.packages('cityClimateHealth')
```
