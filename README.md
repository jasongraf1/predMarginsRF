# predictiveMargins

Package for calculating predictive margins with random forests in R.

Note that this package is currently in development, and only works with forests fit using the `{ranger}` and `{party}` packages. 

To install the package:

```
remotes::install_github("jasongraf1/predictiveMargins")
```

Information on how to use the package can be found in the vignette: `vignettes/using_predictive_margins.html` 

--------------

In development:

- Streamline code for `{party}` forests, which is not very efficient
- Adapting to work with continuous outcomes
