# predictiveMargins

Package for calculating predictive margins with random forests in R.

Note that this package currently only works with forests fit using the `{ranger}` package. 

To install the package:

```
remotes::install_github("jasongraf1/predictiveMargins")
```

Information on how to use the package can be found in the vignette: `vignettes/using_predictive_margins.html` 

--------------

In development:

- Adapting package to work with `{party}` (and `{randomForest}`?) package
- Adapting to work with *n* > 2 outcomes [**DONE**]
- Adapting to work with continuous outcomes
