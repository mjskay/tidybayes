# tidybayes 3.0.2

* Future-proof against changes coming in ggdist 3.1 (unification of `stat_...` and `stat_dist_...`)
* Minor fixes for R 3.5/3.6 compatibility
* Use `@examplesIf` for conditional examples (#294)
* Warn when input names in `compose_data()` are overwritten by generated names (#295)


# tidybayes 3.0.1

Minor update for compatibility with brms 2.16.0.


# tidybayes 3.0.0

Breaking changes and deprecations:

* The `[add_]XXX_draws()` (`predicted_draws()`, `add_predicted_draws()`, etc)
  functions have been substantially restructured:
  * `add_fitted_draws()` and `fitted_draws()` are now deprecated, along with the
    `scale` argument. Several years' teaching experience has demonstrated that
    "fitted" is a very confusing name for students. Use the more-specific 
    [`add_`]`linpred_draws()` if you want draws from the linear predictor or the new
    [`add_`]`epred_draws()` if you want draws from the expectation of the
    posterior predictive (which is what `fitted_draws()` was most typically
    used for).
  * Arguments for renaming output columns are now all called `value`, but
    retain function-specific default column names. E.g. the `prediction` argument
    for `predicted_draws()` is now spelled `value` but has a default of `".prediction"`.
    One breaking change is that the default output column for `linpred_draws()`
    is now `".linpred"` instead of `".value"`. This should make it easier to
    combine outputs across multiple functions while also making it easier to
    remember the name of the argument that changes the output column name.
  * The `n` argument is now spelled `ndraws` to be more consistent with 
    terminology in the `posterior` package and to prevent partial argument
    name matching bugs with `newdata`.
  * The first argument to all of these functions is now `object` instead of
    `model`, in order to match with argument names in `posterior_predict()`, etc.
    This was necessary to prevent partial argument name matching bugs with
    certain model types in `rstanarm` that have an `m` argument to their
    prediction functions.

New features:

* Support for the new `posterior` package:
  * Several new `_rvars` counterparts to the `_draws` family of functions, 
    including `spread_rvars()`, `gather_rvars()`, `epred_rvars()`, `linpred_rvars()`,
    and `predicted_rvars()`, which add columns of `posterior::rvar()` objects
    to data frames instead of long-format columns of draws. These can be
    easier to read and take up less memory than the long-format data frames
    of draws. See `vignette("tidy-posterior")` for examples.
  * The `nest_rvars()` and `unnest_rvars()` functions for converting between
    data frames of `rvar`s and long format data frames of draws.
  * `tidy_draws()` has been rebuilt on top of `posterior::as_draws_df()`, which
    means it should support even more model types and benefit from efficiency 
    improvements in `posterior`. This means that `cmdstanr` is now supported,
    for example.
  * An implementation of `posterior::summarise_draws()` for grouped data frames
    of draws: `summarise_draws.grouped_df()`
  * `compare_levels()` now supports data frames of `posterior::rvar()`s.
* The `epred_draws()`, `linpred_draws()`, and `predicted_draws()` functions should
  now support any models that implement `posterior_epred()`, `posterior_linpred()`,
  and `posterior_predict()` so long as they take a `newdata` argument.
* Several dependencies have been removed or demoted to *Suggests*, including
  `forcats`, `plyr`, and `purrr`.
* Added the option to set the `seed` when subsampling to several functions (#276).

New documentation:

* `vignette("tidy-posterior")` describing the use of `tidybayes` with `posterior`,
  and particularly the `posterior::rvar()` data type. This vignette also includes
  an updated version of the ordinal regression example from `vignette("tidy-brms")`,
  now with an illustration of the relationship between the latent linear predictor
  and the category-level probabilities.

Bug fixes:

* Omit sampler parameters in `tidy_draws()` if retrieving them results in an error (#244)


# tidybayes 2.3.1

* Allow user-specified names for comparisons in `compare_levels()` (#272)
* Allow contrasts from the `emmeans` package to be used with `compare_levels()`
  via the new `emmeans_comparison()` function (#272)


# tidybayes 2.1.1

* Initial split of `tidybayes` into `tidybayes` and `ggdist` (#250). `ggdist` now contains all stats/geoms from
  tidybayes (except deprecated ones), all support functions for stats/geoms (such as `point_interval()`),
  `vignette("slabinterval")`, and `vignette("freq-uncertainty-vis")`. `tidybayes` retains all other functions,
  and re-exports all `ggdist` functions for now.
* All stats and geoms now support automatic orientation detection (#257), following the new automatic orientation
  detection approach in `ggplot2`. If needed, this can be overridden using the existing `orientation` parameter.
  * All `h`-suffix geoms are now deprecated. The `h`-suffix geoms have been left in `tidybayes` and give a 
    deprecation warning when used; they cannot be used from `ggdist` directly.
  * The `h`-suffix `point_interval()` functions are also deprecated, since they are not needed in tidybayes
    nor in `ggplot2::stat_summary()`.
  * `geom_interval()`, `geom_pointinterval()`, and `geom_lineribbon()` no longer automatically set the 
    `ymin` and `ymax` aesthetics if `.lower` or `.upper` are present in the data. This allows them to work
    better with automatic orientation detection (and was a bad feature to have existed in the first place
    anyway). The deprecated `tidybayes::geom_intervalh()` and `tidybayes::geom_pointintervalh()` still
    automatically set those aesthetics, since they are deprecated anyway (so supporting the old behavior
    is fine in these functions).
* `geom_lineribbon()`/`stat_lineribbon()` now supports a `step` argument for creating stepped lineribbons (#249).
  H/T to Solomon Kurz for the suggestion.
* `ggdist` now has its own implementation of the scaled and shifted Student's t distribution (`dstudent_t()`,
  `qstudent_t()`, etc), since it is very useful for visualizing confidence distributions. That is re-exported
  in `tidybayes` as well`.
* All deprecated functions and geoms now throw deprecation warnings (previously, several deprecated functions
  did not). 


# tidybayes 2.0.3

Minor fixes for changes in tibble 3.0.0


# tidybayes 2.0.2

Various minor forward and backward compatibility fixes:

* Fix `stringsAsFactors` issues for R 4
* Fix issues with `[[<-` for R 4
* Fix minor issues with dplyr 1.0.0
* Use `parse()` instead of `str2lang()` for compatibility with R <= 3.6


# tidybayes 2.0.1

* Various geoms and stats have been merged together under the `geom_slabinterval()` and `stat_slabinterval()`
  "meta-geom" (#84). This has enabled a bunch of new geoms to be created (see `vignette("slabinterval")` 
  and fixed a number of outstanding issues:
  * Histogram geoms and histogram+interval geoms (#162)
  * CCDF bar charts and gradient plots
  * The alpha aesthetic can now be mapped on eye plots (and all related geoms) (#163)
  * Vertical version of eye plot (and vertical/horizontal variants of all slabinterval variants) (#56)
  * Intervals and densities are now correctly grouped in eye plots (e.g. when dodging) (#83)
  * Fill and color aesthetics can now be mapped within the slab part of eyes (and all slabintervals), allowing 
    gradients to be made easily (#136) and regions of practical equivalence (ROPEs) to be annotated easily.
    Examples of ROPEs have been added to the main vignettes (#129).
  * Intervals and eyes support `position = "dodge"` correctly (#180)
  * The new geoms (and replacements for old ones) have custom scales allowing fine-grained targeting of fill, 
    color, and size aesthetics of all the component parts of the composite geoms.
  * There is a new sub-family of auto-sizing Wilkinson dotplot stats and geoms, `geom_dots()` and `geom_dotsinterval()` 
    (#210). These include a `quantiles` parameter on the stats to make it easy to create quantile dotplots.
* Analytical distributions can be visualized using the new `stat_dist_...` family of geoms for both
  the `geom_slabinterval()` family and `geom_lineribbon()` (see `stat_dist_slabinterval()` and `stat_dist_lineribbon()`).
* The new `parse_dist()`, which parses distribution specifications (like `normal(0,1)`) into tidy columns, can
  be combined with the `stat_dist_...` family of geoms to easily to visualize priors (e.g. from `brms`).
* New distribution functions for the marginal LKJ distribution (`dlkjcorr_marginal()` and company), combined
  with `parse_dist()` and the `stat_dist_...` family make it easy to visualize the marginal LKJ prior on 
  a cell in a correlation matrix. (#191 #192)
* There is a new vignette on frequentist uncertainty visualization, `vignette("freq-uncertainty-vis")`,
  also made possible by the new `stat_dist_...` family of geoms (#188)
* `tidy_draws()` can now be applied to already-tidied data frames, allowing dependent functions (like `spread_draws()`
  and `gather_draws()`) to also be applied to data frames directly (#82). This can be a useful optimization in workflows 
  where the initial tidying is slow but spreading/gathering is fast (see discussion in #144)
* Kruschke-style distribution-of-distribution plots are now easier to construct with `stat_dist_slabh()`.
  An example of this usage is in `vignette("tidy-brms")`.
* `hdi()` now uses trimmed densities by default to avoid odd behavior with bounded distributions (#165).
* `compare_levels(comparison = )` now uses a modern tidy approach to dealing with unevaluated expressions,
  so `rlang::exprs()` can be used in place of `plyr::.()` (#174, #175)
* `geom_lineribbon()` now works with `ggnewscale` (#178)
* `fitted_draws()`/`predicted_draws()` give more helpful error messages on unsupported models (#177)


# tidybayes 1.1.0

New features and documentation:

* Support matrices, n-d arrays, and lists of vectors in compose_data (#159)
* Support nested vectors, matrices, n-d arrays, and ragged arrays through x[.,.] syntax in gather/spread_draws (#154)
* Add detached-line-ribbon HOPs example for ordinal models in brms vignette

Bug fixes:

* Fixed errors on CRAN from changes in brms
* Properly handle Dirichlet responses in predicted_draws (#164)


# tidybayes 1.0.4

New features and documentation:

* Initial support for add_residual_draws, towards #133
* Add tidybayes-residuals vignette
* Add add_draws to support models that add_[fitted|predicted]_draws does not (closes #149) 
* Add sample_draws to make it easier to take fewer draws anywhere in the pipeline (towards #144)
* Add hypothetical outcome plots (HOPs) to examples

Minor changes:

* Fixed errors on CRAN from changes in dplyr
* Fix bug to support multivariate models in `predicted_draws()`, closes #134
* Add support for `emm_list` in `gather_emmeans_draws()`, closes #126
* Default for show.legend no longer omits all guides
* Make default `geom_lineribbon()` color black, closes #153


# tidybayes 1.0.3

* Added `gather_pairs` method for creating custom scatterplot matrices (and more!) 
* Ordinal models in brms now use original category labels (#122)
* `NA` values are now better supported in `point_interval`, and it has an `na.rm` argument (#123)
* Added sampler diagnostics to tidy_draws() Stan output (#109)
* Added MCMCglmm+emmeans example to vignettes
* Add guards to prevent usage of packages listed in `Suggests`


# tidybayes 1.0.0

Major changes:

* First CRAN release.
* Various function, argument, and column name changes towards unification
with the Stan ecosystem. See help("tidybayes-deprecated") for more information.
