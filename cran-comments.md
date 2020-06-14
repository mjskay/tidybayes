## Submission comments
This is a new submission as part of splitting tidybayes (already on CRAN) into two parts.
The tidybayes package has grown into two related, large pieces of functionality: (1) functions
for visualizing distributions and uncertainty, and (2) functions for manipulating posteriors
from Bayesian models. Because the visualization functions (category 1) can also be applied to
non-Bayesian models (and have become a large-ish API unto themselves), I created ggdist to
contain all of those functions.

This new version of tidybayes depends on ggdist for the visualization functions, so there
is only one implementation of them across the two packages. Tidybayes now re-exports 
the visualization functions from ggdist so that existing code that depends on tidybayes
is not affected.

## Test environments
* Windows 10 (local), R-release 4.0.0
* Windows 10 (local), R-devel 2020-06-03 r78637
* Windows (github), R-release 4.0.1
* Windows (github), R-devel 2020-06-12 r78687
* MacOs (github), R-release 4.0.0
* Linux (github), R-release 4.0.1

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are two downstream dependencies for this package. Both 
have as many or fewer errors with the new version of tidybayes.

- mcp:
  - on CRAN:     1 error , 0 warnings, 0 notes
  - new version: 1 error , 0 warnings, 0 notes

- trialr:
  - on CRAN:     0 errors, 0 warnings, 2 notes
  - new version: 0 errors, 0 warnings, 2 notes
