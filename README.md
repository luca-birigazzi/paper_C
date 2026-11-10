# Interactive figures

Supplementary interactive figures for *[paper title]*, [authors].

For a population of N = 3 units with fixed mean μ and variance σ², the deviation
vectors z = y − μ1 lie on the circle S_σ: the intersection of the mean-zero plane
H = {z : 1ᵀz = 0} with the sphere ‖z‖ = √N σ. The figures show how the maximum of
a linear function cᵀz over S_σ is found by projecting c onto H.

| Figure | File | Shows |
|---|---|---|
| Single unit | `single-unit.html` | c = e₃; maximum z₃* = σ√(N−1) |
| Mean of a subset | `subset-mean.html` | c = c_A = (1/M) 1_A, A = {1, 2}; maximum z̄_A* = σ√((N−M)/M) |

Open `index.html` (or the published site) in a browser. Drag to rotate, scroll to
zoom, and hover on z* for its coordinates.

## Reproducing the figures

Requires R with `plotly` and `htmlwidgets`, and pandoc (included with RStudio) for
self-contained HTML:

```r
install.packages(c("plotly", "htmlwidgets"))
source("R/hyperplane_sphere_N3_plotly.R")        # -> hyperplane_sphere_N3_plotly.html
source("R/subset_mean_difference_N3_plotly.R")   # -> subset_mean_N3_plotly.html
```

Rename the outputs to `single-unit.html` and `subset-mean.html`. The second script
also writes a figure for the difference of two subset means, which is not part of
this site yet.
