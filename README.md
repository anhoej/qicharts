# Quality improvement charts

The **qicharts** package contains functions for plotting run charts and basic Shewhart control charts for measure and count data.

The main function is `qic()`, which in its simplest form takes a vector of values, plots a run chart, and performs tests for non-random variation in the data sequence. Non-random variation may be the result of deliberate process improvement or unintentional, possibly unwanted, changes in process performance.

`qic()` has a simple interface with a rich set of options to control data analysis and plotting, including options for automatic data aggregation by subgroups, easy analysis of before-and-after data, exclusion of one or more data points from analysis, and splitting charts into sequential time periods. Missing values and empty subgroups are handled gracefully.

The `trc()` function creates trellis run charts, i.e. small multiple run charts from multivariate data.

The `paretochart()` function plots a pareto chart from categorical data.

For quick demos, run
```
example(qic)
example(trc)
example(paretochart)
vignette('runcharts')
```
Then read the help files
```
?qic
?trc
?paretochart
```
