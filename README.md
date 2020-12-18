# geoharmonise

The `geoharmonise` package allows you to correct errors in geographical area names within your data.

## Overview

This package provides functions that allow you to compare a set of geographical area names to those held on the ONS Open Geography Portal: https://geoportal.statistics.gov.uk/

### Installation :
```r
devtools::install_github("moj-analytical-services/geoharmonise")
```

## Available functions

### ONS_geolist()

ONS area names are regularly updated. This function will return a vector of dates for which there is a corresponding ONS dataset of names for a specified geographical area type.

```r
datelist <- ONS_geolist(are_type = "Local Authority", date = "31-Dec-20")
```

This will return a vector of dates for which there is a valid ONS dataset of Local Authority names. The `date` parameter should correspond to the date of the dataset you wish to check.
It will filter this list to only those that precede this date. If no `date` is entered, this list will be unfiltered.

### geocheck()

This function will compare a sumbitted vector of area names to a specified ONS list.

```r
checklist <- geocheck(names = data$local_authority, area_type = "Local Authority", ONS_filedate = "01-Apr-20")
```

It will return a list containing 3 elements:
* `matches`: A data frame of names that successfully match, alongside a label of whether the match is 'Exact' or 'Partial'
* `unmatched_names`: A vector of names from the submitted set of names for which there was no match in the ONS file
* `unmatched_ONS`: A vector of names from the specified ONS file for which there is no match in the submitted set of names 

You should use these to make any necessary adjustments to your data. This will most likely involve manually changing the values in `unmatched_names` to those in `unmatched_ONS`.
