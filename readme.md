# clbrief

clbrief is a small R package for downloading data from the UK Parliament's Research Briefings API.

## Installation

Install from GitHub using remotes.

``` r
install.packages("remotes")
remotes::install_github("olihawkins/clbrief")
```

## Overview

The UK Parliament's [Research Briefings API](http://explore.data.parliament.uk/?learnmore=Research%20Briefings) contains data on research briefings published by the House of Commons Library, the House of Lords Library, and the Parliamentary Office of Science and Technology (POST). You can find the research briefings published at [researchbriefings.parliament.uk](https://researchbriefings.parliament.uk/). This package provides a simple way to access this data.

## Core API

At the heart of the package is a function called `fetch_briefings_json` which downloads data for a given number of the most recent briefings published, based on the requested number of pages of results and a given number of items per page. The API will return a maximum of 500 briefings per page of results and this is the default `pagesize`. The function will throw an error if you use a `pagesize` of more than 500.

To fetch data on the last 100 briefings published:

``` r
briefings_json <- clbrief::fetch_briefings_json(pages = 1, pagesize = 100)
```

To fetch data on the last 1,000 briefings published:

``` r
briefings_json <- clbrief::fetch_briefings_json(pages = 2) # The default pagesize is 500
```

Other functions in the package allow you to extract data from the JSON returned by `fetch_briefings_json` as a tibble.

``` r
briefings <- clbrief::get_briefings(briefings_json)
topics <- clbrief::get_topics(briefings_json)
sections <- clbrief::get_sections(briefings_json)
```

Wrapper functions are provided as a convenience if you are only interested in one particular dataset and wish to download and extract the data in one call.

``` r
briefing_topics <- clbrief::fetch_topics(pages = 1, pagesize = 100)
```

### Fetch JSON

Functions that make calls to the API are prefixed `fetch_*`.

---

_clbrief_::__fetch_briefings_json__(_pages = 1_, _pagesize = 500_, _pause = 1_)

Fetches data on all research briefings up to a given number derived from the number of `pages` of results and the `pagesize`. The `pause` argument is the amount of time the function waits between successive calls to the API. The data returned is a list of lists representing the JSON returned from the API, with one list item per briefing at the top level.

---

### Extract tibbles

Functions that extract and process the downloaded data locally are prefixed `get_*`. These functions return the data as a tibble.

---

_clbrief_::__get_briefings__(_briefings_json_)

Extracts summary data about each research briefing, with one row per briefing.

---

_clbrief_::__get_topics__(_briefings_json_)

Extracts data about the topics covered by each research briefing, with one row per combination of briefing and topic. Note that data on the topic of a briefing is not always provided by the publisher, so _briefings without topics will not be returned from this function_. You can find briefings without topics by looking for all briefing codes that are returned by `get_briefings` that do not appear in the tibble returned from `get_topics` for the same dataset.

---

_clbrief_::__get_sections__(_briefings_json_)

Extracts data about the Parliamentary sections responsible for producing each research briefing, with one row per combination of briefing and section. Note that data on the section responsible for a briefing is not always provided by the publisher, so _briefings without sections will not be returned from this function_. You can find briefings without sections by looking for all briefing codes that are returned by `get_briefings` that do not appear in the tibble returned from `get_sections` for the same dataset.

---

### Fetch JSON and extract tibbles

These functions fetch the requested briefings, extract and process the data, and return the data as a tibble. These are convenience functions: if you wish to extract more than one dataset on a given set of briefings it is more efficient to download the data once with `fetch_briefings_json` and extract each of the relevant datasets from that data.

---

_clbrief_::__fetch_briefings__(_pages = 1_, _pagesize = 500_, _pause = 1_)

Fetches and extracts summary data about each research briefing, with one row per briefing.

---

_clbrief_::__fetch_topics__(_pages = 1_, _pagesize = 500_, _pause = 1_)

Fetches and extracts data about the topics covered by each research briefing, with one row per combination of briefing and topic. Note that data on the topic of a briefing is not always provided by the publisher, so _briefings without topics will not be returned from this function_. You can find briefings without topics by looking for all briefing codes that are returned by `get_briefings` that do not appear in the tibble returned from `get_topics` for the same dataset.

---

_clbrief_::__fetch_sections__(_pages = 1_, _pagesize = 500_, _pause = 1_)

Fetches and extracts data about the Parliamentary sections responsible for producing each research briefing, with one row per combination of briefing and section. Note that data on the section responsible for a briefing is not always provided by the publisher, so _briefings without sections will not be returned from this function_. You can find briefings without sections by looking for all briefing codes that are returned by `get_briefings` that do not appear in the tibble returned from `get_sections` for the same dataset.

---

## Package status

This is an ad-hoc package that was developed to provide quick access to research briefings data. The package does not yet have unit tests, but the functions appear to work as intended. You should satisfy yourself that the functions behave in the way that you expect if you wish to use this package for research purposes.
