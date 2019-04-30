
[![Travis build status](https://travis-ci.org/nfox29/photosearcher.svg?branch=master)](https://travis-ci.org/nfox29/photosearcher) [![Codecov test coverage](https://codecov.io/gh/nfox29/photosearcher/branch/master/graph/badge.svg)](https://codecov.io/gh/nfox29/photosearcher?branch=master) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) <!-- README.md is generated from README.Rmd. Please edit that file -->

photosearcher
=============

The goal of photosearcher is to provide a repeatable methodology for accessing the Flickr API. More information can be found on the [package website](https://nfox29.github.io/photosearcher/).

Installation
------------

You can install the released version of photosearcher from github with:

``` r
devtools::install_github("nfox29/photosearcher")
```

Getting and API key
-------------------

Before using the package users should sign uo for an API key from the [Flickr development page](https://www.flickr.com/services/apps/create/). The first function of the package you use you will be prompted to enter your API key. The API key will then be saved as a .Rda file and be called to when using any other function

Package functions
-----------------

The package currently focuses on the ability to use the Flickr API to search for images and their metadata throught the flickr.photos.search method. The package does however supporst a number of other Flickr API call methods inlcuding the flickr.tags.getRelated and flickr.places.tagsForPlace methods. The Flickr website offers full [API Documentation](https://www.flickr.com/services/api/) for all of its call methods.

Searching for photographs
-------------------------

These are a few basic example of the main photo\_search function:

``` r
  #Search for photos of foxes in the UK for the year 2017
  foxes <- photo_search(mindate = "2017-01-01",
             maxdate = "2018-01-01",
             text = "foxes",
             bbox = "-12.875977,49.210420,2.636719,59.977005",
             has_geo = TRUE)  

  #Search for images of trees globally for the 1st of January 2019
  trees <- photo_search(mindate = "2019-01-01",
             maxdate = "2019-01-01",
             text = "tree",
             has_geo = TRUE)

  #Search for images with text mountain and tagged lake
  moutain_lake <- photo_search(mindate = "2019-01-01",
             maxdate = "2019-01-01",
             text = "mountain",
             tags = "lake"
             has_geo = FALSE)
```

When the arguement `has_geo` is `TRUE`all images will contain latitude and longitude infomation. These can be plotted using other pakcages at user preference. Here, the images of UK foxes are plotted using `ggplot2`.

[Foxes]()

Downloading images
------------------

``` r
  #Downloading foxes images from photosearch
  download_images(photo_id = foxes$id, saveDir = "Fox images")

  #Download a specific images based off its id
download_images(photo_id = 47416147181)
```

The package will only download images that the owner on Flickr has granted download permission. Those without permission will not be downloaded, but will produce a warning to let know the ID numbers of those skipped. Permission to download does not automatically provide permission to use and distribute, check the photographs licence before use. The `photo_search` function provides the licence information for each image.

Finding information on a user
-----------------------------

``` r
  #Find a users hometown
  user <- user_info(user_id = "22299651@N07")

  user$hometown
```

### Get the top tags for location

``` r
  #Find the tags for Southampton, UK
  southampton <- location_tags(woe_id = 35356)
  
  #Find the tags for New York state, US
  new_york <- location_tags(woe_id =    2347591)
```

The woe\_id arguement is a FLickr specific place identifier. A places woeID can be found using the `find_place` function or using [online tools](http://woeid.rosselliot.co.nz/).

Find a locations woeID
----------------------

``` r
  #Find woeID for Kuala Lumpur
  kuala_lumpur <- find_place(place = "Kuala Lumpur")

  kuala_lumpaur$woeid
```

Finding tags most associated with a tag
---------------------------------------

``` r
  #Find tags associated with zebra
  zebra <- related_terms(term = "zebra")

  #Find tags most assoicated with hiking
  hiking <- related_terms(term = "hiking")
```

Issues and bugs
---------------

This package requires an internet connection as well as a connection to the Flickr API, which may not be constantly available If you discover a bug not associated with connection to the API that is not already [reported issue](https://github.com/nfox29/photosearcher/issues), please [open a new issue](https://github.com/nfox29/photosearcher/issues/new) providing a reproducible example.
