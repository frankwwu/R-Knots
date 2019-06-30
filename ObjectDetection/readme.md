---
title: "Object Detection with image.darknet"
output: 
  html_document: 
    keep_md: yes
---

## image.darknet

https://github.com/bnosac/image/blob/master/image.darknet/R/darknet_models.R

## Installation of image.darknet

<code>
install.packages("devtools")
</code>

<code>
devtools::install_github("bnosac/image", subdir = "image.darknet", build_vignettes = TRUE)
</code>


```r
library(image.darknet)
```

## Creating a modle


```r
yolo_tiny_voc <- image_darknet_model(type = 'detect', 
    model = 'tiny-yolo-voc.cfg', 
    weights = system.file(package='image.darknet', 'models', 'tiny-yolo-voc.weights'), 
    labels = system.file(package='image.darknet', 'include', 'darknet', 'data', 'voc.names'))
```

## The function for object detection


```r
detect <- function(input, output) {
  a <- image_darknet_detect(file = input, object = yolo_tiny_voc, threshold = 0.19)
  
  file.copy('predictions.png', output)
  knitr::include_graphics(c(input, output))
}
```


```r
dir.create(file.path('.', 'output'), showWarnings = FALSE)
```

## Works with a upsidedown cat.


```r
detect('input\\cat-yoga.jpg', 'output\\predictions-cat-yoga.png')
```

<img src="input\cat-yoga.jpg" width="564" /><img src="output\predictions-cat-yoga.png" width="564" />

## Works well with a little bit complicated scene.


```r
detect('input\\riding.jpg', 'output\\predictions-riding.png')
```

<img src="input\riding.jpg" width="564" /><img src="output\predictions-riding.png" width="564" />

## Does not work with a person in white.


```r
detect('input\\wedding.jpg', 'output\\predictions-wedding.png')
```

<img src="input\wedding.jpg" width="675" /><img src="output\predictions-wedding.png" width="675" />

## Does not work with a dark person and a partially covered car.


```r
detect('input\\zx.jpg', 'output\\predictions-zx.png')
```

<img src="input\zx.jpg" width="549" /><img src="output\predictions-zx.png" width="549" />

## Can AI understand innovation?


```r
detect('input\\cargo-bike-1.jpg', 'output\\predictions-cargo-bike-1.png')
```

<img src="input\cargo-bike-1.jpg" width="1200" /><img src="output\predictions-cargo-bike-1.png" width="1200" />

## No, Lenin is not a cat!


```r
detect('input\\berlin.jpg', 'output\\predictions-berlin.png')
```

<img src="input\berlin.jpg" width="1024" /><img src="output\predictions-berlin.png" width="1024" />
