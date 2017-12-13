# Bug: Creating multiple plots and tables with a loop


```r
library(foreach)
```

```
## Warning: package 'foreach' was built under R version 3.4.1
```

```r
library(formattable)
```

```
## Warning: package 'formattable' was built under R version 3.4.1
```

# This does not Work.


```r
withVisible(
for (i in 1:5)
{
  print(i)
  formattable(pressure, align ="l")
  cat('\n')
  plot(pressure)
  cat('\n')
})
```

```
## [1] 1
```

```
## 
## [1] 2
```

![](loop_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```
## 
## [1] 3
```

```
## 
## [1] 4
```

```
## 
## [1] 5
```

```
## $value
## NULL
## 
## $visible
## [1] FALSE
```

# This does not Work, too.


```r
foreach(i = 1:5) %do%
{
  print(i)
  formattable(pressure, align ="l")
  cat('\n')
  plot(pressure)
  cat('\n')
}
```

```
## [1] 1
```

```
## 
## [1] 2
```

![](loop_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```
## 
## [1] 3
```

```
## 
## [1] 4
```

```
## 
## [1] 5
```

```
## [[1]]
## NULL
## 
## [[2]]
## NULL
## 
## [[3]]
## NULL
## 
## [[4]]
## NULL
## 
## [[5]]
## NULL
```
