# Strings


```r
x<-"x"
y<-"y"
paste(x ,y)
```

```
## [1] "x y"
```

```r
paste(x ,y, sep="")
```

```
## [1] "xy"
```

```r
paste(x ,y, sep="&")
```

```
## [1] "x&y"
```



```r
print(x, quote = FALSE)
```

```
## [1] x
```

```r
print(x, quote = TRUE)
```

```
## [1] "x"
```



```r
cat(x, "with R")
```

```
## x with R
```

```r
cat(x, y)
```

```
## x y
```



```r
library(stringr)
```

```
## Warning: package 'stringr' was built under R version 3.3.3
```

```r
str_dup(x, 3)
```

```
## [1] "xxx"
```


```r
str_pad(x, width = 5)
```

```
## [1] "    x"
```

```r
str_pad(x, width = 5, side = "both")
```

```
## [1] "  x  "
```

```r
str_pad(x, width = 5, pad = "#")
```

```
## [1] "####x"
```

```r
str_pad(x, width = 5, pad = "_", side = "both")
```

```
## [1] "__x__"
```
