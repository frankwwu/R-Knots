Read .for file with read.fwf
========================================================

The .for file is a fixed width file. It takes several tries to read it. First use read.csv() to read in the file then examine its format.


```r
path<-"http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"
d<-read.csv(path)
head(d)
```

```
##                 Weekly.SST.data.starts.week.centered.on.3Jan1990
## 1                  Nino1+2      Nino3        Nino34        Nino4
## 2  Week          SST SSTA     SST SSTA     SST SSTA     SST SSTA
## 3  03JAN1990     23.4-0.4     25.1-0.3     26.6 0.0     28.6 0.3
## 4  10JAN1990     23.4-0.8     25.2-0.3     26.6 0.1     28.6 0.3
## 5  17JAN1990     24.2-0.3     25.3-0.3     26.5-0.1     28.6 0.3
## 6  24JAN1990     24.4-0.5     25.5-0.4     26.5-0.1     28.4 0.2
```

From the result, it appears we need to skip a few lines of text. And we can easily figure out each column widths. To keep things simple, we will not read column headers. Now we can specify these parameters in the read.fwf().


```r
path<-"http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for"
df=read.fwf(file=url(path), skip=4, widths=c(12,7,4,9,4,9,4,9,4))
head(df)
```

```
##             V1   V2   V3   V4   V5   V6   V7   V8  V9
## 1  03JAN1990   23.4 -0.4 25.1 -0.3 26.6  0.0 28.6 0.3
## 2  10JAN1990   23.4 -0.8 25.2 -0.3 26.6  0.1 28.6 0.3
## 3  17JAN1990   24.2 -0.3 25.3 -0.3 26.5 -0.1 28.6 0.3
## 4  24JAN1990   24.4 -0.5 25.5 -0.4 26.5 -0.1 28.4 0.2
## 5  31JAN1990   25.1 -0.2 25.8 -0.2 26.7  0.1 28.4 0.2
## 6  07FEB1990   25.8  0.2 26.1 -0.1 26.8  0.1 28.4 0.3
```

The result is exactly what we wanted.

