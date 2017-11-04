---
title: Example Report
author: You
output: html_document
---

Look how I read outputs from the drake cache."
Drake notices that `small`, `coef_regression2_small`,
and `large` are dependencies of the
future compiled output report file target, `report.md`.
Just be sure that the workflow plan command for the target `'report.md'`
has an explicit call to `knit()`, something like `knit('report.Rmd')` or
`knitr::knit(input = 'report.Rmd', quiet = TRUE)`.


```r
library(drake)
readd(small)
```

```
## cache C:/Users/c240390/AppData/Local/Temp/RtmpILkxfE/file45833c2c4f/.drake
```

```
##            x y
## 1 -1.1723603 0
## 2  0.4791348 0
## 3 -1.2633875 1
## 4 -2.3645150 0
## 5 -1.8411492 1
```

```r
readd(coef_regression2_small)
```

```
## cache C:/Users/c240390/AppData/Local/Temp/RtmpILkxfE/file45833c2c4f/.drake
```

```
## (Intercept)          x2 
## 0.384241253 0.006468626
```

```r
loadd(large)
```

```
## cache C:/Users/c240390/AppData/Local/Temp/RtmpILkxfE/file45833c2c4f/.drake
```

```r
head(large)
```

```
##             x y
## 1 -0.91686280 1
## 2  1.61519584 0
## 3  0.06924194 0
## 4  0.35407128 2
## 5  0.05360384 0
## 6 -0.23906365 4
```
