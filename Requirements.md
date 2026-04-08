# Requirements & Data Access

---

## Software

This project requires both **R** and **Python**.

### R (version ≥ 4.2)

Download from [cran.r-project.org](https://cran.r-project.org)

RStudio is optional but strongly recommended. [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop)

Install the required R packages by running this once in your R console:

```r
install.packages(c(
  "tidyverse",        
  "arrow",           
  "rdrobust",        
  "rddensity",        
  "DoubleML",         
  "mlr3",             
  "mlr3learners",
  "ranger",           
  "fixest",          
  "marginaleffects",  
  "kmodR",           
  "sf",               
  "tmap"             
))
```

> If a package is missing at runtime, R will throw `there is no package called 'X'`. Run `install.packages("X")` and retry.

### Python (version ≥ 3.9)

Download from [python.org/downloads](https://www.python.org/downloads)

Used for descriptive statistics. Install the required packages with:

```bash
pip install pandas
pip install numpy
pip install geopandas
pip install matplotlib
pip install pyarrow
pip install json
```

---

## Viewing the HTML output

The descriptive statistics script produces an **HTML file** that must be **downloaded and opened locally in your browser**. GitHub does not render HTML files. To view it correctly, one must download the file then open it by double-clicking or dragging it into a browser window.

---

## Data

Due to confidentiality, **the data files are not included in this repository.** 

### Sources

The analysis combines two datasets provided by [Agreste](https://agreste.agriculture.gouv.fr), the statistical service of the French Ministry of Agriculture:

**Agricultural Census 2020 — *Recensement Agricole 2020***

A comprehensive, exhaustive survey of all French agricultural holdings conducted approximately every 10 years. It covers farm size, type of production, equipment, labour, and legal status. This census forms the cross-sectional baseline of our analysis.

**ESEA Survey 2023 — *Enquête sur la Structure des Exploitations Agricoles***

A representative sample survey conducted between census years to provide updated and more detailed information (in 2023: machinery, irrigation, structural changes). This survey supplies the 2023 wave of our panel and contains the CAP eligibility status variable used as the treatment.

Together, the census provides an exhaustive baseline while the ESEA survey enables dynamic monitoring of agricultural change. This allows us to track farm exits and output shifts around the 2023 reform.
