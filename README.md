## What is this?
some html animatied demos for stats classes, using R and Rmarkdown
See them in action at 

## What demos are included
- The effect of alpha level on power
- The effect of sample size of power
- The effect of sample standard deviation on power
- The effect of effect size on power
- The effect outliers have on regression lines
- A simulated sampling distribution demonstrating the central limit theorem

## What do I need to build them for myself?
- System requirements:
  - R
  - pandoc (>= 1.12.3)
- R package requirements
  - animation
  - knitr
  - rmarkdown
  - pander
  - ggplot2
  - grid
  - gridExtra

## How do I build them?
1. Start an R session with an interactive command line
2. Set the working directory to the directory with build_demos.R in it (the top level dir)
3. source("build_demos.R")
4. build_demos()
5. by default output will be in html_output directory
6. Open html_output/demos.html, follow links to the demos, and be amazed

To only build some demos (they can take a while, ggplot-ing is slow), give build_demos() a character vector specifying which to build.
See comments inside build_demos() for more info. 


