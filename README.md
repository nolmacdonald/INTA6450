# INTA6450

Data Analytics and Security - Georgia Tech (Fall 2024)

## Contents

- Module 1 - Introduction
- Module 2 - Hardware Trends
- Module 3 - Software Trends

## Documentation

The documentation can be rendered by setting the working directory to where the `docs` folder is located.
For GitHub pages, use the `gitbook` command.
For a PDF use the `pdf_book` command.

```
setwd("~/projects/INTA6450/docs")
bookdown::render_book("index.Rmd", "bookdown::gitbook")
bookdown::render_book("index.Rmd", "bookdown::pdf_book")
```
