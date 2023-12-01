
# Advent of Code R template

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

This is an R version of [YiWen's Advent of Code (AOC) template repository for Python](https://github.com/yiwen-h/aoc_python_template) to help support a Coffee & Coding session, November 2023.

Inspiration is taken from [David Selby's package-based test-driven approach](https://github.com/Selbosh/adventofcode2021) for AOC 2021 and uses [Emil Hvitfeldt's solution](https://github.com/EmilHvitfeldt/rstats-adventofcode) for AOC 2022 as an example (referred to as 'day 00' in this repository).

## Getting set up

1. Please click the 'Use this template' button in this repository, clone the repo, or fork it to your own GitHub account.
2. Install required packages from CRAN with `install.packages(c("devtools", "testthat", "usethis"))` if not already installed. {devtools} and {usethis} help you develop R packages and {testthat} is a popular testing framework for R.

## How to use this template

### Set up the test

If starting a new package, you can use `usethis::use_testthat()` to set up the folder structure required by {testthat}. That's already been done in this repository.

1. Put the test data in a `day-01.txt` file (replacing `01` with the day number) in the `tests/testthat/test-data/` folder.
2. Run `usethis::use_test("day-01")` to generate a `tests/testthat/test-day-01.R` file and write your test.
3. Run `usethis::use_r("day-01.R")` to set up an `R/day-01.R` file and write your function. You could call it `solve_day_01()` or something more descriptive.
4. Run `devtools::load_all()` to load the functions (or use the shortcut <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>L</kbd> in RStudio).
4. Run your test by executing `devtools::test()` in your R console, or by clicking the 'Test' button in the 'Build' pane of RStudio (which has the shortcut <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>T</kbd>). It is normal and ok to fail the test at first!

### Work on your solution

5. Work on improving your function and run your test until your function works.
6. Once your function passes the test, add the 'real' data set as a .txt file to the `inst/` folder.
7. Run your function with the full data as the input.
8. Put your answer into the AOC website - hopefully you've got your star! ⭐

In [David's example](https://github.com/Selbosh/adventofcode2021), he wrote his daily solutions to R scripts in the `inst/` folder and then executed them from his package README file so that the answers would render there.

## Extras

* Note that two utility functions are provided for retrieving data: `load_test_data()`, which is used in your test scripts, and `load_real_data()`, which you should use to get your final answer. 
* It's an exercise for the user to recreate [YiWen's 'fancy' approach](https://github.com/yiwen-h/aoc_python_template#optional-fancy-api-way-of-getting-the-real-data) to fetching AOC data from the API. This would mean you no longer have to copy-paste any data.
* Since you are likely to be the only user of your package, you don't necessarily need to do the full package setup as described in resources like [Hadley Wickham's R Packages book](https://r-pkgs.org/). The package in this repository is quite minimal, so it's up to you if you'd like to add things like function documentation with {roxygen2}, for example.
* You can also use the {testthat} functions in a regular R script, which you may prefer if your project is not in the format of an R package.
