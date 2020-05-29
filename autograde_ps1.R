# Sample R assignment autograder for Gradescope
#
# The sample assignment is:
# Write a function named continents.gdp with the signature
# function(my.gapminder, y) which returns a data frame
# that contains the total GDP on every continent in the year y
# using the data in the data frame my.gapminder, which
# is formatted similarly to gapminder
# The data frame you return should contain the columns
# continent and gdp (in this order). The rows should be
# sorted in alphabetical order by continent
#
# Author: Michael Guerzhoy guerzhoy@princeton.edu


# START FILE SETUP
# This segment sets up the files needed for grading

rm(list = ls())

filename <- "ps1.R" # Assignment file name

assignment_title_env_var = Sys.getenv(c("ASSIGNMENT_TITLE"))
# Check for the existence of an environment variable called "ASSIGNMENT_TITLE"
if(assignment_title_env_var != ""){
  # If the environment variable is not empty, we're in Gradescope,
  # so set the output correctly for Gradescope to find it
  json.filename <- "/autograder/results/results.json"
}else{
  # If the environment variable is empty, we're working locally,
  # so just put the output in this same directory.
  json.filename <- "results.json"
}

# END FILE SETUP

# START ASSIGNMENT SETUP
# This section is for things needed to assess the submitted file(s) that aren't the actual tests.
# This is a good place to load sample data and set up structures needed for testing.

suppressMessages(library(tidyverse))
suppressMessages(library(jsonlite))

library(gapminder)
library(tidyverse)

factors.to.chars <- function(column){
  if("factor" %in% class(column)){
    as.character(column)
  }else{
    column
  }
}

my.toString <- function(obj){
  if("data.frame" %in% class(obj)){
    toString(as_tibble(lapply(obj, as.character)))
  }else{
    toString(obj)
  }
}


my.gap.1 <- tibble(country = c("countr1", "countr2", "countr1", "countr2"),
                 continent = c("cont1", "cont2", "cont1", "cont2"),
                 year = c(1, 1, 2, 2),
                 lifeExp = c(70, 71, 76, 50),
                 pop = c(30, 20, 32, 21),
                 gdpPercap =c(40, 50, 42, 52))


my.gap.2 <- tibble(country = c("countr1", "countr2", "countr1", "countr2"),
                   continent = c("cont2", "cont1", "cont2", "cont1"),
                   year = c(1, 1, 2, 2),
                   lifeExp = c(70, 71, 76, 50),
                   pop = c(30, 20, 32, 21),
                   gdpPercap =c(40, 50, 42, 52))

# END ASSIGNMENT SETUP

# START TEST SETUP
# This section is where a list of tests is created. Each test in the list has several parts:
# name: the name of the evaluation criterion shown to students
# fun: the function to be run, as a text string
# args: the arguments provided to that function, as a list of objects
# expect: the expected output of `fun`, as an R object
# visibility: whether this criterion will be shown to students. Accepted values are "visible" and "hidden"
# weight: the number of points possible for this criterion

test.cases <- list(
                  list(
                    name = "Continents sorted",
                    fun = "continents.gdp",
                    args = list(my.gap.1, 2),
                    expect = data.frame(continent = c("cont1", "cont2"), gdp = c(32*42, 21*52)),
                    visibility = "visible",
                    weight = 10
                  ),
                  list(
                    name = "Continents not sorted",
                    fun = "continents.gdp",
                    args = list(my.gap.2, 2),
                    expect = data.frame(continent = c("cont1", "cont2"), gdp = c(21*52, 32*42)),
                    visibility = "hidden",
                    weight = 10
                    )
          )

# END TEST SETUP

# START RUN TESTS
# The rest of the code runs the tests, then prepares the JSON output appropriately.
# You probably don't want to edit it unless you have to.

ret <- tryCatch(source(filename), error = function(c) c, warning = function(c) c ,message = function(c) c)
if(!("visible" %in% names(ret))){
  tests <- list()
  tests[["tests"]] <- list()

  for(i in 1:length(test.cases)){
    tests[["tests"]][[i]] <- list(test.cases[[i]][["name"]],
                                  score = 0,
                                  max.score = test.cases[[i]][["weight"]],
                                  visibility = test.cases[[i]][["visibility"]],
                                  output = my.toString(ret))
  }



  write(toJSON(tests, auto_unbox = T), file = json.filename)
  stop(my.toString(ret))
}

tests <- list()
tests[["tests"]] <- list()
for(i in 1:length(test.cases)){
  cur.name <- test.cases[[i]][["name"]]
  cur.fun <- test.cases[[i]][["fun"]]
  cur.args <- test.cases[[i]][["args"]]
  cur.expect <- test.cases[[i]][["expect"]]
  cur.vis <- test.cases[[i]][["visibility"]]
  cur.weight <- test.cases[[i]][["weight"]]
  ret.val <- tryCatch(do.call(cur.fun, cur.args), error = function(c) c, warning = function(c) c ,message = function(c) c)

  cur.output <- ""


  if(my.toString(ret.val) == my.toString(cur.expect)){
      cur.score <-  cur.weight
      cur.output <- "Test passed\n"
  }else{
    cur.score <- 0
    cur.output <- paste("Input:", my.toString(cur.args), "\n\nFunction:",  cur.fun, "\n\nExpected:", my.toString(cur.expect), "\n\nGot:", my.toString(ret.val))
  }
  tests[["tests"]][[i]] <- list(name = cur.name,
                                score = cur.score,
                                max.score = cur.weight)

  tests[["tests"]][[i]][["output"]] <- cur.output

  if(cur.vis != "visible"){
    tests[["tests"]][[i]][["visibility"]] <- cur.vis
  }
}

cat.tests <- function(tests){
  for(i in 1:length(tests[["tests"]])){
    cur.name <- test.cases[[i]][["name"]]
    cur.fun <- test.cases[[i]][["fun"]]
    cur.args <- test.cases[[i]][["args"]]
    cur.expected <- test.cases[[i]][["expect"]]
    cur.output <- tests[["tests"]][[i]][["output"]]
    score <- tests[["tests"]][[i]][["score"]]
    max.score <- tests[["tests"]][[i]][["max.score"]]



    cat(sprintf("Test %s: %s(%s)\n", cur.name, cur.fun, my.toString(cur.args)))
    cat(sprintf("Expected: %s\n", my.toString(cur.expected)))
    cat(sprintf("Output:\n %s\n", my.toString(cur.output)))
    cat(sprintf("Score: %g/%g\n", score, max.score))
    cat("====================================================\n\n\n")

  }
}
cat.tests(tests)
write(toJSON(tests, auto_unbox = T), file = json.filename)

# END RUN TESTS
