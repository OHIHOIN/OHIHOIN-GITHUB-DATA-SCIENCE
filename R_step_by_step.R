#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);
library(ggplot2);#(#graphics)
library(dplyr);#data processing
library(GGally);#for additional graphics
library(rio);#Simple command for importing and exporting
library(pander);#creates tables
library(printr);
options(max.print=42);#Puts a limit to the result output
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};

# R basic syntax ----
#'
#' # R basic syntax
#'
#' ## Assignment, Variables, and Data types.
#'
#' To store a value as variable `foo` in R the convention is to use the
#' `<-` operator, not `=`. This makes it easier to distinguish stand-alone
#' expressions from function arguments.

#+ assignment_operator
foo<-500;
bar<-foo;
bar<-foo<-500;
#' It's not a formal rule, it's rarely even written down, but `foo`, `baz`,
#' `bat`, etc. are throw-away variables people use for testing. If you need more
#' test variables, just make up three letter ones that start with `b`.
#' If you see one of those in a script you're reviewing it means it is left over
#' from when that code was being debugged. Example code shared with this class
#' will usually use `foo` and friends to represent the parts of an expression
#' that should be replaced with whatever values you are using instead of being
#' used literally. Shorter than having to write `YOURFUNCTIONHERE` or
#' `YOURARGUMENTHERE` each time.
#'
#' This is not specific to R-- it's just a little quirk of programming culture
#' in general. A quirk with a practical consequence: _never use `foo`, `bar`,
#' `baz`, `bat`, etc. in your production (i.e. finalized) code_ because
#' otherwise you or somebody else debugging your code will attempt to use those
#' names as test variables and in some situations this could overwrite the
#' existing variables!
#'
#'
#' ## Simple data types
#'
#' Numeric. You can do arithmetic on them: `+`, `-`, `/`, `*`, `^`, `log()`,
#' `exp()`, `sqrt()`

#+ assignment_numeric
foo <- 2 + 2; foo;
foo <-log(10); foo;
whatisthis(bar)
foo <- log(2^2); foo;
foo <- log(((3^2)*(2+3)/4)-1); foo;
#' Character strings. Create these by wrapping single (`'`) or double (`"`)
#' quotes around the value.

#+ assignment_string
Greg <- ("Do Not Panic"); Greg;
Greg <- ('Do Not Panic'); Greg;
Greg <- ("Don't worry"); Greg;
Jerry <- ('The "Heart of Gold" comes equipped with heated leather seats and an infinite improbability drive'); Jerry;
Joseph <-42; Joseph;

#' Logical values are `TRUE` or `FALSE`. Can also be created using `>`, `<`,
#' `==`, `!=`, `>=`, `<=`

#+ assignment_logical
job <-50; job;
job == 50;
job < 50; job;
job > 50; job;
job != 50; job;
job >= 50; job;
job <= 50; job;
#' Missing values are represented by `NA` (no quotes for any of these). Null
#' values are _not_ the same as missing and they are represented by `NULL`. In
#' some circumstances you might also run into `Inf`, `-Inf`, and `NaN`. These
#' often indicate errors somewhere else in your code.

#' Dates and times. Can be created with the `Sys.Date()` or `Sys.time()`
#' functions or converted from a character string using `as.Date()`.

#+ assignment_datetime
Schedule <- Sys.Date(); Schedule;
Schedule <- Sys.time(); Schedule;
schedule <- "08/02/2022"
whatisthis(schedule)
schedule <- as.Date(schedule,"%m/%d/%Y")
whatisthis(schedule)
#' Factors are basically integers that have labels. They are a human-readable
#' alternative to using integer codes for discrete data. These will make more
#' sense after we talk about vectors in the next section.

#+ factor_example

#+ assignment_wierd

#' ## Data structures
#'
#' Of course we usually need to work with variables bundled together, not single
#' values.
#'
#' ### Vectors
#'foo <- c(12, 14, 16, 18, 20, 22)
#'print(foo <- c(12, 14, 16, 18, 20, 22))
print(foo);
#' The default data structure in R is a `vector`. You create one with the `c()`
#' command with any number of arguments. All items in a vector have to be the
#' same type.

#+ vectors_c

#' Since the default data structure in R is a `vector`, if you
#' create some sort of simple value you are creating a `vector` even if you are
#' not using `c()`... it just happens to be a `vector` of length 1. These
#' are identical, and both return `1` when used with the `length()` function.

#+ vectors_length1
length(foo);
#' If you want to create a sequence of consecutive integers, you can use the `:`
#' operator.

#+ vectors_sequence
40:70
36:80
-20:30
seq_len(20)

#' In most other languages, you need to use a `for` loop in order to perform
#' some sort of change to a series of values. In R, you often don't have to
#' when you are working with vectors because a lot of functions (including all
#' the arithmetic and logical ones above) and be applied to a vector and they
#' work. If the function involves multiple vectors (e.g. `+`), usually you'll
#' want all of them to be either the same length or length 1.

#+ vectors_operators
foo+7
foo>= 12

#' You can assign names to some or all members of a vector when you create it.
#' You can also assign or re-assign names later using the `names()` function.

#+ vectors_names1
print(cup <- c(b="job", c="school", d="office"))


#' You can also use it to see the currently assigned names.

#+ vectors_names2
names(foo)
names(cup)
names(cup);
names(cup) <- c("spoon", "knife", "fork")
cup
#' You can subset a vector by using `[...]` with the `...` replaced by _another_
#' vector, of integers indicating which positions you want to extract. Or you
#' could use a vector of names.

#+ vectors_subset1
foo[12]
foo[3]
#' If you just need a single value, use a single name or number.

#+ vectors_subset2

#' If you need a series of adjacent values, use `:`. If you need several
#' adjacent series with interruptions between them, use `c()` with `:`
#' expressions separated by commas `,`.

#+ vectors_subset3
foo[c(12,14,16)]
foo[12:16]

#' Other useful functions for exploring vectors: `length()`, `summary()`,
#' `table()`, `head()`, `tail()`, `sum()`, `diff()`, `seq_along()`.

#+ vectors_explore
summary(foo)
table(foo)
bird <- sample(1:10, 30, replace = TRUE)* 1000
table(bird)
table(bird)
bird
head(bird)
tail(bird)
diff(bird)
sum(bird)
seq_along(bird) #used to generate ID numbers for each element
sum(bird, na.rm=TRUE)
#' Here are some aggregation functions. For these, make sure to use `na.rm=T` if
#' your vector has `NA`s in it... `max()`, `min()`, `mean()`, `median()`,
#' `quantile()`.

#+ vectors_aggregate
quantile(bird)
quantile(bird, na.rm=TRUE)

#' ### Data Frames
#'
#' You can bundle several vectors of the same length together into a
#' `data.frame` using the `data.frame()` command. A `data.frame` is a tabular
#' data structure where the columns can be different types from each other
#' (though within each column the type will be uniform because each column is
#' still a vector). Most data in R is in the form of a `data.frame` or a class
#' that inherits from `data.frame`. The `dplyr` package makes working with
#' `data.frames` easier and a lot of attention will be devoted to `dplyr`
#' [below](#data-frames-indepth). For now, here are some basic commands for
#' exploring `data.frames`: `dim()`, `nrow()`, `ncol()`, `names()` and (for
#' small datasets) `plot()`.

#+ df_explore
dim (iris)
nrow (iris)
ncol(iris)
names(iris)
head(iris)
tail(iris)
#' how to select rows
#+ df_rows
 iris [3:20,]
iris[c(3:20, 4:36,5:40),]
nrow(iris)
seq_len(nrow(iris))
sample(seq_len(nrow(iris)),20) #without replacement
sample(seq_len(nrow(iris)),20,replace = TRUE) #with replacement
iris[sample(seq_len(nrow(iris)),20),]

#' 

#' how to select columns
#+ df_column 
 iris[,1:5]
 iris[,c(2:5, 3:4,2:5)]
 names(iris)
 iris[,c("Sepal.Length","Sepal.Width")]
 foo <- c("Sepal.Length","Sepal.Width")
 iris[,foo]
 iris$Sepal.Length
 iris[["Sepal.Length"]]
 
 foo <- "Sepal.Length"
 iris[[foo]]
#' 
#' how to select columns and rows at the same time


#+ df_subset
 iris[c(3:20, 4:36,5:40),c(2:5, 3:4,2:5)]

#' ## comments
#'
#' `#` This is an ordinary comment. Everything after it on the same line is not
#' executed.
#'
#' `#'` This indicates that this line should be formatted as text. It must be
#' the first two characters in that line in order to work.
#'
#' `#+` This indicates that the following lines (until the next #' or #+) should
#' be treated as a "code chunk". I.e. the next lines (but not this one) will be
#' run, the coede will be displayed according to your settings and the results
#' will be displayed according to your settings.
#'
#' For #+ you can add "chunk options" separated by commas. The first one has no
#' name and is always the label of the chunk. E.g. df_subset. The rest need
#' names, e.g. error=TRUE.
#' ## LINWEAR MODEL
#+ example(lm)
example(lm)
summary(lm.D9)
summary(lm.D9)$coeff
library(broom)
tidy(lm.D9)
 glance(lm.D9)
#+ debugg
performance <- lm(mpg~hp+wt+vs+gear+carb+disp, mtcars)
summary(performance)
tidy(performance)
#+ debugg2
tidy(performance)[-1,c("estimate","p.value")] 
#+ debugg2a
lm(mpg~hp+wt+vs+gear+carb+disp, mtcars) %>% tidy() %>% select(c("estimate","p.value"))
#+ debugg3
lm(mpg~hp+wt+vs+gear+carb+disp, mtcars) %>% tidy() %>% select(c("estimate","p.value"))
#+ MULTIPLE COMPARISON
performance %>%tidy() %>% select(c("estimate", "p.value")) %>% slice(-1)

performance %>%tidy() %>% select(c("estimate", "p.value")) %>% slice(-1) %>% (unlist)%>% p.adjust()
#' ##  dplyr learning
#' 


#' ##. Working with Datasets and d[lyr
#' 
#' Define. location of your files
#' 
#+ IMPORT FILES
r"(C:\Users\Dr Greg\Desktop\DATA  FOR PRACTICE)" %>% gsub("\\\\","/",.)
list.files(r"(C:\Users\Dr Greg\Desktop\DATA  FOR PRACTICE)") 
#' ## To List and Import Files
#' 
#' 

Example1<- list.files(r"(C:\Users\Dr Greg\Desktop\DATA  FOR PRACTICE)", full.names = T) %>% gsub("\\\\","/",.) %>% sapply(import) %>% setNames(.,basename(names(.)))

Example2 <- Example1$Birthweight.sav

