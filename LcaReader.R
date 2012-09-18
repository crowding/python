#!/bin/env RScript

library(plyr)

main <- function() {
  top <- 10
  if (file.exists("lca_data.RData")) {
    load("lca_data.RData")
  } else {
    main_frame <- read.csv("http://www.foreignlaborcert.doleta.gov/pdf/quarter_2_2012/LCAFY2012_Q2.csv")
    save(main_frame, file="lca_data.RData")
  }

  cat("finished reading\n")
  main_frame_len <- nrow(main_frame)
  cat(paste("Processing ", main_frame_len, " rows\n"))
  main_frame <- normalize(main_frame)
  main_frame <- idata.frame(main_frame)
  cat("finished normalizing data\n")

  pretty_print('Most sought after VISAs')
  visa_counts = ddply(main_frame, .(VISA_CLASS), nrow)
  print(arrange(visa_counts, V1, decreasing=TRUE))

  pretty_print('Top ten job titles')
  job_counts <- ddply(main_frame, .(JOB_TITLE), nrow)
  print(arrange(job_counts, V1, decreasing=TRUE)[1:top,])

  pretty_print('Employers with most number of VISAS in any status')
  employer_status_count <-
    with(main_frame, as.data.frame(table(EMPLOYER_NAME, STATUS)))
  print(order_by(employer_status_count, 'Freq', decreasing=TRUE)[1:top,])

  status_count = table(main_frame$STATUS)
  pretty_print(paste(status_count[["CERTIFIED"]], 'VISAs are certified and',
                     main_frame_len - status_count[["CERTIFIED"]], 'are not'))

  pretty_print("Employers with the highest salary budget")
  employer_sum <-
    with(main_frame, tapply(RATE_FROM, data.frame(EMPLOYER_NAME),
                                   sum, na.rm=TRUE))
  employer_sum <- as.data.frame(as.table(employer_sum), responseName="sum")
  print(order_by(employer_sum, "sum", decreasing=TRUE)[1:top,])

  pretty_print('The city offering the highest dough (summed over all positions)')
  city_sum <- with(main_frame,
    tapply(RATE_FROM, data.frame(EMPLOYER_CITY, EMPLOYER_STATE), sum, na.rm=TRUE))
  city_sum <- as.data.frame(as.table(city_sum), responseName="sum")
  print(order_by(city_sum, "sum", decreasing=TRUE)[1:top,])

  pretty_print('Jobs with the most dough (summed across offers from all employers)')
  job_title_group <- with(main_frame, tapply(RATE_FROM, data.frame(JOB_TITLE), sum, na.rm=TRUE))
  job_title_group <- as.data.frame(as.table(job_title_group), responseName="sum")
  print(order_by(job_title_group, "sum", decreasing=TRUE)[1:top,])
  print('Done')
}

order_by <- function(df, by=colnames(df), decreasing=FALSE) {
  x <- df[do.call(order, c(as.list(df[by]), decreasing=decreasing)),]
  row.names(x) <- NULL
  x
}

pretty_print <- function(name) {
  cat("********************************************************\n")
  cat(name, "\n")
  cat("********************************************************\n")
}

normalize <- function(x) {
  x <- rename(x, c(LCA_CASE_EMPLOYER_NAME   = "EMPLOYER_NAME",
                   LCA_CASE_EMPLOYER_CITY   = "EMPLOYER_CITY",
                   LCA_CASE_EMPLOYER_STATE  = "EMPLOYER_STATE",
                   LCA_CASE_JOB_TITLE       = "JOB_TITLE",
                   CERTIFIED                = "STATUS_CERTIFIED",
                   LCA_CASE_WAGE_RATE_FROM  = "RATE_FROM",
                   LCA_CASE_WAGE_RATE_UNIT  = "WAGE_RATE_UNIT"))

  wage_rate_multiplier = c(Year=1, Month=12, `Bi-Weekly`=52/2, Week=52)
  mutate(x, RATE_FROM =
         RATE_FROM*wage_rate_multiplier[WAGE_RATE_UNIT])
}

main()
