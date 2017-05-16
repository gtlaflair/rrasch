library(sampling)
library(here)
library(tidyverse)

data <- here('data', 'test_data.csv') %>%
    read_csv()

############ Cal.Plan ###################

cal_plan <- function(d, k, r, c){
  # This function requires that you have the package "sampling" installed
  # on your computer. The function will load the package, but you must have
  # already installed.
  #
  # This function takes 4 arguments:
  #   1. d = dataframe. Name it whatever you want. This data frame must include
  #   columns for Arrangement ID, Examiner ID, and Oral Score. The examiner ID
  #   column must be labled "ExaminerID"
  #   2. k =  the number of samples you wish to take from each examiner. Enter a
  #      number that is not greater than the number of samples available.
  #   3. r = the exact number of raters in your dataset.
  #   4. c = the number of calibration sessions you are planning.
  #
  #
  # This function will randomly sample the specified number of performances
  # from each of the raters in your dataset. Then it will redistribute those
  # performance to other raters for the purpose of calibration sessions and
  # Rasch analysis in Facets. It outputs eight files automatically.
  #     1. A visual of the unlinked data
  #     2. A table of how the data was originally collected
  #     3. An unlinked dataset to test for subset connection in Facets
  #     4. An index table for diagnostic checking. To be used if you believe
  #     the reassignment did not work correctly
  #     5. A calibration plan table. This table will show which examiners
  #     get which performance. The first column will indicate the orginal
  #     plan and the next k columns will indicate the assignments for each
  #     calibration session.
  #     6. A linked dataset with random scores to verify the subset connection
  #     in Facets.
  #     7. A visual table of the linking plan.
  #     8. A second visual table of the linking plan
  #
  #     Written by Geoffrey T. LaFlair (2013)
  #
  ###########################################

  require(sampling)
  temp <- strata(d, stratanames = "ExaminerID",
                 size = c(rep(k,r)),
                 method = "srswor")
  temp2 <- getdata(d, temp)
  Sample <- rep(1:k, r)
  temp3 <- data.frame("AID" = temp2[[1]], "EXMNRS" = temp2[[3]],
                      "Score" = temp2[[2]], "Sample" = Sample)
  wide1 <- reshape(temp3, direction = "wide", v.names = c("EXMNRS"),
                   timevar = c("Sample"), idvar = c("AID"), drop = c("Score"))
  wide2 <- reshape(temp3, direction = "wide", v.names = c("AID"),
                   timevar = c("Sample"), idvar = c("EXMNRS"),
                   drop = c("Score"))
  j <- rep(r, r*k)
  s <- rep(k, r*k)

  Cal.table <- temp3
  Cal.table$Km <- rep(0:(k-1), r)
  Cal.table$Jn <- sort(rep(0:(r-1), k))

  shuf0 <- (Cal.table$Jn*k)+Cal.table$Km
  Cal.table$C.0 <- shuf0

  h <- 0:(c-1)
  q <- 1:k
  ncol <- 1:ncol(Cal.table)
  l.col <- max(ncol)

  CalNames <- NULL
  for (i in 1:length(h)) {
    CalNames[[paste("C",i,sep=".")]] <- ((Cal.table[,h[1]+7])+(s*q)+((s^2)*h[i]))%%(s*j)
    CalNames[i]
    Cal.table[l.col+i] <- CalNames[i]
    }

  y <- 0:c
  t <- Cal.table[1]
  e <- Cal.table[2]

  calRep <- NULL
  calTabs <- NULL
  CT <- NULL
  for (i in 1:length(y)){
    calRep[[paste("CA", i-1, sep=".")]] <- Cal.table[order(Cal.table[,y[i]+7]),c(2:3, i+3)]
    calRep[[i]][3] <- t
    calRep[[i]] <- calRep[[i]][order(calRep[[i]][1]),]
    calTabs[paste("CA", i-1, sep = ".")] <- as.data.frame.list(calRep[[i]][3])
    calTabs <- as.data.frame(calTabs)
    calTabs <- data.frame("EXMNRS" = e, calTabs)
  }

  m <- data.frame(calTabs[-c(2:(c+1))])
  mcol <- ncol(m)
  linktest <- reshape(m, direction = "long", ids = row.names(m),
                      varying = c(2:mcol))
  RandScore <- sample(0:9, (k*r*(c+1)), replace = TRUE)
  linktest$RandScore <- RandScore

  LT <- linktest[c(3,1,5)]
  LTsample <- sort(rep(0:c, r*k))
  LT$Sample <- LTsample

  linktest <- linktest[c(3,1,5)]
  linktest <- linktest[order(linktest[2]),]

  wide3 <- reshape(LT, direction = "wide", v.names = c("EXMNRS"),
                  timevar = c("Sample"), idvar = c("CA"), drop = c("RandScore"))
  wide4 <- reshape(LT, direction = "wide", v.names = c("Sample"),
                   timevar = c("EXMNRS"), idvar = c("CA"), drop = c("RandScore"))

  tables <- list("UnlinkVis" = wide1, "OrgRatePlan" = wide2,
                 "UnlinkLong" = temp3, "CheckTable" = Cal.table,
                 "CalPlan" = calTabs[-c(2:(c+1))], "LinkTest" = linktest,
                 "LinkVis1" = wide4, "LinkVis2" = wide3)

  write.csv(tables$UnlinkVis, "unlink_vis.csv", row.names = FALSE)
  write.csv(tables$OrgRatePlan, "org_rate_plan.csv", row.names = FALSE)
  write.csv(tables$UnlinkLong, "unlink_long.csv", row.names = FALSE)
  write.csv(tables$CheckTable, "check_table.csv", row.names = FALSE)
  write.csv(tables$CalPlan, "cal_plan.csv", row.names = FALSE)
  write.csv(tables$LinkTest, "link_test.csv", row.names = FALSE)
  write.csv(tables$LinkVis1, "link_vis_one.csv", row.names = FALSE)
  write.csv(tables$LinkVis2, "link_vis_two.csv", row.names = FALSE)

  return(tables)
}

data$ExaminerID <- factor(data$ExaminerID)
i<-as.data.frame(table(data$ExaminerID))
size <- c(round(i$Freq/284,2)*100)
size[size == 0] <- 1
samp <- strata(data, stratanames = "ExaminerID",
               size = size,
               method = "srswor")
