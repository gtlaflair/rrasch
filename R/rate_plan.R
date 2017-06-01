library(tidyverse)

id <- 2500
ratings <- 2
raters <- 70



rate_plan <- function(id, ratings, raters){

    # This function takes 4 arguments:
    #   1. d = dataframe. Name it whatever you want. This data frame must include
    #   columns for Arrangement ID, Examiner ID, and Oral Score. The examiner ID
    #   column must be labled "ExaminerID"
    #   2. k =  the number of samples you wish to take from each examiner. Enter a
    #      number that is not greater than the number of samples available.
    #   3. r = the exact number of raters in your dataset.
    #   4. c = the number of calibration sessions you are planning.

    num_resp <- 1:id
    # num_raters <- sample(x = raters, size = id, replace = TRUE, prob = rep(raters/id, raters))
    num_ratings <- 1:ratings %>%
        rep(., id/ratings)
    num_raters <- 1:raters %>%
        rep(., id/raters)

    dif <- id - length(num_raters)

    num_raters <- c(num_raters, num_raters[0:dif])

    d <- data.frame(num_resp, num_raters, num_ratings)


    # k <- d %>% group_by(num_raters) %>% count(.)
    k <- 1
    c <- ratings
    r <- raters

    # require(sampling)
    # temp <- strata(d, stratanames = "num_raters",
    #                size = c(rep(k,r)),
    #                method = "srswor")
    # temp2 <- getdata(d, temp)
    # Sample <- rep(1:k, r)
    # temp3 <- data.frame("AID" = temp2[[1]], "EXMNRS" = temp2[[3]],
    #                     "Score" = temp2[[2]], "Sample" = Sample)
    # wide1 <- reshape(temp3, direction = "wide", v.names = c("EXMNRS"),
    #                  timevar = c("Sample"), idvar = c("AID"), drop = c("Score"))
    # wide2 <- reshape(temp3, direction = "wide", v.names = c("AID"),
    #                  timevar = c("Sample"), idvar = c("EXMNRS"),
    #                  drop = c("Score"))
    j <- rep(r, length(num_raters))
    # j <- j[1:length(num_raters)]
    s <- rep(k, length(num_raters))
    # s <- s[1:length(num_raters)]

    rate_table <- d
    km <- rep(0:(k-1), length(num_raters)/k)
    # km <- km[1:length(num_raters)]
    # km <- c(km, km[1:dif])
    rate_table$km <- km

    jn <- sort(rep(0:(r-1), r))
    jn <- jn[1:length(num_raters)]
    # jn <- c(jn, jn[1:50])

    rate_table$jn <- jn

    # shuf0 <- (rate_table$jn*k)+rate_table$km
    rate_table$c_0 <- num_resp

    h <- 0:(c-1)
    q <- 1:k
    n_col <- 1:ncol(rate_table)
    l_col <- max(n_col)

    s_q <- s*q
    # s_q <- c(s_q, s_q[1:dif])

    s_2 <- s^2
    # s_2 <- c(s_2, s_2[1:dif])

    s_j <- s*j
    # s_j <- c(s_j, s_j[1:dif])

    rate_names <- NULL
    for (i in 1:length(h)) {
        rate_names[[paste("c",i,sep="_")]] <- ((rate_table[,h[1]+6])+(s*q)+((s*2)*h[i]))%%(s*j)
        rate_names[i]
        rate_table[l_col+i] <- rate_names[i]
    }

    y <- 0:c
    t <- rate_table[1]
    e <- rate_table[2]

    calRep <- NULL
    calTabs <- NULL
    CT <- NULL
    for (i in 1:length(y)){
        calRep[[paste("CA", i-1, sep=".")]] <- rate_table[order(rate_table[,y[i]+6]),c(2:3, i+3)]
        calRep[[i]][3] <- t
        calRep[[i]] <- calRep[[i]][order(calRep[[i]][1]),]
        calTabs[paste("CA", i-1, sep = ".")] <- as.data.frame.list(calRep[[i]][3])
        calTabs <- as.data.frame(calTabs)
        calTabs <- data.frame("EXMNRS" = e, calTabs)
    }

    m <- data.frame(calTabs[-c(2:(c+1))])
    m <- m[-4]
    mcol <- ncol(m)
    linktest <- reshape(m, direction = "long", ids = row.names(m),
                        varying = c(2:mcol))
    RandScore <- sample(0:9, (k*r*(c+1)), replace = TRUE)
    linktest$RandScore <- sample(0:9, length(linktest[1]))

    LT <- linktest[c(3,1,5)]
    LTsample <- sort(rep(0:c, r*k))
    LT$Sample <- LTsample

    linktest <- linktest[c(3,1,5)]
    linktest <- linktest[order(linktest[2]),]

    wide3 <- reshape(LT, direction = "wide", v.names = c("num_raters"),
                     timevar = c("Sample"), idvar = c("CA"), drop = c("RandScore"))
    wide4 <- reshape(LT, direction = "wide", v.names = c("Sample"),
                     timevar = c("num_raters"), idvar = c("CA"), drop = c("RandScore"))

    tables <- list("UnlinkVis" = wide1, "OrgRatePlan" = wide2,
                   "UnlinkLong" = temp3, "CheckTable" = Cal.table,
                   "CalPlan" = calTabs[-c(2:(c+1))], "LinkTest" = linktest,
                   "LinkVis1" = wide4, "LinkVis2" = wide3)

    write.csv(tables$UnlinkVis, "unlink_vis.csv", row.names = FALSE)
    write.csv(tables$OrgRatePlan, "org_rate_plan.csv", row.names = FALSE)
    write.csv(tables$UnlinkLong, "unlink_long.csv", row.names = FALSE)
    write.csv(tables$CheckTable, "check_table.csv", row.names = FALSE)
    write.csv(tables$CalPlan, "cal_plan.csv", row.names = FALSE)
    write.csv(tables$LinkTest, "link_test.csv", row.names = TRUE)
    write.csv(tables$LinkVis1, "link_vis_one.csv", row.names = FALSE)
    write.csv(tables$LinkVis2, "link_vis_two.csv", row.names = FALSE)

    return(tables)




    # multi_rate <- function(df, n) {
    #     varname <- paste("rating", n, sep = "_")
    #     df <- mutate_(df, .dots=setNames(paste0("num_resp%%", ratings), varname))
    #     df
    # }
    #
    # for (i in 1:ratings) {
    #     rate_data <- multi_rate(df=rate_data, n=i)
    # }
    #
    # mod <- function(x){
    #
    #     x %% length(ratings)
    # }


}


rate_data$rating_1 %% (4 * 20) + 1

(num_ratings * ratings) %% (num_ratings * 20)
