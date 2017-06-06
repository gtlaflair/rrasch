library(tidyverse)
library(magrittr)
library(here)
library(openxlsx)


ess <- here('data', 'essays_raters.csv') %>%
    read_csv(.)

responses <- ess$examinee_id
raters <- ess$rater_id %>% na.omit(.)
tasks <- ess$task_id
benchmarks <- responses[6:10]

responses <- 1563
raters <- 19
tasks <- 3

rate_plan <- function(responses, tasks, raters, ratings = NULL, benchmarks = NULL, print_tables = FALSE){


    if(!is.null(ratings)){

    ratings <- ratings

    }

    if(is.null(ratings)){

        ratings <- 2
    }

    if(length(responses) == 1 & length(raters) == 1 & length(unique(tasks)) == 1){

        num_resp <- 1:responses

        num_tasks <- 1:tasks %>%
            rep(., responses / tasks)

        dif_tasks <- responses - length(num_tasks)

        num_tasks <- num_tasks %>%
            c(., .[0:dif_tasks])

        num_raters <- 1:raters %>%
            sample(.) %>%
            rep(., responses / raters)

        dif_raters <- responses - length(num_raters)

        num_raters <- num_raters %>%
            c(., .[0:dif_raters])

        rate_table <- data.frame(num_resp, num_tasks, num_raters)

        raters_n <- raters

    }

    if(length(responses) > 1 & length(responses) > 1 & length(unique(tasks)) > 1){

        if(is.null(benchmarks)){

            rate_table <- data.frame(num_resp = responses, num_tasks = tasks)

            num_resp <- rate_table$num_resp

            num_raters <- raters %>%
                sample(.) %>%
                rep(., length(responses) / length(raters))

            dif_raters <- length(responses) - length(num_raters)

            num_raters <- num_raters %>%
                c(., .[0:dif_raters])

            rate_table <- data.frame(rate_table, num_raters)

            raters_n <- length(unique(num_raters))

        }


        if(!is.null(benchmarks)) {

            rate_table <- data.frame(num_resp = responses, num_tasks = tasks)

            bench <- rate_table[which(rate_table$num_resp %in% benchmarks),]

            rate_table <- rate_table[which(!rate_table$num_resp %in% benchmarks),]

            responses <- responses[which(!responses %in% benchmarks)]

            num_resp <- rate_table$num_resp

            num_raters <- raters %>%
                sample(.) %>%
                rep(., length(responses) / length(raters))

            dif_raters <- length(responses) - length(num_raters)

            num_raters <- num_raters %>%
                c(., .[0:dif_raters])

            rate_table <- data.frame(rate_table, num_raters)

            raters_n <- length(unique(num_raters))

            bens <- data.frame(num_resp = rep(bench$num_resp, length(raters)), num_task = rep(bench$num_task, length(raters)))

            bens <- arrange(bens, num_resp) %>%
                cbind(., raters) %>%
                rename(., response_ids = num_resp, task_ids = num_task, rater_ids = raters) %>%
                mutate(rand_score = sample(0:9, n(), replace = TRUE))

    }}


    k <- 1

    n_raters <- rep(raters_n, length(num_raters))
    k_constant <- rep(k, length(num_raters))

    rate_table <- rate_table %>%
        mutate(km = rep(0:(k-1), length(num_raters)/k))

    jn <- sort(rep(0:(raters_n-1), raters_n)) %>%
        .[1:length(num_raters)]

    rate_table$jn <- jn

    rate_table$c_0 <- num_resp

    j <- rep(raters_n, length(num_raters))
    s <- rep(k, length(num_raters))

    h <- 0:(ratings-1)
    q <- 1:k
    n_col <- 1:ncol(rate_table)
    l_col <- max(n_col)

    rate_names <- NULL
    for (i in 1:length(h)) {
        rate_names[[paste("c",i,sep="_")]] <- ((rate_table[,h[1]+6])+(s*q)+((s*2)*h[i]))%%(s*j)
        rate_names[i]
        rate_table[l_col+i] <- rate_names[i]
    }

    y <- 0:ratings
    t <- rate_table[1]
    z <- rate_table[2]
    e <- rate_table[3]

    rate_rep <- NULL
    rate_tabs <- NULL
    for (i in 1:length(y)){
        rate_rep[[paste("response_ids", i-1, sep=".")]] <- rate_table[order(rate_table[,y[i]+6]),c(2:3, i+3)]
        rate_rep[[i]][3] <- t
        rate_rep[[i]] <- rate_rep[[i]][order(rate_rep[[i]][1]),]
        rate_tabs[paste("response_ids", i-1, sep = ".")] <- as.data.frame.list(rate_rep[[i]][3])
        rate_tabs <- as.data.frame(rate_tabs)
        rate_tabs <- data.frame('tasks' = z, "raters" = e, rate_tabs)
    }


    link_test <- rate_tabs %>%
        select(., -contains('num_tasks.'), -contains('num_raters.'), -contains('response_ids.0')) %>%
        gather(., key = id, value = response_ids, -num_tasks, -num_raters) %>%
        mutate(rand_score = sample(0:9, n(), replace = TRUE)) %>%
        select(., 4, 1, 2, 5) %>%
        rename(., rater_ids = num_raters, task_ids = num_tasks) %>%
        as_tibble(.)

    if(!is.null(benchmarks)){

        link_test <- bind_rows(link_test, bens)
    }

    if(length(unique(num_tasks)) < 2){

        link_test <- link_test %>%
            select(., -task_ids)

        rater_view <- link_test %>%
            group_by(rater_ids) %>%
            summarise(response_ids = list(response_ids),
                      rater_counts = n())
    }

    if(length(unique(num_tasks)) > 2){

        link_test <- link_test

        rater_view <- link_test %>%
            group_by(rater_ids) %>%
            summarise(response_ids = list(response_ids),
                      task_ids = list(task_ids),
                      rater_counts = n())
    }

    tables <- list('rater_view' = rater_view, 'link_test' = link_test)

    if(print_tables == TRUE){

    write_csv(link_test, "link_test.csv")
    write_rds(rater_view, 'rater_view.rds')
    write.xlsx(link_test, 'facets_data.xlsx', rowNames = FALSE, colNames = FALSE)

    }

    return(tables)

}
