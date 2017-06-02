library(tidyverse)
library(magrittr)

rate_plan <- function(responses, raters){

    # responses <- 1998
    # raters <- 65

    ratings <- 2

    num_resp <- 1:responses

    # num_ratings <- 1:ratings %>%
        # rep(., ceiling(responses/ratings))

    num_raters <- 1:raters %>%
        rep(., responses/raters)

    dif <- responses - length(num_raters)

    num_raters <- num_raters %>%
        c(., .[0:dif])

    rate_table <- data.frame(num_resp, num_raters)

    k <- 1

    n_raters <- rep(raters, length(num_raters))
    k_constant <- rep(k, length(num_raters))

    rate_table <- rate_table %>%
        mutate(km = rep(0:(k-1), length(num_raters)/k))

    jn <- sort(rep(0:(raters-1), raters)) %>%
        .[1:length(num_raters)]

    rate_table$jn <- jn

    rate_table$c_0 <- num_resp

    j <- rep(raters, length(num_raters))
    s <- rep(k, length(num_raters))

    h <- 0:(ratings-1)
    q <- 1:k
    n_col <- 1:ncol(rate_table)
    l_col <- max(n_col)

    rate_names <- NULL
    for (i in 1:length(h)) {
        rate_names[[paste("c",i,sep="_")]] <- ((rate_table[,h[1]+5])+(s*q)+((s*2)*h[i]))%%(s*j)
        rate_names[i]
        rate_table[l_col+i] <- rate_names[i]
    }

    y <- 0:ratings
    t <- rate_table[1]
    e <- rate_table[2]

    rate_rep <- NULL
    rate_tabs <- NULL
    for (i in 1:length(y)){
        rate_rep[[paste("responses", i-1, sep=".")]] <- rate_table[order(rate_table[,y[i]+5]),c(2:3, i+3)]
        rate_rep[[i]][3] <- t
        rate_rep[[i]] <- rate_rep[[i]][order(rate_rep[[i]][1]),]
        rate_tabs[paste("responses", i-1, sep = ".")] <- as.data.frame.list(rate_rep[[i]][3])
        rate_tabs <- as.data.frame(rate_tabs)
        rate_tabs <- data.frame("raters" = e, rate_tabs)
    }

    m <- data.frame(rate_tabs[-c(2:(ratings+1))])
    m <- m[-4]
    mcol <- ncol(m)
    link_test <- reshape(m, direction = "long", ids = row.names(m),
                        varying = c(2:mcol))

    link_test <- link_test %>%
        mutate(rand_score = sample(0:9, n(), replace = TRUE)) %>%
        select(., 3, 1, 5)

    rater_view <- link_test %>%
        group_by(num_raters) %>%
        mutate(Tasks = map_int(., str_int))
        summarise(Counts = n())


    write.csv(link_test, "link_test.csv", row.names = FALSE)

    return(link_test)

}
