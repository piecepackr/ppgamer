process_ranks <- function(ranks) {
    if (is.character(ranks)) {
        ranks <- gsub("[[:space:]]", "", ranks)
        ranks <- gsub("[[:punct:]]", "", ranks)
        ranks <- gsub("n", "0", ranks)
        ranks <- gsub("a", "1", ranks)
        if (length(ranks) == 1) ranks <- stringr::str_split(ranks, "")[[1]]
    }
    as.integer(ranks) + 1
}

random_dice <- function(n_dice = 4, n_ranks = 6) {
    sample.int(n_ranks, n_dice, replace = TRUE)
}
