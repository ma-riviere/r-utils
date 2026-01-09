label_pval <- function(p, accuracy = 0.001, add_p = FALSE) {
    stars <- cut(
        p,
        breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        labels = c("***", "**", "*", ".", ""),
        right = TRUE,
        include.lowest = TRUE
    )
    pval_label <- scales::label_pvalue(accuracy = accuracy, add_p = add_p)(p)
    paste(pval_label, stars)
}
