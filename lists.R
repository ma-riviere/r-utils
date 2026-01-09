## Get element by name from list:
rmatch <- function(x, name) {
    pos <- match(name, names(x))
    if (!is.na(pos)) {
        return(x[[pos]])
    }
    for (el in x) {
        if (class(el) == "list") {
            out <- Recall(el, name)
            if (!is.null(out)) return(out)
        }
    }
}

# ------------------------------------------------------------------------------
# rectangularize: Custom recursion with column prefixing
# ------------------------------------------------------------------------------

flatten_node <- function(node, prefix = "") {
    # Leaf: atomic value -> single-column data table
    if (rlang::is_atomic(node)) {
        dt <- data.table::as.data.table(list(V = node))
        data.table::setnames(dt, if (nzchar(prefix)) prefix else "value")
        return(dt)
    }

    # Recurse on children, prefixing their names
    children <- purrr::imap(node, \(child, name) {
        child_prefix <- if (nzchar(prefix)) paste0(prefix, "__", name) else name
        flatten_node(child, child_prefix)
    })

    # All children are now data tables - combine them
    if (length(children) == 0) {
        return(data.table::data.table())
    }

    # Row-bind if unnamed (array of objects), else col-bind
    if (is.null(names(node))) {
        out <- data.table::rbindlist(children, use.names = TRUE, fill = TRUE)
    } else {
        out <- purrr::reduce(children, pad_cbind)
    }

    return(out)
}

rectangularize <- function(x) {
    # Preprocess: replace all NULLs with NA
    x <- purrr::modify_tree(x, leaf = \(el) if (is.null(el)) NA else el)

    flatten_node(x)
}

# ------------------------------------------------------------------------------
# rectangularize2: Simple modify_tree approach (no column prefixing)
# ------------------------------------------------------------------------------

pad_to_size <- function(dt, target) {
    n <- nrow(dt)
    if (n >= target) {
        return(dt)
    }
    pad <- dt[rep(NA_integer_, target - n)]
    data.table::rbindlist(list(dt, pad), use.names = TRUE, fill = TRUE)
}

pad_cbind <- function(dt1, dt2) {
    if (!is.data.table(dt1)) {
        dt1 <- data.table::as.data.table(as.list(dt1))
    }
    if (!is.data.table(dt2)) {
        dt2 <- data.table::as.data.table(as.list(dt2))
    }
    # Handle empty data tables
    if (nrow(dt1) == 0 && ncol(dt1) == 0) {
        return(dt2)
    }
    if (nrow(dt2) == 0 && ncol(dt2) == 0) {
        return(dt1)
    }
    target <- max(nrow(dt1), nrow(dt2))
    cbind(pad_to_size(dt1, target), pad_to_size(dt2, target))
}

# Recursively convert data.frames to lists of rows, NULLs to NA
normalize_tree <- function(x) {
    if (is.data.frame(x)) {
        rows <- purrr::transpose(as.list(x))
        return(lapply(rows, normalize_tree))
    }
    if (is.list(x)) {
        return(lapply(x, normalize_tree))
    }
    if (is.null(x)) {
        return(NA)
    }
    return(x)
}

rectangularize2 <- function(x) {
    # Preprocess: convert data.frames to lists of rows, NULLs to NA
    x <- normalize_tree(x)

    # Now all data.frames are lists, so is_node = is.list works
    purrr::modify_tree(
        x,
        is_node = is.list,
        post = \(node) {
            if (purrr::every(node, rlang::is_atomic)) {
                return(data.table::as.data.table(node))
            }
            if (purrr::none(node, rlang::is_atomic)) {
                return(data.table::rbindlist(node, use.names = TRUE, fill = TRUE))
            }
            if (purrr::some(node, rlang::is_atomic)) {
                atomics <- purrr::keep(node, rlang::is_atomic) |> as.data.table()
                non_atomics_list <- purrr::discard(node, rlang::is_atomic)
                non_atomics <- purrr::reduce(non_atomics_list, pad_cbind)
                return(pad_cbind(atomics, non_atomics))
            }
        }
    )
}
