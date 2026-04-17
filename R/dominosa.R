parse_dominosa_pips <- function(pips) {
	if (is.matrix(pips)) {
		storage.mode(pips) <- "integer"
		pips
	} else if (is.character(pips)) {
		rows <- strsplit(pips, "/")[[1]]
		rows <- lapply(rows, function(r) as.integer(process_ranks(r)) - 1L)
		lens <- lengths(rows)
		if (length(unique(lens)) != 1L) {
			abort("All rows in `pips` must have the same length.")
		}
		do.call(rbind, rows)
	} else {
		abort(str_glue("`pips` must be a matrix or character string, not {class(pips)[1L]}."))
	}
}

validate_dominosa_pips <- function(pips) {
	n <- max(pips)
	n_cells <- length(pips)
	expected_cells <- (n + 1L) * (n + 2L)
	if (n_cells != expected_cells) {
		abort(str_glue(
			"For a double-{n} domino set the grid must have {expected_cells} cells, but `pips` has {n_cells} cells."
		))
	}
	expected_count <- n + 2L
	bad <- integer(0)
	for (k in 0:n) {
		if (sum(pips == k) != expected_count) bad <- c(bad, k)
	}
	if (length(bad) > 0L) {
		abort(str_glue(
			"For a double-{n} domino set each pip value must appear exactly {expected_count} time(s). The following pip values have incorrect counts: {paste(bad, collapse = ', ')}."
		))
	}
}

# Compute 1-based domino type ID for sorted pip pair (p_lo <= p_hi).
# Ordering: (0,0)=1, (0,1)=2, ..., (0,n)=n+1, (1,1)=n+2, ...
domino_id <- function(p_lo, p_hi, n) {
	as.integer(p_lo * (n + 1L) - (p_lo * (p_lo - 1L)) %/% 2L + (p_hi - p_lo) + 1L)
}

build_dominosa_placements <- function(pips, n_rows, n_cols, n) {
	# Horizontal pairs: (r,c)-(r,c+1)
	h_r <- rep(seq_len(n_rows), each = n_cols - 1L)
	h_c <- rep(seq_len(n_cols - 1L), times = n_rows)
	h_p1 <- pips[cbind(h_r, h_c)]
	h_p2 <- pips[cbind(h_r, h_c + 1L)]

	# Vertical pairs: (r,c)-(r+1,c)
	v_r <- rep(seq_len(n_rows - 1L), each = n_cols)
	v_c <- rep(seq_len(n_cols), times = n_rows - 1L)
	v_p1 <- pips[cbind(v_r, v_c)]
	v_p2 <- pips[cbind(v_r + 1L, v_c)]

	all_p1 <- c(h_p1, v_p1)
	all_p2 <- c(h_p2, v_p2)
	p_lo <- pmin(all_p1, all_p2)
	p_hi <- pmax(all_p1, all_p2)

	list(
		cell1 = c(
			(h_r - 1L) * n_cols + h_c,
			(v_r - 1L) * n_cols + v_c
		),
		cell2 = c(
			(h_r - 1L) * n_cols + h_c + 1L,
			v_r * n_cols + v_c
		),
		did = domino_id(p_lo, p_hi, n)
	)
}

# Commits a single placement `pid` as solution for its domino
# Then eliminates everything that conflicts with this placement
assign_placement <- function(pid, eliminated, cell_pids, domino_pids, pl_cell1, pl_cell2, pl_did) {
	to_elim <- c(
		cell_pids[[pl_cell1[pid]]], # all other placements that touch cell 1
		cell_pids[[pl_cell2[pid]]], # all other placements that touch cell 2
		domino_pids[[pl_did[pid]]] # all other placements for this domino type
	)
	eliminated[to_elim[to_elim != pid]] <- TRUE
	eliminated
}

# Constraint propagation
# Repeatedly scans for forced assignments until no more can be deduced
propagate_dominosa <- function(
	eliminated,
	cell_pids,
	domino_pids,
	pl_cell1,
	pl_cell2,
	pl_did,
	n_cells,
	n_dom
) {
	repeat {
		prev <- eliminated

		for (d in seq_len(n_dom)) {
			pids <- domino_pids[[d]]
			active <- pids[!eliminated[pids]]
			if (length(active) == 0L) {
				# domino can't be placed => unsolvable
				return(NULL)
			}
			if (length(active) == 1L) {
				# Forced assignment
				eliminated <- assign_placement(
					active,
					eliminated,
					cell_pids,
					domino_pids,
					pl_cell1,
					pl_cell2,
					pl_did
				)
			}
		}

		for (ci in seq_len(n_cells)) {
			pids <- cell_pids[[ci]]
			active <- pids[!eliminated[pids]]
			if (length(active) == 0L) {
				# cell has no placements => unsolvable
				return(NULL)
			}
			if (length(active) == 1L) {
				# Forced assignment
				eliminated <- assign_placement(
					active,
					eliminated,
					cell_pids,
					domino_pids,
					pl_cell1,
					pl_cell2,
					pl_did
				)
			}
		}

		if (identical(eliminated, prev)) break
	}
	eliminated
}

search_dominosa <- function(
	eliminated,
	cell_pids,
	domino_pids,
	pl_cell1,
	pl_cell2,
	pl_did,
	n_cells,
	n_dom
) {
	# Check for all forced placements first
	eliminated <- propagate_dominosa(
		eliminated,
		cell_pids,
		domino_pids,
		pl_cell1,
		pl_cell2,
		pl_did,
		n_cells,
		n_dom
	)
	if (is.null(eliminated)) {
		return(NULL)
	}

	active_counts <- vapply(domino_pids, function(pids) sum(!eliminated[pids]), integer(1L))
	if (all(active_counts == 1L)) {
		# If all dominoes have exactly one placement left we have our solution
		return(eliminated)
	}

	# Minimum Remaining Values (MRV)
	# Branch on domino with fewest remaining candidates (>= 2)
	unresolved <- which(active_counts >= 2L)
	d <- unresolved[which.min(active_counts[unresolved])]
	pids <- domino_pids[[d]]
	candidates <- pids[!eliminated[pids]]

	for (pid in candidates) {
		result <- search_dominosa(
			assign_placement(pid, eliminated, cell_pids, domino_pids, pl_cell1, pl_cell2, pl_did),
			cell_pids,
			domino_pids,
			pl_cell1,
			pl_cell2,
			pl_did,
			n_cells,
			n_dom
		)
		if (!is.null(result)) return(result)
	}
	NULL
}

#' Solve Dominosa puzzle
#'
#' Solves a Dominosa (Domino Solitaire) puzzle given a grid of pip values.
#'
#' @param pips A matrix of non-negative integer pip values,
#'   or a string with rows of integers separated by `"/"`.
#'   For a double-`n` domino set the grid must have `(n + 1)(n + 2)` cells
#'   and each pip value `0`:`n` must appear exactly `n + 2` times.
#' @return A list with components:
#'   \describe{
#'     \item{`domino_ids`}{An integer matrix of the same dimensions as `pips`
#'       where cells belonging to the same domino share the same integer value.}
#'     \item{`pips`}{The input `pips` matrix.}
#'     \item{`ppn`}{A string of Portable Piecepack Notation for the solution.}
#'   }
#' @seealso <https://www.solitairelaboratory.com/puzzlelaboratory/DominoGG.html> and
#'     <https://puzzlebreaks.com/dominosa/> for more information about Dominosa.
#' @examples
#' pips <- matrix(c(0, 1, 0, 1, 3,
#'                  3, 1, 0, 2, 2,
#'                  3, 2, 1, 0, 3,
#'                  2, 3, 1, 0, 2), nrow = 4L, byrow = TRUE)
#' s <- solve_dominosa(pips)
#' if (rlang::is_installed(c("piecepackr", "ppn"))) {
#'   g <- ppn::read_ppn(textConnection(s$ppn))[[1]]
#'   envir <- piecepackr::game_systems(round = TRUE)
#'   ppn::plot_move(g, open_device = FALSE, annotate = FALSE, envir = envir, scale = 0.95)
#' }
#' @export
solve_dominosa <- function(pips) {
	pips <- parse_dominosa_pips(pips)
	validate_dominosa_pips(pips)

	n <- max(pips)
	n_rows <- nrow(pips)
	n_cols <- ncol(pips)
	n_cells <- n_rows * n_cols
	n_dom <- ((n + 1L) * (n + 2L)) %/% 2L

	pl <- build_dominosa_placements(pips, n_rows, n_cols, n)
	pl_cell1 <- pl$cell1
	pl_cell2 <- pl$cell2
	pl_did <- pl$did
	n_pl <- length(pl_did)

	cell_pids <- lapply(seq_len(n_cells), function(ci) which(pl_cell1 == ci | pl_cell2 == ci))
	domino_pids <- lapply(seq_len(n_dom), function(d) which(pl_did == d))

	result <- search_dominosa(
		rep(FALSE, n_pl),
		cell_pids,
		domino_pids,
		pl_cell1,
		pl_cell2,
		pl_did,
		n_cells,
		n_dom
	)

	if (is.null(result)) {
		abort("No solution exists for this Dominosa puzzle.")
	}

	dom_ids <- matrix(0L, nrow = n_rows, ncol = n_cols)
	for (d in seq_len(n_dom)) {
		pids <- domino_pids[[d]]
		pid <- pids[!result[pids]]
		for (cell in c(pl_cell1[pid], pl_cell2[pid])) {
			r <- (cell - 1L) %/% n_cols + 1L
			cc <- (cell - 1L) %% n_cols + 1L
			dom_ids[r, cc] <- as.integer(d)
		}
	}

	sol <- list(domino_ids = dom_ids, pips = pips)
	sol$ppn <- dominosa2ppn(sol)
	sol
}

dominosa2ppn <- function(sol) {
	pips <- sol$pips
	dom_ids <- sol$domino_ids
	y_max <- nrow(pips)
	n_dom <- max(dom_ids)

	pieces <- character(n_dom)
	for (i in seq_len(n_dom)) {
		cells <- which(dom_ids == i, arr.ind = TRUE)
		r1 <- cells[1L, 1L]
		c1 <- cells[1L, 2L]
		r2 <- cells[2L, 1L]
		c2 <- cells[2L, 2L]
		p1 <- pips[r1, c1]
		p2 <- pips[r2, c2]
		if (r1 == r2) {
			# Horizontal with `<` (90° CCW): 1st number appears on LEFT, 2nd on RIGHT.
			x <- c1 + 0.5
			y <- y_max + 1L - r1
			rotation <- "<"
		} else {
			# Vertical (0°): 1st number appears on TOP, 2nd on BOTTOM.
			x <- c1
			y <- y_max + 0.5 - r1
			rotation <- ""
		}
		pieces[i] <- str_glue("`{p1}-{p2}'{rotation}@({x},{y})")
	}

	paste0(
		"---\nGameType: Dominosa\nSetUp: None\n...\n",
		str_glue("Solution. {paste(pieces, collapse = ' ')}\n")
	)
}
