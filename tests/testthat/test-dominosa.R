test_that("solve_dominosa() works as expected", {
	# double-0: trivial single domino
	pips0 <- matrix(c(0L, 0L), nrow = 1L)
	s0 <- solve_dominosa(pips0)
	expect_equal(s0$domino_ids, matrix(c(1L, 1L), nrow = 1L))

	# double-1: 2x3 grid
	# fmt: skip
	pips1 <- matrix(c(
		0L, 0L, 1L,
		1L, 0L, 1L
	), nrow = 2L, byrow = TRUE)
	s1 <- solve_dominosa(pips1)
	expect_equal(
		s1$domino_ids,
		# fmt: skip
		matrix(c(
			1L, 1L, 3L,
			2L, 2L, 3L
		), nrow = 2L, byrow = TRUE)
	)

	# string input equivalent to pips1
	expect_equal(solve_dominosa("001/101")$domino_ids, s1$domino_ids)

	# ppn output
	expect_snapshot(cat(s1$ppn))

	skip_if_not_installed("ppn")
	skip_if_not_installed("ppcli")
	skip_on_os("windows")
	g1 <- ppn::read_ppn(textConnection(s1$ppn))[[1]]
	expect_snapshot(ppn::cat_move(g1, move = "Solution.", color = FALSE))

	# double-2: 3x4 grid
	# fmt: skip
	pips2 <- matrix(c(
				0L, 0L, 2L, 2L,
				0L, 1L, 2L, 0L,
				1L, 1L, 1L, 2L
			), nrow = 3L, byrow = TRUE)
	s2 <- solve_dominosa(pips2)
	# Verify all 6 domino types present and each pip pair is valid
	expect_setequal(as.vector(s2$domino_ids), 1:6)
	for (d in 1:6) {
		cells <- which(s2$domino_ids == d, arr.ind = TRUE)
		expect_equal(nrow(cells), 2L)
		p <- sort(c(pips2[cells[1, 1], cells[1, 2]], pips2[cells[2, 1], cells[2, 2]]))
		expected_pip <- switch(
			d,
			`1` = c(0L, 0L),
			`2` = c(0L, 1L),
			`3` = c(0L, 2L),
			`4` = c(1L, 1L),
			`5` = c(1L, 2L),
			`6` = c(2L, 2L)
		)
		expect_equal(p, expected_pip)
	}
})

test_that("solve_dominosa() errors on invalid input", {
	# Wrong grid size
	expect_snapshot(
		error = TRUE,
		solve_dominosa(matrix(c(0L, 0L, 1L, 1L), nrow = 1L))
	)
	# Wrong pip distribution: pip 0 appears 2 times, pip 1 appears 4 times
	expect_snapshot(
		error = TRUE,
		solve_dominosa(matrix(c(1L, 1L, 1L, 1L, 0L, 0L), nrow = 2L, byrow = TRUE))
	)
	# Valid distribution but no solution exists
	expect_snapshot(
		error = TRUE,
		solve_dominosa("010/101")
	)
	# Unbalanced strings
	expect_snapshot(
		error = TRUE,
		solve_dominosa("0101/10")
	)
	# Wrong type
	pips1 <- matrix(
		c(
			0L,
			0L,
			1L,
			1L,
			0L,
			1L
		),
		nrow = 2L,
		byrow = TRUE
	)
	expect_snapshot(
		error = TRUE,
		solve_dominosa(as.data.frame(pips1))
	)
})
