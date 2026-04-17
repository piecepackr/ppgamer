test_that("parse_fujisan_pawns() works as expected", {
	expect_equal(parse_fujisan_pawns("S12M/A12C"), c(1L, 14L, 15L, 28L))
	expect_equal(parse_fujisan_pawns("6SM6/6AC6"), c(7L, 8L, 21L, 22L))
	expect_equal(parse_fujisan_pawns("S12S/A12A"), c(1L, 14L, 15L, 28L))
	expect_equal(parse_fujisan_pawns("6PP6/6PP6"), c(7L, 8L, 21L, 22L))
	expect_snapshot(error = TRUE, parse_fujisan_pawns("S12M"))
	expect_snapshot(error = TRUE, parse_fujisan_pawns("S12M/A11C"))
	expect_snapshot(error = TRUE, parse_fujisan_pawns("S13M/A12C"))
	expect_snapshot(error = TRUE, parse_fujisan_pawns("S13/A12C"))
})

test_that("fujisan solver works as expected", {
	coins <- "235334140030554141221205"
	dice <- "0000"
	s <- solve_fujisan(coins = coins, dice = dice)
	expect_length(s$shortest_path, 0)
	expect_true(is.na(s$shortest_distance))

	coins <- "2nnna244naa5/335a45235432"
	dice <- "a2/55"
	s <- solve_fujisan(coins = coins, dice = dice)
	expect_length(s$shortest_path, 13)
	expect_equal(s$shortest_distance, 12)

	skip_if_not_installed("ppn")
	skip_if_not_installed("ppcli")
	skip_if_not_installed("withr")
	skip_on_os("windows")
	expect_equal(
		{
			withr::local_seed(42)
			random_fujisan_coins()
		},
		# fmt: skip
		matrix(
			c(
				1L, 2L, 3L, 1L, 1L, 3L, 0L, 3L, 5L, 4L, 0L, 2L,
				2L, 4L, 4L, 1L, 0L, 3L, 4L, 5L, 0L, 5L, 5L, 2L
			),
			nrow = 2L
		)
	)

	puzzle2 <- matrix(
		# fmt: skip
		c(
			4, 4, 4, 5, 2, 0, 2, 4, 0, 3, 1, 1,
			1, 2, 5, 3, 3, 5, 3, 2, 5, 1, 0, 0
		),
		nrow = 2,
		byrow = TRUE
	)
	s2 <- solve_fujisan(coins = puzzle2)
	expect_equal(solve_fujisan(coins = puzzle2, pawns = "6SM6/6AC6")$shortest_distance, 0L)
	g2 <- ppn::read_ppn(textConnection(s2$ppn))[[1]]
	expect_snapshot(ppn::cat_move(g2, move = "SetupFn.", color = FALSE))

	saved_ppn <- paste(readLines(system.file("ppn/fujisan.ppn", package = "ppn")), collapse = "\n")
	expect_equal(str_wrap(s2$ppn), str_wrap(saved_ppn))
})
