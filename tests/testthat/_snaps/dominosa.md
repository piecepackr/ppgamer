# solve_dominosa() works as expected

    Code
      cat(s1$ppn)
    Output
      ---
      GameType: Dominosa
      SetUp: None
      ...
      Solution. `0-0'<@(1.5,2) `1-0'<@(1.5,1) `1-1'@(3,1.5)

---

    Code
      ppn::cat_move(g1, move = "Solution.", color = FALSE)
    Output
       ┌───┬─┐
       │ ┃ │·│
       ├───┤━│
       │·┃ │·│
       └───┴─┘
              

# solve_dominosa() errors on invalid input

    Code
      solve_dominosa(matrix(c(0L, 0L, 1L, 1L), nrow = 1L))
    Condition
      Error in `validate_dominosa_pips()`:
      ! For a double-1 domino set the grid must have 6 cells, but `pips` has 4 cells.

---

    Code
      solve_dominosa(matrix(c(1L, 1L, 1L, 1L, 0L, 0L), nrow = 2L, byrow = TRUE))
    Condition
      Error in `validate_dominosa_pips()`:
      ! For a double-1 domino set each pip value must appear exactly 3 time(s). The following pip values have incorrect counts: 0, 1.

---

    Code
      solve_dominosa("010/101")
    Condition
      Error in `solve_dominosa()`:
      ! No solution exists for this Dominosa puzzle.

---

    Code
      solve_dominosa("0101/10")
    Condition
      Error in `parse_dominosa_pips()`:
      ! All rows in `pips` must have the same length.

---

    Code
      solve_dominosa(as.data.frame(pips1))
    Condition
      Error in `parse_dominosa_pips()`:
      ! `pips` must be a matrix or character string, not data.frame.

