# parse_fujisan_pawns() works as expected

    Code
      parse_fujisan_pawns("S12M")
    Condition
      Error in `parse_fujisan_pawns()`:
      ! The `pawns` string must have exactly one '/' separator but got: 'S12M'

---

    Code
      parse_fujisan_pawns("S12M/A11C")
    Condition
      Error in `parse_fujisan_pawn_row()`:
      ! Row 2 of pawns string 'S12M/A11C' covers 13 columns but must cover exactly 14

---

    Code
      parse_fujisan_pawns("S13M/A12C")
    Condition
      Error in `parse_fujisan_pawn_row()`:
      ! Row 1 of pawns string 'S13M/A12C' covers 15 columns but must cover exactly 14

---

    Code
      parse_fujisan_pawns("S13/A12C")
    Condition
      Error in `parse_fujisan_pawns()`:
      ! The `pawns` string must contain exactly 4 pawns but got 3: 'S13/A12C'

# fujisan solver works as expected

    Code
      ppn::cat_move(g2, move = "SetupFn.", color = FALSE)
    Output
         ┌─┬─┬─┬─┬─┬─┰─┬─┬─┬─┬─┬─┐  
        ☀⃟│4⃝│4⃝│4⃝│5⃝│2⃝│n⃝┃2⃝│4⃝│n⃝│3⃝│a⃝│a⃝│☾⃟ 
         ┝━┿━┿━┿━┿━┿━╋━┿━┿━┿━┿━┿━┥  
        ⸸⃟│a⃝│2⃝│5⃝│3⃝│3⃝│5⃝┃3⃝│2⃝│5⃝│a⃝│n⃝│n⃝│♛⃟ 
         └─┴─┴─┴─┴─┴─┸─┴─┴─┴─┴─┴─┘  
                                    

