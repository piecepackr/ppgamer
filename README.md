# ppgamer

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/ppgamer)](https://cran.r-project.org/package=ppgamer)
[![R-CMD-check](https://github.com/piecepackr/ppgamer/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/piecepackr/ppgamer/actions)
[![codecov](https://codecov.io/github/piecepackr/ppgamer/branch/main/graph/badge.svg)](https://app.codecov.io/github/piecepackr/ppgamer)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Related links](#links)

## <a name="overview">Overview</a>

* Currently this package only provides a [Fuji-san](https://www.ludism.org/ppwiki/Fuji-san) solver `solve_fujisan()` which can compute the shortest solution (if it exists) to a given Fuji-san puzzle and output the PPN text to record/visualize the solution.
* In the future it may provide computer players for other board games and/or puzzles.
* This is an extraction and refinement of functionality originally contained in the experimental [{ppgames}](https://www.github.com/piecepackr/ppgames) package.

## <a name="installation">Installation</a>


```r
remotes::install_github("piecepackr/ppgamer")
```

## <a name="examples">Examples</a>

```r
library("igraph")
library("piecepackr")
library("ppgamer")
library("ppn") # `remotes::install_github("piecepackr/ppn")`
puzzle2 <- matrix(c(4,4,4,5,2,0,2,4,0,3,1,1,
                    1,2,5,3,3,5,3,2,5,1,0,0), nrow=2, byrow=TRUE)
s2 <- solve_fujisan(coins=puzzle2)
game <- read_ppn(textConnection(s2$ppn))[[1]]

dark_colorscheme <- list(suit_color="darkred,black,darkgreen,darkblue,black",
                      invert_colors.suited=TRUE, border_color="black", border_lex=2)
piecepack_suits <- list(suit_text="\U0001f31e,\U0001f31c,\U0001f451,\u269c,\uaa5c", # 🌞,🌜,👑,⚜,꩜
                    suit_fontfamily="Noto Emoji,Noto Sans Symbols2,Noto Emoji,Noto Sans Symbols,Noto Sans Cham",
                    suit_cex="0.6,0.7,0.75,0.9,0.9")
traditional_ranks <- list(use_suit_as_ace=TRUE, rank_text=",a,2,3,4,5")
cfg3d <- list(width.pawn=0.75, height.pawn=0.75, depth.pawn=0.375, 
                   dm_text.pawn="", shape.pawn="convex6", invert_colors.pawn=TRUE,
                   edge_color.coin="tan", edge_color.tile="tan")
cfg <- pp_cfg(c(piecepack_suits, dark_colorscheme, traditional_ranks, cfg3d))

animate_game(game, op_scale=1, op_angle=90, trans=op_transform, cfg=cfg, file="fujisan.gif")
```

![Animation of a Fuji-san game](https://www.trevorldavis.com/piecepackr/images/knitr/fujisan.gif)

## <a name="links">Related links</a>

### R packages

* [{piecepackr}](https://github.com/piecepackr/piecepackr)
* [{ppcli}](https://github.com/piecepackr/ppcli)
* [{ppdf}](https://github.com/piecepackr/ppdf)
* [{ppdn}](https://github.com/piecepackr/ppn)
