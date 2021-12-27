# AdventOfCode2021

Solutions to Advent of Code 2021. I've used Python for the last two years, so this time I'll try something completely different: LISP. Specifically, the [SICP Scheme](https://docs.racket-lang.org/sicp-manual/index.html) language package for [Racket](https://racket-lang.org/), because [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html) is the best documentation I've found for learning this.

I plan to at least try each day with Scheme, but I might end up going back to Python for some of them.

Edit 2021-12-04: Switching from SICP scheme to Racket. This just means I'm using the whole language instead of a subset designed to accompany one specific textbook. There's a much larger standard library, including some data structures I'm sure will be useful later.

Edit 2021-12-27: I'm not quite finished yet (still haven't done day 19), but I think I can summarize the experience so far. My goal was to learn a new and different language, and I definitely accomplished that. There were a few days where information from the subreddit proved useful, but overall I managed to write each day's program without just copying the whole algorithm. I did copy Dijkstra's algorithm for day 15, but he never posted it on Reddit so that's OK.

I didn't need to use Python at all. I did need to use mutable data once (because Dijkstra's algorithm runs super slow if you implement it with linked lists), and some things like graphics and file access rely on side effects, but apart from that it's 100% functional programming. I see the appeal, even though I'm not about to replace loops with tail recursion in all my other programs.
