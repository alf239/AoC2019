# Advent of Code 2019
## Solutions by Alexey Filippov

This repository is for solutions for [Advent of Code 2019](https://adventofcode.com/2019) by me.
The code you see is not necessarily the code that produced the submitted result - as I go and re-use
the earlier code, I refactor it as I see fit. I cannot promise that everything works, either - 
going back and forth is time consuming, so if you want to see that code that did run, search for the 
version on the day the task was published - it's likely to be the best version, as it avoided 
the second-system effects.

I do not claim full ownership of the solutions' code. For example, the brilliant `pairs = zip <*> tail` hack is taken from StackOverflow; 
when I'm struggling to find a proper import (e.g. day 4), I copy Haskell library functions from Hoogle/Hackage.

This repository is merely a scratch pad for me to look up the tasks I can build upon.

 1. Simple exercise on `iterate` - add fuel until enough to carry itself
 2. A simple runtime for "Intcode" machine: `add` and `mul` for now, indirect addressing
 3. Search for intersection of wires, two definitions of distance
 4. Patterns in numbers' decimary representation. Run-length encoding
 5. Add `jit`, `jif`, `in` and `out` instructions for Intcode interpreter
 6. Distances over tree - sum of all distances from a point, shortest distance in an ordered tree
 7. Run Intcode until blocking output, encapsulate the run
 8. A general task, speed coding `chunkOf n` and `reduce`/`foldl`
 9. Full Intcode computer with relative address mode
