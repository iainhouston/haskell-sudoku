## Plan: Add Sudoku generator and file-based solver

TL;DR - extend `Sudoku.hs` so it can generate a new puzzle and save it to a text file, then read that file back in to solve it.

**Steps**
1. Add file I/O helpers to `Sudoku.hs`.
   - `readGridFile :: FilePath -> IO Grid`
   - `writeGridFile :: FilePath -> Grid -> IO ()`
   - Use a 9-line text format with digits '1'..'9' and '0' for blanks.

2. Add a random full-solution generator.
   - Use the existing solver-based search pattern with a randomized digit order.
   - Implement a `shuffle` helper and `generateSolution :: IO Grid`.

3. Create a puzzle generator from a full solution.
   - Implement `makePuzzle :: Grid -> Int -> IO Grid` or similar.
   - Remove cells randomly while preserving a unique solution using the existing `solve` function.
   - Choose a default blank count (e.g. 40 blanks) so generated puzzles are solvable and reasonably challenging.

4. Add command-line mode selection in `main`.
   - `generate <output-file>` generates and writes a puzzle.
   - `solve <input-file>` reads a puzzle file and solves it.
   - Optionally support `help` or default with the built-in `puzzle` constant.

5. Keep the current solver intact and reuse its `solve`, `pretty`, `rows`, `cols`, `boxes`, and pruning/search logic.

**Verification**
1. Run `runhaskell Sudoku.hs generate puzzle.txt` and verify `puzzle.txt` has 9 lines of 9 digits.
2. Run `runhaskell Sudoku.hs solve puzzle.txt` and verify it prints a valid solution and total solution count.
3. Confirm the generated puzzle has exactly one solution by observing the solver count or by checking generated puzzles with multiple runs.

**Decisions**
- Use a single executable with separate `generate` and `solve` modes rather than two separate programs.
- Use a simple text file format for puzzles, matching your requested 9x9 digit layout.
- Keep puzzle generation deterministic enough to produce valid output, with uniqueness ensured by re-solving after removing cells.

**Further Considerations**
1. If you want difficulty levels later, add an optional blank-count argument to `generate`.
2. If you want multiple puzzles at once, add a `count` argument to `generate`.
