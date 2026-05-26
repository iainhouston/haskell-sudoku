module Main where

-- Strategy overview
-- =================
-- Solving uses two phases applied in sequence:
--   1. Constraint propagation (prune): for each row, column, and 3x3 box,
--      if a cell is fixed to a single digit, remove that digit from all other
--      cells in the same group.  Repeat until no further reductions are possible.
--   2. Backtracking search: if propagation alone doesn't finish the puzzle,
--      pick an unsettled cell, try each of its remaining candidates in turn,
--      re-prune after each assignment, and recurse.  Dead ends are abandoned
--      immediately; all surviving branches are collected into a list of solutions.
--
-- Puzzle generation reverses this: first build a complete, valid grid by running
-- the same search with candidate digits shuffled randomly, then "dig out" cells
-- one at a time — keeping a cell blank only when the puzzle still has a unique
-- solution.

import Data.List (intercalate, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------
-- Type aliases give domain names to otherwise generic Haskell types.
-- They are purely for readability; the compiler treats them as identical to
-- their underlying types.

type Matrix a = [Row a]      -- a 9x9 grid of anything
type Row a    = [a]          -- one horizontal row

type Grid    = Matrix Digit  -- a grid of single characters
type Digit   = Char          -- '1'..'9', or '0' for a blank cell
type Choices = [Digit]       -- the set of digits still possible for one cell

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

-- ---------------------------------------------------------------------------
-- Solver entry point
-- ---------------------------------------------------------------------------
-- `solve` is a pipeline: choices . prune . search
--   choices  : lift each cell from a single Digit to its list of Choices
--   prune    : apply constraint propagation to narrow those lists
--   search   : backtrack over whatever ambiguity remains
-- The result is a list of all solutions (usually length 0 or 1).

solve :: Grid -> [Grid]
solve = search . prune . choices

-- ---------------------------------------------------------------------------
-- Phase 1a — lift a Grid to a Matrix of Choices
-- ---------------------------------------------------------------------------
-- A fixed cell becomes a singleton list [d]; a blank cell becomes the full
-- list ['1'..'9'].  This uniform representation lets prune and search work
-- without special-casing blanks.

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where
    choice d = if blank d then digits else [d]

-- ---------------------------------------------------------------------------
-- Phase 1b — constraint propagation (prune)
-- ---------------------------------------------------------------------------
-- The key insight: if a cell in a group (row / col / box) is already fixed to
-- digit d (its Choices list has length 1), then d cannot appear in any other
-- cell of that group, so we remove d from their Choices lists.
--
-- `pruneBy f` applies that reduction along one axis:
--   f    — extracts the groups (rows, cols, or boxes) from the matrix
--   reduce — does the actual elimination within one group
--   f again — puts the matrix back in original orientation
-- We apply pruneBy for all three axes in sequence.

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map reduce . f

-- `reduce` receives one group (nine Choices lists) and collects every digit
-- that is already fixed (singleton list).  It then strips those "fixed" digits
-- from every non-singleton cell in the group.
-- The list comprehension `[d | [d] <- xss]` pattern-matches each element of
-- xss against the single-element list pattern [d], silently skipping lists of
-- any other length.

reduce :: [Choices] -> [Choices]
reduce xss = map (remove fixed) xss
  where fixed = [d | [d] <- xss]

-- `remove` strips forbidden digits from a cell's Choices.
-- The first equation is the guard: never touch a cell that is already fixed
-- (length 1), or we might erase its only candidate.

remove :: [Digit] -> Choices -> Choices
remove _  [x] = [x]
remove ds xs  = filter (`notElem` ds) xs

-- ---------------------------------------------------------------------------
-- Phase 2 — backtracking search
-- ---------------------------------------------------------------------------
-- `search` returns *all* solutions as a lazy list.  Callers that want only one
-- solution (like `solveAndPrint`) take `head`, which stops the search early
-- thanks to Haskell's lazy evaluation.
--
-- Three cases:
--   blocked — a cell has no candidates, or a group has a duplicated singleton:
--             this branch is a dead end; return the empty list (no solutions).
--   solved  — every cell is a singleton: extract the digits and return the grid.
--   otherwise — pick an unsettled cell, produce one candidate matrix for each
--               of its choices (expand), prune each, and recurse.
--               `concatMap` flattens the list-of-lists of solutions.

search :: Matrix Choices -> [Grid]
search cm
  | blocked cm = []
  | solved cm  = [map (map head) cm]
  | otherwise  = concatMap (search . prune) (expand cm)

-- Every cell is a singleton — the puzzle is fully determined.
solved :: Matrix Choices -> Bool
solved = all (all ((== 1) . length))

-- A branch is dead if any cell lost all its candidates (hasEmpty) or if two
-- fixed cells in the same group carry the same digit (not safe).
blocked :: Matrix Choices -> Bool
blocked cm = hasEmpty cm || not (safe cm)

hasEmpty :: Matrix Choices -> Bool
hasEmpty = any (any null)

-- `safe` checks all three group types.  A group is ok if no digit appears
-- in more than one fixed (singleton) cell.
safe :: Matrix Choices -> Bool
safe cm =
  all okGroup (rows cm)  &&
  all okGroup (cols cm)  &&
  all okGroup (boxes cm)

-- Extract all fixed digits from the group and verify there are no duplicates.
okGroup :: [Choices] -> Bool
okGroup css = nodups [c | [c] <- css]

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = x `notElem` xs && nodups xs

-- ---------------------------------------------------------------------------
-- Branching — expand a matrix on the first ambiguous cell
-- ---------------------------------------------------------------------------
-- Given a matrix with at least one unsettled cell, `expand` returns a list
-- of matrices — one per candidate of the first ambiguous cell — each
-- identical to the input except that cell is fixed to a single digit.
--
-- `break` is applied twice to locate the target:
--   (rows1, row:rows2) — splits the matrix at the first row containing an
--                         ambiguous cell; rows1 are all-singleton rows.
--   (row1, best:row2)  — splits that row at the first ambiguous cell;
--                         best is its Choices list (length > 1).
--
-- The list comprehension rebuilds the full matrix for each digit c in best:
--   [c]                     wraps c into a singleton Choices
--   [c] : row2              prepends it to the trailing cells  (parsed as
--   row1 ++ ([c] : row2)    this because : and ++ share infixr 5 precedence)
--   [row1 ++ [c] : row2]    wraps the rebuilt row in a list so ++ can splice
--   rows1 ++ [...] ++ rows2 restores the untouched rows either side
--
-- Using the *first* ambiguous cell is fine for correctness; `randomSolve`
-- uses `bestCell` instead, which picks the most-constrained cell for speed.

expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- best]
  where
    -- Split the matrix at the first row containing an ambiguous cell.
    (rows1, row : rows2) = break (any ((> 1) . length)) cm
    -- Split that row at the first ambiguous cell.
    (row1, best : row2)  = break ((> 1) . length) row

-- ---------------------------------------------------------------------------
-- Matrix orientations — rows, cols, boxes
-- ---------------------------------------------------------------------------
-- The solver works on rows by default.  To prune/search along columns or
-- boxes, we *transpose* the matrix into that orientation, do the work, then
-- transpose back.  Because transposing twice is the identity, `pruneBy f`
-- applies `f` before and after `reduce`.

-- `rows` is the identity; included for symmetry.
rows :: Matrix a -> Matrix a
rows = id

-- `cols` transposes: element [r][c] moves to [c][r].
-- The recursive definition zips the first elements of each row into one new
-- row, then recurses on the tails.
cols :: Matrix a -> Matrix a
cols []         = []
cols [xs]       = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

-- `boxes` rearranges so that each 3x3 sub-grid becomes one "row" of the result.
-- Tactic: chunk each row into triples (group), take the cols of three adjacent
-- chunked rows (that collects each box's cells into one list), then flatten.
-- Applying `boxes` twice returns the original layout, so it is its own inverse.
boxes :: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map cols . group . map group

-- Split a flat list into consecutive triples.
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

-- Flatten one level of list nesting.
ungroup :: [[a]] -> [a]
ungroup = concat

-- ---------------------------------------------------------------------------
-- Puzzle generator
-- ---------------------------------------------------------------------------

-- An all-blank starting grid — the seed for random solution generation.
blankGrid :: Grid
blankGrid = replicate 9 (replicate 9 '0')

-- High-level generator:
--   1. Build a random complete solution.
--   2. Shuffle the 81 cell positions so we visit them in random order.
--   3. Try removing cells one at a time, keeping the blank only if the
--      puzzle still has exactly one solution.
generatePuzzle :: Int -> IO Grid
generatePuzzle blanks = do
  full      <- fromMaybe (error "Failed to generate a full Sudoku solution") <$> randomSolution
  positions <- shuffle [(r, c) | r <- [0..8], c <- [0..8]]
  removeCells blanks positions full

-- Kick off a random solve from the blank grid.
randomSolution :: IO (Maybe Grid)
randomSolution = randomSolve (choices blankGrid)

-- Like `search`, but IO-based so it can shuffle candidates before branching.
-- Shuffling the digit order at each branch point produces a different random
-- complete grid on each run.
-- Returns `Maybe Grid` rather than a list because we only need one solution.
randomSolve :: Matrix Choices -> IO (Maybe Grid)
randomSolve cm
  | blocked cm = return Nothing
  | solved cm  = return $ Just (map (map head) cm)
  | otherwise  = do
      let (r, c, cs) = bestCell cm   -- most-constrained cell
      cs' <- shuffle cs              -- randomise the order we try digits
      tryChoices r c cs'
  where
    tryChoices _ _ []     = return Nothing
    tryChoices r c (d:ds) = do
      result <- randomSolve (prune (setChoice cm r c [d]))
      case result of
        Just _  -> return result
        Nothing -> tryChoices r c ds

-- Find the unsettled cell with the fewest remaining candidates.
-- This "minimum remaining values" heuristic reduces the branching factor and
-- makes the random solver much faster.
bestCell :: Matrix Choices -> (Int, Int, Choices)
bestCell cm = (r, c, cs)
  where
    ((r, c), cs) = minimumBy compareChoices candidates
    candidates   = [((r, c), cs) | (r, row) <- zip [0..] cm
                                  , (c, cs)  <- zip [0..] row
                                  , length cs > 1]
    compareChoices (_, a) (_, b) = compare (length a) (length b)

-- Replace the Choices at position (r, c) in the matrix with a new list.
setChoice :: Matrix Choices -> Int -> Int -> Choices -> Matrix Choices
setChoice cm r c cs = [if i == r then updateRow row else row | (i, row) <- zip [0..] cm]
  where
    updateRow row = [if j == c then cs else x | (j, x) <- zip [0..] row]

-- Walk the shuffled position list, blanking cells one at a time.
-- A cell is left blank only if the resulting grid still has exactly one
-- solution.  We stop as soon as `blanks` cells have been successfully removed,
-- or the position list is exhausted (in which case we return the best we got).
removeCells :: Int -> [(Int, Int)] -> Grid -> IO Grid
removeCells 0 _ g = return g   -- reached the target blank count
removeCells _ [] g = return g  -- ran out of positions to try
removeCells n ((r,c):rest) g
  | current == '0' = removeCells n rest g   -- already blank, skip
  | otherwise = do
      let g' = updateGrid g r c '0'
      if uniqueSolution g'
        then removeCells (n - 1) rest g'    -- blank accepted
        else removeCells n rest g           -- would break uniqueness, skip
  where
    current = g !! r !! c

-- ---------------------------------------------------------------------------
-- Uniqueness check — used during puzzle generation
-- ---------------------------------------------------------------------------

uniqueSolution :: Grid -> Bool
uniqueSolution g = countSolutions g == 1

-- Run the solver but stop as soon as a second solution is found.
-- We never need to count beyond 2: the result is either 0, 1, or "2+"
-- (which `uniqueSolution` treats as "not unique").
countSolutions :: Grid -> Int
countSolutions = searchCount . prune . choices

-- `searchCount` mirrors `search` but accumulates an integer count and carries
-- a `limit` so it can short-circuit.  The inner `go` loop processes
-- alternatives left to right, stopping as soon as the running total hits
-- the limit (here, 2).
searchCount :: Matrix Choices -> Int
searchCount cm = searchCount' cm 2
  where
    searchCount' cm' limit
      | blocked cm' = 0
      | solved cm'  = 1
      | otherwise   = go (expand cm') 0
      where
        go [] acc             = acc
        go _ acc | acc >= limit = acc   -- early exit
        go (x:xs) acc         = go xs (acc + searchCount' x limit)

-- Replace the digit at position (r, c) in a Grid.
updateGrid :: Grid -> Int -> Int -> Digit -> Grid
updateGrid g r c d = [if i == r then updateRow row else row | (i, row) <- zip [0..] g]
  where
    updateRow row = [if j == c then d else x | (j, x) <- zip [0..] row]

-- ---------------------------------------------------------------------------
-- Pseudo-random number generation
-- ---------------------------------------------------------------------------
-- We use a simple linear congruential generator (LCG) seeded from the system
-- clock, avoiding a dependency on the `random` package.
-- The LCG parameters (multiplier 1103515245, increment 12345, modulus 2^31)
-- are the classic values from C's stdlib — good enough for puzzle generation.

type Seed = Int

-- Seed from the current POSIX time in milliseconds.
getSeed :: IO Seed
getSeed = round . (* 1000) <$> getPOSIXTime

-- Advance the LCG by one step.
nextSeed :: Seed -> Seed
nextSeed s = (1103515245 * s + 12345) `mod` 2147483648

-- Draw a random integer in [lo, hi] and return the next seed.
randomR' :: (Int, Int) -> Seed -> (Int, Seed)
randomR' (lo, hi) seed = (lo + seed' `mod` (hi - lo + 1), seed')
  where seed' = nextSeed seed

-- IO-facing shuffle: obtain a fresh seed and delegate to the pure version.
shuffle :: [a] -> IO [a]
shuffle xs = fst . shuffle' xs <$> getSeed

-- Pure Fisher-Yates shuffle threaded through the LCG seed.
-- At each step, pick a random remaining element, prepend it to the result,
-- and recurse on what is left.
shuffle' :: [a] -> Seed -> ([a], Seed)
shuffle' [] seed = ([], seed)
shuffle' xs seed = (chosen : rest', seed'')
  where
    (i, seed')        = randomR' (0, length xs - 1) seed
    (chosen, remaining) = removeAt i xs
    (rest', seed'')   = shuffle' remaining seed'

-- Remove the element at index i, returning it and the shortened list.
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = (xs !! i, take i xs ++ drop (i + 1) xs)

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------
-- Puzzle files contain exactly 9 lines of 9 characters each.
-- Digits '1'..'9' are given cells; '0' is a blank.

readGridFile :: FilePath -> IO Grid
readGridFile path = do
  content <- readFile path
  case parseGrid content of
    Left err -> error err
    Right g  -> return g

writeGridFile :: FilePath -> Grid -> IO ()
writeGridFile path g = writeFile path (unlines g)

-- `parseGrid` uses the `Either String` monad for error propagation: `Left`
-- carries an error message, `Right` carries the valid grid.
-- `mapM` applies `parseLine` to every line and sequences the results — if any
-- line fails it short-circuits and returns the first `Left` error.
parseGrid :: String -> Either String Grid
parseGrid text = do
  let ls = lines text
  if length ls /= 9
    then Left "Puzzle file must contain exactly 9 lines."
    else mapM parseLine ls
  where
    parseLine line
      | length line /= 9 = Left "Each line in the puzzle file must contain exactly 9 characters."
      | all valid line   = Right line
      | otherwise        = Left "Puzzle file contains invalid characters; use digits 0-9."
    valid c = c == '0' || c `elem` digits

-- ---------------------------------------------------------------------------
-- Main program
-- ---------------------------------------------------------------------------

defaultBlankCount :: Int
defaultBlankCount = 40

usage :: String
usage = unlines
  [ "Usage:"
  , "  Sudoku generate <output-file> [blank-count]"
  , "  Sudoku solve <input-file>"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["generate", path] -> do
      puzzle' <- generatePuzzle defaultBlankCount
      writeGridFile path puzzle'
      putStrLn $ "Generated puzzle written to " ++ path
      putStr (pretty puzzle')
    ["generate", path, countStr] ->
      case readMaybe countStr of
        Just n | n >= 0 && n <= 81 -> do
          puzzle' <- generatePuzzle n
          writeGridFile path puzzle'
          putStrLn $ "Generated puzzle with " ++ show n ++ " blanks written to " ++ path
          putStr (pretty puzzle')
        _ -> putStrLn "Blank count must be an integer between 0 and 81."
    ["solve", path] -> do
      grid <- readGridFile path
      solveAndPrint grid
    [] -> putStrLn usage
    _  -> putStrLn usage

-- Print the first solution (if any) and the total count.
-- `solve` is lazy: `length sols` forces the full search, while `head sols`
-- stops at the first solution.  We traverse the list once for the count and
-- once for the first element, which is fine because Haskell caches the list.
solveAndPrint :: Grid -> IO ()
solveAndPrint grid =
  case solve grid of
    []   -> putStrLn "No solutions."
    sols -> do
      putStrLn "Solution:"
      putStr (pretty (head sols))
      putStrLn $ "(Total solutions: " ++ show (length sols) ++ ")"

-- ---------------------------------------------------------------------------
-- Pretty printing
-- ---------------------------------------------------------------------------
-- Render a Grid as a formatted 9x9 board with box dividers.
-- Each row is split into three 3-cell groups and rendered with vertical
-- separators so the 3x3 boxes are visually distinct.  A full-width separator
-- is inserted after every third row to highlight the box boundaries.
-- Blank cells are shown as spaces instead of the `0` sentinel.

pretty :: Grid -> String
pretty g = unlines $
  "+-------+-------+-------+" : concatMap rowLine (zip [1..] g)
  where
    rowLine (r, row) =
      ("| " ++ intercalate " | " (map (unwords . map prettyCell) (group row)) ++ " |") : ["+-------+-------+-------+" | r `mod` 3 == 0]

    prettyCell '0' = " "
    prettyCell c   = [c]
