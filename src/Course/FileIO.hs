{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
  getArgs >>= \l -> case l of
                      Nil -> putStrLn "No argument provided"
                      h:._ -> run h

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run a =
  do
    f <- readFile a -- <- means "got bind by"
    c <- getFiles (lines f)
    printFiles c 
--  readFile a >>= \f ->
--  getFiles (lines f) >>= \c -> 
--  printFiles c
  
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . ((<$>) getFile)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile = 
  lift2 (<$>) (,) readFile
--  do a <- (,)
--     b <- readFile
--     pure(a <$> b)  
--getFile f =
--  ((,) f) <$> readFile f
--  (\c -> (f, c)) <$> readFile f
--  do
--    c <- readFile f
--    pure (f, c)
--  readFile f >>= \c -> pure (f, c)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
--printFiles x =
  --void (sequence ((\(p, c) -> printFile p c) <$> x))
  --void (sequence ((<$>) (uncurry printFile) x))
printFiles = 
  void . sequence . (<$>) (uncurry printFile)
  
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile p c =
  putStrLn ("the name if the file: " ++ p) *>
  putStrLn ("the content: " ++ c)

