literate-ludum-dare-31
======================

Literate -- A game made in Haskell for the 31st Ludum Dare

## Building and running
This library depends on the `Lambency` framework, which can be [found here](https://github.com/Mokosha/Lambency). Once
that repository is cloned, the rest of the steps are straightforward:

    $ cd path/to/this/repo
    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal install path/to/Lambency/repo
    $ cabal configure
    $ cabal run LD31

## How to Play
Literate (as in liberate, but with a "T") is a game that tests to see how many words you can gather in a limited amount
of time. Once you start the game, click on letters that appear until you've spelled a word. Once you believe that you have
spelled a word, hit the space bar to clear the letters. If you've written a word, you will receive points equal to the length
of the word! The points are accrued in the top row, while the timer is in the bottom.
