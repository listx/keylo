# Keylo - Keyboard Layout Optimizer

Given a set of constraints, desired properties, etc., Keylo generates the optimal keyboard layout for you.
In other words, you can make your own layout in the spirit of Dvorak that is customized for your needs.

The problem with the existing programs out there was that they all enforced a strict US ANSI 104-key IBM-style keyboard; so, I wrote this program to accept any arbitrary type of keyboard layout.

## How it Works

Currently the "display this layout to STDOUT" code is customized specifically for the Esrille Nisse keyboard.
But, it should be quite trivial to make it support any other kind of keyboard, as the underlying optimization algorithm does not care whether it's a 104-key, 87-key, or any other keyboard.

Basically, Keylo takes an input corpus and uses it as a reference for giving weights to important letters, bigrams, and words.
The more frequent it is, the more influence it has in determining the "score" of a particular arrangement of keys.
Keylo modifies the arrangement and scores it, and chooses the modified layout as the new "best" layout if it has a better score (if you give the `-a ARandom` flag).
If you give it the `-a ASimAnneal` flag it uses a simulated annealing formula for probabilistically determining whether to accept the better score or not.
In practice, the two algorithms seem to perform similarly, as the underlying mutation algorithm is already heavily modified to prefer only modifying poorly-positioned keys.

# Installation

Install with cabal.
In NixOS, I do

```
cabal2nix --shell . > shell.nix
nix-shell
cabal sandbox init
cabal build
```

# Example

```
cabal build && time ./dist/build/keylo/keylo -a ASimAnneal --corpus ~/prog/keylo/corpus/list.txt -b ~/prog/keylo/corpus/blacklist.txt --rng-seed 0,0 --threads 8 --time 2000 --rounds=100
```

# TODO

- Make a much smaller data structure and use this during the search, so that we don't get huge gigabytes of RAM usage during big searches
- Add 104-key layout (and compare it to other keyboard layout optimizer programs?)
- Load default layout's seed state from an external file, rather than hardcoding it in `Layout.lhs`
- Make the penalty system more intuitive and tweakable on the fly (again, not hardcoded)

# Contributing

Pull requests welcome.
