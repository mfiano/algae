# algae

Assortment of Lisp Game Algorithms and Experiments

## Overview

**WARNING**: Code in this repository is experimental. APIs may not be complete and may change often,
or packages may be removed entirely. If you are brave enough to use this system and a previously
existing package is no longer present, one of two things happened: 1) It has been abandoned. 2) It
has been lifted and polished into a stand-alone library outside of this repository.

This project houses some mostly self-contained algorithms, data structures, and experiments that
have proven useful at least once during my many years of game development in Common Lisp. There are
some ideas that may overlap other domains outside of game development, but all of my work is in this
field, so I'm not sure how useful they'd be.

It is broken up into separate packages for each idea, with some more complex ideas arranged into
sub-packages. If time was on my side, there should be a separate basic documentation in the the
particular idea's top-level comments, but there are no promises here -- a lot of this code was quick
exploratory ideas that haven't been fleshed out completely, and haven't been revisited since.

While this repository is mostly for personal use, I welcome any feedback or questions you may have,
and if you find anything useful for your own pursuit, I'd be happy to hear from you.

## Install

```lisp
(ql:quickload :algae)
```

## Usage

See individual idea code comments, package definition, examples, etc.

## License

Copyright © 2020-2021 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
