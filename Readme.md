# What is this?

This is a procedurally generated Minesweeper game written in Haskell using the
[`Reflex-DOM` framework](https://github.com/reflex-frp/reflex-dom).

Instead of placing mines at the beginning of the game all cells start out with
an undefined state. Only when the player clicks on a cell to reveal it the state
of it and all surrounding cells will be fixed to either contain a mine or be
safe.

This way the distribution of the mines on the board depends on the actions of
the player and in which order the cells are revealed.

Using [`GHCJS`](https://github.com/ghcjs/ghcjs) this project can be
transpiled to JavaScript and be deployed as a static web app.

It can also be built with `GHC` producting a native executable using `WebkitGtk`
as a _browser_.

You can play a live demo of this project [here](https://acran.github.io/haboom/).

# Context

This project was made for a univerity course on
[advanced functional programming](https://www.tcs.ifi.lmu.de/lehre/ss-2020/fun).

It was inspired by and heavily based on

* [Kaboom](https://github.com/pwmarcz/kaboom) - a **cruel, but fair** Minesweeper
  game by Paweł Marczewski ([pwmarcz.pl](https://pwmarcz.pl))

The files in the [`css/`](css/) directory are copied directly from `Kaboom`.

# How to install and build

## Cloning from GitHub

Download the code with `git clone` from GitHub:
~~~sh
git clone --recurse-submodules https://github.com/acran/haboom.git
~~~

## Using `nix`

The `reflex` framework recommends using the [Nix package manager](https://nixos.org/).

With `nix` already installed all required dependencies and compilers can be
download with the `try-reflex` script included in the `reflex-platform`
directory:
~~~sh
# install all dependencies and enter reflex sandbox
reflex-platform/try-reflex

# in the reflex sandbox build with ghc/ghcjs

ghcjs Main.hs # build as static web app
cp -r css/ Main.jsexe/ # add static assets to build result
# open Main.jsexe/index.html with a browser

ghc Main.hs # build as native executable
./Main # start executable
~~~

On the first run of `try-reflex` about 14GB of dependencies will be downloaded.
For more information see the
[`reflex-platform` documentation](https://github.com/reflex-frp/reflex-platform/#setup).

## Using `docker`

If you just want to build the project you can also use the included `Dockerfile`
to execute the above steps in a `docker` container:
~~~sh
# build image with name 'haboom'
docker build -t haboom .
# build with ghcjs in a container named 'haboom'
docker run --name haboom haboom ghcjs Main.hs
# copy build product from container
docker cp haboom:/haboom/Main.jsexe ./Main.jsexe

cp -r css/ Main.jsexe/ # add static assets to build result
# open Main.jsexe/index.html with a browser
~~~

When the build seems to hang because the output has stopped just be patient and
wait until the build finishes.

## Building the documentation

You can use `haddock` to build a browsable source code documentation:
~~~sh
haddock --odir=docs --html --pretty-html Main.hs Game/*.hs Gui/*.hs

# or using the docker image
docker run --name haboom haboom haddock --odir=docs --html --pretty-html Main.hs Game/*.hs Gui/*.hs
docker cp haboom:/haboom/docs/ docs

# open docs/index.html with a browser
~~~

You can find an online version of the documentation
[here](https://acran.github.io/haboom/docs/).

# Known issues

* The game is not guaranteed to be solvable without guessing, and if the player
  is forced to guess it is not guaranteed to be safe - unlike with `Kaboom`!
* The [placement algorithm](https://github.com/acran/haboom/blob/master/Game/Logic.hs#L40-L46)
  is actually _really_ crude. It was first implemented as a placeholder but
  incidentally worked out quite good for small field sizes with few mines. For
  bigger field sizes and many mines unfortunately the mines end up in one big
  cluster most of the times.
* The performance is not very optimized and results in noticable delays with
  bigger field sizes.
