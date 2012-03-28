Getting Yesod up and running for the first time is not always going smooth. There is a five-minute tutorial, but fact is that it can take significantly more time, especially if you're installing yesod for the first time.

Things not working don't need to be your fault! Sometimes the dependencies on the cabal packages can be wrong. If you find something wrong, let us know.

## Cabal Library vs cabal-install

A first practical detail that is good to know is the difference of the cabals.

The Cabal Library is a library that updates as you upgrade your ghc. cabal-install is cabal itself. Cabal library is often written capitilized, opposite to cabal-install.

Issuing `cabal --version` should print the version numbers for the two.

## Setting up Haskell, Cabal etc.

* ghc must be up to date.
* upgrade from ghc 6 to 7 to avoid most yesod-installing troubles. An old ghc also means an old Cabal Library.

### Ubuntu

*Ubuntu 11.10 have ghc 7.0.4 shipped in the repository, so most information below is superfluous.*

Ubuntu 11.04 have an out of date haskell repository, packages are broken and ghc 7 isn't present. The haskell platform is present as a system package, but is not up to date. <strong>If you want to install yesod painlessly, get ghc >= 7. See the topic below</strong>

The happy and alex package is however provided and working. (apt-get them)

#### GHC on Ubuntu

Ghc is easily downloaded from http://www.haskell.org/ghc/. Installing ghc should be no big problem. (if you experience otherwise, update this wikipage!).

After you have an updated ghc, you should go back to the Haskell platform page and download and build the link under "Build from source". If that install doesn't work for you, or you still have other issues, continue reading this page.

#### Cabal on Ubuntu

<em>By now you've updated your ghc to >= 7, and couldn't build haskell-platform from source</em>

You can get a working cabal-install by installing cabal with apt-get. After that run `cabal update`. It should tell you that a newer version is available and you can get it by `cabal install cabal-install`. Do not run that! instead run `cabal install cabal-install --cabal-lib-version=1.x.y.z`. To see what xyz should be, run `ghc-pkg list Cabal`. That will print out your current latest Cabal Library version.

Run `cabal --version` to see your cabal is up to date <strong>and</strong> is using the newer Cabal Library. If cabal-install is not up to date, possible causes: 1. you don't have `~/.cabal/bin` is in your `$PATH`. That is `PATH=$PATH:~/.cabal/bin/` should be present in your bash config files (lies in `~/.bash_something`). This must also be done to use yesod or any other cabal-installed program. 2. you are calling the original cabal in /usr/bin To solve the problem, you can delete the original cabal or place the path '~/.cabal/bin' before '/usr/bin'

### Windows

Get the latest haskell platform. This will provide both ghc and cabal. If you don't have the Haskell platform of if you otherwise receive unix related errors, you may require the msys utility to install some Haskell packages.

## C packages that must be installed

Yesod uses databases packages and whatnot that do depend on C-libraries. Cabal can't sucessfully install your packages if the C-libraries are missing.

### Ubuntu

There are a lot of -dev packages in synaptic that you need to install. A very incomplete list of such packages follow. Please add packages you found necessary to this list aswell.

* libfcgi-dev
* libsqlite3-dev
* postgresql-server-dev-8.4 (Might be worth installing the latest version)

## Installing Yesod

See the five-minute tutorial.

Last updated September 7, 11 7:34 am

Yesod is BSD licensed. The content of this site is [Attribution-NonCommercial-ShareAlike 3.0 Unported](http://creativecommons.org/licenses/by-nc-sa/3.0/)<br />
Site design by [Chris Done](http://chrisdone.com/). For site issues, contact [Michael Snoyman](mailto:michael@snoyman.com).