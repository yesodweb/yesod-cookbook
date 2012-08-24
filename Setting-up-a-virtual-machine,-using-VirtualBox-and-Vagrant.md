If you are deploying your Yesod app on a remote webserver that is a different platform than your working machine (most of the time it is), it is necessary to build on a virtual machine that matches the webserver environment.

For deploying to Heroku for instance, you need to compile on a machine with Ubuntu 10.04 Lucid Lynx (64 bit). As of March 2012 Heroku uses Linux servers with glibc version 2.11.1, same as Ubuntu 10.04. And because Heroku uses 64 bit architecture, the Virtual Machine has to be 64 bit too.

This guide provides step by step instructions in creating a virtual machine for Heroku deployments.

## Installation
1. Download [VirtualBox from Oracle](https://www.virtualbox.org/wiki/Downloads). VirtualBox is a general-purpose full virtualizer for x86 hardware. Targeted at server, desktop and embedded use, it is a professional-quality virtualization solution that is also open source software. VirtualBox runs on Windows, Mac OS X, Linux, and Solaris.
1. Download [Vagrant](http://downloads.vagrantup.com/). Vagrant uses Oracle’s VirtualBox to build configurable, lightweight, and portable virtual machines dynamically. Vagrant has excellent [support documentation](http://vagrantup.com/v1/docs/getting-started/index.html) (but watch out for the `vagrant destroy` command, you'll have to redo everything).
1. Get the [vagrant-haskell-heroku box project on Bitbucket](https://bitbucket.org/puffnfresh/vagrant-haskell-heroku) (create an account to download it). This `lucid64` box contains automated download+install "recipes" for the Haskell Platform and Heroku Toolbelt.
    * Do `vagrant up` - this will download all required packages and takes a looooooooong time.
    * Do `vagrant ssh`. You are now in Linux Ubuntu in the home folder.
1. Once you are inside the box (from now on we call it "guest"; your work machine is "host"), install additional tools and packages:
    * Do `sudo apt-get update` to update the package list.
    * Do `sudo apt-get upgrade` to update installed packages.
    * `sudo apt-get install git-core`
    * If you prefer vim above vi: `sudo apt-get install vim`
1. In The VirtualBox app you can set shared folders. I put the Yesod project in the shared folder, so I can access it from my host machine as well. The path from guest is `/vagrant/<project name>/`.
1. Cabal and Yesod:
    * Do `cabal update` and inside the Yesod project folder (in guest) `cabal install`.
    * Yesod is not automatically recognized. At the end of `~/.profile` write `PATH="$HOME/.cabal/bin:$PATH"`.
    * `yesod devel` will not yet work because PostgreSQL still needs to be installed and configured. See: [[Setting up PostgreSQL|Setting-up-Postgresql]].


## Viewing your Yesod site in the host browser
The easiest way to view your Yesod site from guest on your host machine is to use port forwarding.

1. Shut down your guest if it is running with `vagrant halt`.
1. Let's say you want to use port 4567. In Vagrantfile, add the setting: `config.vm.forward_port 3000, 4567` (to forward guest port 3000 to host port 4567).
1. In `config/settings.yml` change `approot: "http://localhost:3000"` to `approot: "http://localhost:4567"`.
1. Restart guest with `vagrant up` and do `vagrant ssh`.
1. Start PostgreSQL and Yesod again.
1. On your host machine open a new browser window and visit http://localhost:4567.

There are also other ways, for example using NAT. [[Converting a VirtualBox guest from a client (NAT) to a server (Host Interface Networking)|https://blogs.oracle.com/fatbloke/entry/converting_a_virtualbox_nat_client]] might be a starting point (I haven't tried it).


## Troubleshooting

Occasionally I get the error "SSH connection was refused!". See [http://vagrant.wikia.com/wiki/Usage](http://vagrant.wikia.com/wiki/Usage) for a solution.