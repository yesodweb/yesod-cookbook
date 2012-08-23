If you are deploying your Yesod app on a remote webserver that is a different platform than your working machine (most of the time it is), it is necessary to build on a virtual machine that matches the webserver environment.

For example, if you deploy to Heroku you need to compile on a Linux 64-bit with Ubuntu 10.04.

This guide provides instructions in creating a virtual machine for Heroku deployments.

## Installation
1. Download [VirtualBox from Oracle](https://www.virtualbox.org/wiki/Downloads). VirtualBox is a general-purpose full virtualizer for x86 hardware. Targeted at server, desktop and embedded use, it is a professional-quality virtualization solution that is also open source software. VirtualBox runs on Windows, Mac OS X, Linux, and Solaris.
1. Download [Vagrant](http://downloads.vagrantup.com/). Vagrant has excellent [support documentation](http://vagrantup.com/v1/docs/getting-started/index.html) (but watch out for the `vagrant destroy` command, you'll have to redo everything).
1. Get the [https://bitbucket.org/puffnfresh/vagrant-haskell-heroku](vagrant-haskell-heroku box project on Bitbucket) (create an account to download it). This `lucid64` box contains automated download+install "recipes" for the Haskell Platform and Heroku Toolbelt.
    * Update the configuration file Vagrantfile so it contains `:ghc_version => '7.4.2'`.
    * Do `vagrant up` - this will download all required packages and takes a looooooooong time.
    * Do `vagrant ssh`. You are now in Linux Ubuntu in the home folder.
1. Once you are inside the box, install additional tools and packages:
    * Do `sudo apt-get update` to update the package list.
    * Do `sudo apt-get upgrade` to update installed packages.
    * `sudo apt-get install git-core`
    * If you prefer vim above vi: `sudo apt-get install vim`
    * Getting the right Postgresql is explained in a separate wiki page.
1. In The VirtualBox app you can set shared folders. I put the Yesod project in the shared folder, so I can access it from my working machine as well. The path from Linux is `/vagrant/<project name>/`.
1. Cabal and Yesod:
    * Do `cabal update` and inside the Yesod project folder `cabal install`.
    * Yesod is not automatically recognized. At the end of `~/.profile` write `PATH="$HOME/.cabal/bin:$PATH"`.
    * `yesod devel` will not yet work because Postgresql still needs to be installed and configured. That instruction page needs to be written.


## Viewing `yesod devel` in the browser
* To compile from Linux and see the result on my working machine, I use the network setting NAT (see VirtualBox > Settings > Network).
* Edit the file Vagrantfile in the shared folder and add the line: `config.vm.network :bridged`.
* Do `vagrant reload`. At "Available bridged network interfaces" choose either Ethernet or Wi-Fi.
* Then do `ifconfig` from Linux, at the first item (eth0) grab the IP at `inet addr`. 
* Run `yesod devel`, enter the IP in a browser and add the port `:3000`.


## Troubleshooting

Occasionally I get the error "SSH connection was refused!". See [http://vagrant.wikia.com/wiki/Usage](http://vagrant.wikia.com/wiki/Usage) for a solution.