## Introduction
[[Heroku|http://www.heroku.com/]] is a cloud platform as a service (PAAS) for building, deploying, and running cloud apps using Ruby and other languages. Their business is in removing the hassle from deploying applications. See [[Heroku Isn't for Idiots|http://rdegges.com/heroku-isnt-for-idiots]] for a non-objective but nonetheless informative blog post.

Heroku doesn't run Haskell or Yesod but it runs binaries. For deploying your Yesod (or any Haskell) app to Heroku you will compile the executable and use git to move it on their servers. Once the setup is done, this becomes an easy process.

### 3-step guide

1. Set up a virtual machine to compile your local app. See the step by step guides on this wiki:
    * [[Setting up a virtual machine using VirtualBox and Vagrant|Setting-up-a-virtual-machine,-using-VirtualBox-and-Vagrant]] helps to create a machine that matches Heroku's servers
    * [[Setting up PostgreSQL|Setting-up-PostgreSQL]] because that is what Heroku uses
1. Set up Heroku
1. Deploy to Heroku

This guide assumes you have some working experience with Yesod and Git.

## Set up Heroku
### Set up your account
1. [[Create a free developer account on Heroku|https://api.heroku.com/signup]]
1. Install the Heroku command line tool. This is included in the virtual machine setup of step 1; otherwise do `wget -qO- https://toolbelt.heroku.com/install-ubuntu.sh | sh`
1. Login: `heroku login`. This will also read and otherwise create a public SSH key on your machine.

If you already have an account, and are working on a different virtual machine, you can pass your public SSH keys to Heroku: `heroku keys:add ~/.ssh/id_rsa.pub`. If this doesn't work (Permission denied error), let Heroku create a new key: `heroku keys:clear`, `rm ~/.ssh/id_rsa.pub`, `heroku keys:add`.

### Set up Git
If you've worked with Git this will be familiar. Otherwise read [[Git for beginners|http://ryanflorence.com/git-for-beginners/]] first.

1. Store your name and email in git config:<pre>
    git config --global user.name "Your Name"
    git config --global user.email you@example.com
</pre>
1. Go to your project folder and do:
<pre>
git init
git add .
git commit -m "init"
</pre>

The next steps will also create the remote Git repository.

### Set up your Heroku app
1. Go to your project folder, for instance `cd /vagrant/appname/`.
1. Create your project on the Heroku server: `heroku apps:create appname`.
1. Add the free "Dev Plan" postgresql add-on: `heroku addons:add heroku-postgresql`. More details at [[Getting Started with the Heroku Postgres Add-on|https://devcenter.heroku.com/articles/heroku-postgres-addon]].
1. Connect your Heroku app to your database, see [[the instructions on the "Getting Started" page|https://devcenter.heroku.com/articles/heroku-postgres-addon#retrieve-your-database-credentials-and-connect]].
1. Assign your database to the `DATABASE_URL` environment variable in your application: [[Promote your database|https://devcenter.heroku.com/articles/heroku-postgres-addon#promote-your-database-and-begin-using-it]].
   * If you created your database through Heroku's web interface, follow [[this help from Stackoverflow|http://stackoverflow.com/a/11803478/505157]].

### Make changes to your Yesod project

1. Go to your project folder.
1. In `config/settings.yml` change the `approot` setting in section Production to the URL of your Heroku application, for instance `approot: "http://appname.herokuapp.com"`.
1. Move `deploy/Procfile` to the root of the project folder: `mv deploy/Procfile ./`.
1. It needs to contain only this one line:<pre>
web: ./dist/build/appname/appname production -p $PORT
</pre>
1. Create a package.json file:<pre>
echo '{ "name": "appname", "version": "0.0.1", "dependencies": {} }' >> package.json
</pre>
1. Add the changes to git: `git add .` and `git commit -m "heroku setup"`.


## Deploy to Heroku

Instructions to push your app to Heroku without committing the large binary, **from your virtual machine**.

Repeat these steps to deploy:

    git checkout -b deploy
    cabal clean && cabal configure -fproduction && cabal build
    git add -f dist/build/appname/appname
    git commit -m "binary"
    git push -f heroku deploy:master
    git checkout <original branch>
    git branch -D deploy

The critical thing to note is the creation of a new branch before commiting the binary. I do this every time I push to Heroku, after the branch is pushed I delete it so it can be garbage collected by Git (and more importantly, will never make its way out of my local repository). This requires the use of `push -f` because the branch on Heroku will never be an ancestor of the commit being deployed to heroku.

After pushing the deploy branch checkout the original branch and keep working on your
application.

-----

## Random tips
### Resolve any outstanding library issues.

You'll now have a Yesod app on Heroku but dependent on your app it will not work yet due to missing shared libraries.  Even an empty scaffold site requires a library (libgmp) that is not on Heroku by default.  If you view the logs for your heroku app you'll likely see something like this:

    $ heroku logs
    2012-03-08T20:08:55+00:00 heroku[api]: Add-on add logging:basic by user@email.com
    2012-03-08T20:08:55+00:00 heroku[api]: Release v1 created by user@email.com
    2012-03-08T20:11:03+00:00 heroku[slugc]: Slug compilation started
    2012-03-08T20:11:26+00:00 heroku[api]: Config add PATH by user@email.com
    2012-03-08T20:11:26+00:00 heroku[api]: Release v2 created by user@email.com
    2012-03-08T20:11:26+00:00 heroku[api]: Release v3 created by user@email.com
    2012-03-08T20:11:26+00:00 heroku[api]: Deploy 5cfe526 by user@email.com
    2012-03-08T20:11:27+00:00 heroku[web.1]: State changed from created to starting
    2012-03-08T20:11:27+00:00 heroku[slugc]: Slug compilation finished
    2012-03-08T20:11:33+00:00 heroku[web.1]: Starting process with command `./dist/build/appname/appname production -p 11527`
    2012-03-08T20:11:34+00:00 app[web.1]: ./dist/build/appname/appname: error while loading shared libraries: libgmp.so.10: cannot open shared object file: No such file or directory
    2012-03-08T20:11:35+00:00 heroku[web.1]: Process exited with status 127
    2012-03-08T20:11:35+00:00 heroku[web.1]: State changed from starting to crashed
    2012-03-08T20:11:35+00:00 heroku[web.1]: State changed from crashed to created
    2012-03-08T20:11:35+00:00 heroku[web.1]: State changed from created to starting

Note the line labeld `app[web.1]` that says "error while loading shared libraries: libgmp.so.10".
This means that when the application was compiled it referenced libgmp which can not be found on the Heroku host. To resolve this create a libs directory under in the project, copy the local shared libraries there, and then push them to Heroku.

First find out if there are any other missing libraries. Run the command `heroku run ldd dist/build/appname/appname`. ldd will display a list of all the shared libraries a binary depends on along with the path to that library.

    $ heroku run ldd dist/build/appname/appname
    Running ldd dist/build/appname/appname attached to terminal... up, run.1
	linux-vdso.so.1 =>  (0x00007fff355b8000)
	libpcre.so.3 => /lib/libpcre.so.3 (0x00007f5484ead000)
	libpq.so.5 => /usr/lib/libpq.so.5 (0x00007f5484c85000)
	libz.so.1 => /lib/libz.so.1 (0x00007f5484a6d000)
	libpthread.so.0 => /lib/libpthread.so.0 (0x00007f5484850000)
	libgmp.so.10 => not found
	libm.so.6 => /lib/libm.so.6 (0x00007f54845cc000)
	librt.so.1 => /lib/librt.so.1 (0x00007f54843c4000)
	libdl.so.2 => /lib/libdl.so.2 (0x00007f54841c0000)
    .... SNIP ....

Note the line that ends in `=> not found`.  Any libraries that are missing on the Heroku host will show up as lines like this. For your application to work on Heroku you'll need to make a copy of your local libraries available to the Heroku host. Do this by creating a libs directory on the deploy branch and adding your local libraries there.

    mkdir libs
    $ heroku config:add LD_LIBRARY_PATH=./libs
    Adding config vars and restarting app... done, v5
    LD_LIBRARY_PATH => ./libs

The command `heroku config:add ...` sets the `LD_LIBRARY_PATH` environment variable to `./libs`.  This tells the dynamic linker on the Heroku host to look in the libs directory when searching for shared libraries. Next find where the missing libraries on your local host. To do this use the same command (ldd) locally that you used on Heroku previously.

    $ ldd dist/build/appname/appname
	linux-vdso.so.1 =>  (0x00007fff7ffff000)
	libpcre.so.3 => /lib/x86_64-linux-gnu/libpcre.so.3 (0x00007f4b9cba5000)
	libpq.so.5 => /usr/lib/libpq.so.5 (0x00007f4b9c979000)
	libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f4b9c760000)
	libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f4b9c543000)
	libgmp.so.10 => /usr/lib/libgmp.so.10 (0x00007f4b9c2cf000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f4b9c04a000)
	librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f4b9be42000)
    .... SNIP ....

Note where the missing `libgmp` is located (`/usr/lib/libgmp.so.10` on my machine) and copy it into your libs directory.

    cp /usr/lib/libgmp.so.10 ./libs/

Repeat this step for any other missing libraries then commit and push your libraries to the heroku remote.  You should now have a working Yesod app on Heroku.

-----

### <a name="addendum"></a>GLIBC version errors

If you use a rolling release Linux distribution like I do your version of libc
may be newer than the version on Heroku.  In that case you'll get errors that
look something like the following.

    2011-12-28T01:31:23+00:00 app[web.1]: ./dist/build/appname/
    appname: /lib/libc.so.6: version `GLIBC_2.15' not found
    (required by ./dist/build/appname/appname)
    2011-12-28T01:31:23+00:00 app[web.1]: ./dist/build/appname/
    appname: /lib/libc.so.6: version `GLIBC_2.15' not found
    (required by /app/dist/build/appname/libgmp.so.10)
    2011-12-28T01:31:25+00:00 heroku[web.1]: State changed from starting to crashed
    2011-12-28T01:31:25+00:00 heroku[web.1]: Process exited

There are apparently [several ways to fix this][1] but the simplest I've
found is to build on a distro that has an older version of libc, which is described in step 1 above.

If you want to compare libc versions you can run the following commands.

    $ heroku run --app appname 'ldd --version'
    Running ldd --version attached to terminal... up, run.1
    ldd (Ubuntu EGLIBC 2.11.1-0ubuntu7.8) 2.11.1
    Copyright (C) 2009 Free Software Foundation, Inc.
    This is free software; see the source for copying conditions.  There is NO
    warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    Written by Roland McGrath and Ulrich Drepper.
    $ ldd --version
    ldd (GNU libc) 2.15
    Copyright (C) 2011 Free Software Foundation, Inc.
    This is free software; see the source for copying conditions.  There is NO
    warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    Written by Roland McGrath and Ulrich Drepper.

As I'm writing this Heroku is running Ubuntu with libc version 2.11.1 while my local machine running ArchLinux has libc version 2.15

[1]: http://stackoverflow.com/a/8658468/166732

-----
### Other how-to resources
* [TFoo Heroku deployment](http://nbartlomiej.com/2012/03/29/deploying-tfoo-and-other-haskell-applications-to-heroku/)
* [Deploy from a Vagrant VM](http://brianmckenna.org/blog/haskell_on_heroku)
* Heroku also has [buildpacks](https://devcenter.heroku.com/articles/buildpacks) to let them compile the code on deploy. I actually found a [WIP haskell buildpack](https://github.com/mbbx6spp/cabal-heroku-buildpack). This would make deploy much more convenient for non-ubuntu hosts.