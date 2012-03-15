# Deploying to Heroku
# Introduction

This is not meant to be a tutorial about [git][2], [heroku][3], or
[Yesod][4], if you don't feel comfortable with the tools while following along
you may want to find their respective documentation.

This tutorial also assumes that you already have Yesod, git, and [the heroku
command line tool][5] installed.  The heroku tool is
probably in your package manager, if not there are instructions for installation
on the linked site.

Also, you *must* be working on 64 bit Linux with a version of libc
compatible with the Heroku hosts.  See the [GLIBC version errors](#addendum)
section for more details.

[2]: http://git-scm.com/
[3]: http://www.heroku.com/
[4]: http://www.yesodweb.com/
[5]: http://devcenter.heroku.com/categories/command-line

# Create your Yesod App
Execute `yesod init` to create your Yesod application, select PostgreSQL as the
database back end.  Next follow the instructions in the Procfile in `deploy/Procfile`
to modify your application to work on Heroku.

# Create your Heroku App

To host a Yesod app on Heroku create an application on the Cedar
stack and add the free shared PostgreSQL database.

    heroku apps:create --stack cedar [NAME]
    heroku addons:add shared-database:5mb

Change the Production approot URL in `config/settings.yml` to match the
URL of your Heroku application.

# Build your App for Heroku

Once you're ready to push the application to Heroku build the binary and
add it to the git repository. Next push your branch to the heroku remote.
(The *heroku* remote should have been added by the `heroku apps:create` step.)
See [Advanced git Usage](#advanced) for an alternative git workflow that avoids leaving
binaries in your history.

    cabal clean && cabal configure && cabal build
    git add dist/build/appname/appname
    git push -f deploy:master

# Resolve any outstanding library issues.

You'll now have a Yesod app on Heroku but it will most likely not work
due to missing shared libraries.  Even an empty scaffold site requires
a library (libgmp) that is not on Heroku by default.  If you view the logs
for your heroku app you'll likely see something like this:

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
This means that when the application was compiled it referenced libgmp
which can not be found on the Heroku host.  To resolve this create a libs directory
under in the project, copy the local shared libraries there, and then push them to Heroku.

First find out if there are any other missing libraries.  Run the command
`heroku run ldd dist/build/appname/appname`.  ldd will display a list of all
the shared libraries a binary depends on along with the path to that library.

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

Note the line that ends in `=> not found`.  Any libraries that are missing on
the Heroku host will show up as lines like this.
For your application to work on Heroku you'll need to make a copy
of your local libraries available to the Heroku host.  Do this
by creating a libs directory on the deploy branch and adding your
local libraries there.

    mkdir libs
    $ heroku config:add LD_LIBRARY_PATH=./libs
    Adding config vars and restarting app... done, v5
    LD_LIBRARY_PATH => ./libs

The command `heroku config:add ...` sets the `LD_LIBRARY_PATH` environment
variable to `./libs`.  This tells the dynamic linker on the Heroku host to
look in the libs directory when searching for shared libraries.
Next find where the missing libraries on your local host.  To do this use the same command
(ldd) locally that you used on Heroku previously.

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

Note where the missing `libgmp` is located (`/usr/lib/libgmp.so.10` on my machine)
and copy it into your libs directory.

    cp /usr/lib/libgmp.so.10 ./libs/

Repeat this step for any other missing libraries then commit and push your libraries
to the heroku remote.  You should now have a working Yesod app on Heroku.

-----

# <a name="addendum"></a>GLIBC version errors

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
found is to build on a distro that has an older version of libc.
I use a 64bit Ubuntu virtual machine
as a build host and simply deploy from there.  If you want to compare libc
versions you can run the following commands.

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

As I'm writing this Heroku is running Ubuntu with libc version 2.11.1 while my
local machine running ArchLinux has libc version 2.15

[1]: http://stackoverflow.com/a/8658468/166732

# <a name="advanced"></a>Advanced git Usage

Committing the application binary can be unwieldy because your repository
can quickly become very large.  This wastes disk space and will cause
push, fetch, and clone commands to take much longer.  The alternative
git workflow looks like this:

    # ready to push the current branch to heroku
    git checkout -b deploy
    cabal clean && cabal configure && cabal build
    git add dist/build/appname/appname
    git commit -m "binary"
    git push -f heroku deploy:master
    git checkout <original branch>

The critical thing to note is the creation of a new branch before commiting
the binary.  I do this every time I push to heroku, after the branch is pushed
I delete it so it can be garbage collected by git (and more importantly, will
never make its way out of my local repository).  This requires the use of
`push -f` because the branch on heroku will never be an ancestor of the commit
being deployed to heroku.

After pushing the deploy branch checkout the original branch and keep working on your
application.



