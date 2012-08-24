We assume you have followed the instructions in [[Setting up a virtual machine, using VirtualBox and Vagrant|Setting-up-a-virtual-machine,-using-VirtualBox-and-Vagrant]]. If you use a different system, you will probably have a newer version of PostgreSQL.

We refer to the working machine as "host" and the Ubuntu client inside the virtual Vagrant box as "client".

1. Install PostgreSQL: `sudo apt-get install postgresql`. This will install version 8.4.
1. Create a user account for the postgres deamon: `sudo adduser postgres` and enter a new password for this user.
1. Initialize postgres to use UTF8 instead of Latin1. Do this inside the new postgres user session (note the hyphen after `su`):<pre>
    sudo su - postgres
    rm -rf /var/lib/postgresql/8.4/main
    /usr/lib/postgresql/8.4/bin/initdb -D /var/lib/postgresql/8.4/main -E 'UTF-8' --lc-collate='en_US.UTF-8' --lc-ctype='en_US.UTF-8'</pre>
1. Inside a postgres user session (`sudo su - postgres`) you can control the database server:<pre>
    /usr/lib/postgresql/8.4/bin/pg_ctl -D /var/lib/postgresql/8.4/main -l logfile start
    /usr/lib/postgresql/8.4/bin/pg_ctl -D /var/lib/postgresql/8.4/main stop
    /usr/lib/postgresql/8.4/bin/pg_ctl -D /var/lib/postgresql/8.4/main status</pre>
1. Start the server. Create a database user and a new database with the same names as in your `config/postgresql.yml` file (or modify this file later to make it correspond again):<pre>
    psql template1
    CREATE USER &lt;name&gt; WITH PASSWORD '&lt;pwd&gt;';
    CREATE DATABASE &lt;dbname&gt;;
    GRANT ALL PRIVILEGES ON DATABASE &lt;dbname> TO &lt;name&gt;;
    \q</pre>
1. In another window ssh to the client. Go to the Yesod project folder (likely in a shared folder, like `/vagrant/appname/`.
1. Install pgconfig (part of libpq): `sudo apt-get install libpq-dev`.
1. Automatically install the Haskell lib for PostgreSQL using `cabal clean & cabal install`.
1. Test run Yesod: `yesod devel`. If all is well, you'll see something like:<pre>
    Devel application launched: http://localhost:3000
    Migrating: CREATE TABLE "user"</pre>