*For general instructions about deploying a Yesod app via Keter, please see the Keter [README][]*

[README]: https://github.com/snoyberg/keter/blob/master/README.md

Early versions of Keter relied on Nginx as the reverse proxy. It wrote a server configuration block to `/etc/nginx/sites-enabled/keter` and handled reloading the server when changes were needed.

This fit well in systems where Nginx was already running to serve other apps but was a difficult and brittle code path to maintain and required Keter users to install and setup yet-another-program to use Keter. For these reasons, newer versions of Keter handle the reverse proxying itself.

This poses a problem in systems where we have Nginx running already and for things other than Keter and our Yesod app. Since both Keter and Nginx will want to bind to port 80 to do their jobs, we have a conflict.

Here we outline two ways to get Nginx and Keter to run on the same system. The first should work in all scenarios but may have unforeseen downsides -- it's an experimental idea at this point. The second method is more straight-forward and feels like a better use of the system. The problem is it can only be used in specific scenarios.

## Double-Proxy

In this setup we let Nginx control port 80, we tell Keter to listen on a local port. We setup Nginx to proxy to the local Keter instance who will then proxy to the Yesod app.

### Setup Keter

*/etc/keter/yaml*

~~~ { .yaml }
root: ...
host: 127.0.0.1
port: 8000
~~~

### Setup the Yesod app

*config/keter.yaml*

~~~ { .yaml }
exec: ../dist/build/app/app
args:
  - production

# this will become APPROOT, if you have any absolute URLs, it 
# must be accurate
host: example.com

# however, the requests from nginx -> keter will not come to 
# that hostname, so we need to tell keter to listen on something 
# else as well
extra-hosts:
  - "127.0.0.1:8000"
~~~

## Deploy your app

Deploy your keter bundle and watch the logs to make sure there are no errors. You should now be able to see your site at http://127.0.0.1:8000.

## Setup Nginx

*/etc/nginx/nginx.conf*

~~~
http {
  # each alternative use of nginx should be listening to a different
  # domain and do whatever it does via a server block here or in 
  # sites-enabled
  server {
    listen: 80;
    server_name other-example.com;
    # ...
  }

  # we'll add one to tell nginx to proxy to keter. this too
  # can go in sites-enabled if you're including those here
  server {
    listen: 80;
    server_name example.com;
    proxy_pass: 127.0.0.1:8000;
    
    # some useful proxy settings
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
  }
}
~~~

Now, visiting http://example.com should hit Nginx, get forwarded to http://127.0.0.1:8000 which hits Keter and gets forwarded to (for example) http://127.0.0.1:4001.

## Multi-App

A simpler and better (IMO) method is to ditch Nginx all together. Arguably, there should only be one application on your system which handles proxying requests to internal apps.

Depending on your situation, it's quite possible (and maybe easy) to let Keter be that application.

Keter can deploy any app you like as long as you can provide two things:

1. A binary that uses the `PORT` environment variable to know where to listen
2. A `config/keter.yaml` that specifies how to run that binary

In my case, I was using Nginx to provide some simple file browsers. I was able to put together a small python script using `SimpleHTTPRequestHandler` to serve a directory of my choosing. `tar`ring that together with a `keter.yaml` that specified the directory location as an argument to the script and placing that in `keter/incoming` was all I needed to get keter deploying those apps.

At that point, I could stop using Nginx entirely.

Again, this may or may not be feasible in your situation. If it is though, you can go back to the "standard" Yesod+Keter deployment procedure and get any and all benefits thereof. It's also one less program to run and monitor and has half the network hops of Double-Proxy.