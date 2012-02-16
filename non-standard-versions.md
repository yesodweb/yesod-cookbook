Yesod is not a single monolithic package with a single version. There are dozens of packages that play into Yesod. Some are very standard parts of the Haskell ecosystem, like bytestring and text. Others are very Yesod-specific, like yesod-core and yesod-auth. To tie it all together, the yesod package includes all of these underlying packages at a specific version number. This makes it simple for a user to get up and running with a tried-and-tested version of Yesod. However, there are two downsides:

1. This will automatically pull in some packages you may not need, such as yesod-auth.
2. You may end up using older versions of libraries.

The solution to both problems is simple: don't use the yesod package. It might seem surprising, but you can use Yesod perfectly well without the yesod package itself. The only trick is that without the yesod package, you can't import the Yesod module. Instead, you must import a few different modules manually, such as Yesod.Core and Yesod.Forms.

It's probably easiest to just remove the yesod package from your cabal file and start building. If you want to see some prior art in this department, check out the [source code for this site](http://github.com/snoyberg/yesodwiki).