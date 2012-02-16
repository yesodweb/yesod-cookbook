Compass <http://compass-style.org> is very helpful for navigating the maze of browser incompatibility and verbosity of CSS.  We have it integrated into our yesod build process.  Note that this guide is not the cleanest in the world.  For compass usage, refer to <http://compass-style.org>

To do this, we modify `Setup.hs` a bit:

    #!/usr/bin/env runhaskell
    module Main where
    import Distribution.Simple
    import Distribution.Simple.LocalBuildInfo
    import Distribution.Simple.Setup
    import Distribution.PackageDescription
    import System.Process (runCommand,  waitForProcess )
    
    main :: IO ()
    main = defaultMainWithHooks $ simpleUserHooks 
             { preBuild = genCompass }
    
    runPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
    runPostConf _ _ _ _ = return () 
    
    genCompass :: Args -> BuildFlags -> IO HookedBuildInfo
    genCompass _ _ = do
      putStrLn "Running Compass"
      waitForProcess =<< runCommand "compass compile compass"
      return emptyHookedBuildInfo


Set our `XXX.cabal` file to use a custom build process:

    build-type:        Custom

Then we create a `compass/config.rb` file:
  
    #Delineate the directory for our SASS/SCSS files (this directory)
    sass_path = "compass" #  File.dirname(.);
    
    print "sasspath = #{sass_path} \n"
    css_path = File.join(sass_path, "..", "static", "autogen")
    
    # Delinate the images directory
    images_dir = File.join("img")
     
    print "images dir: #{images_dir} \n"
    
    # Specify the output style/environment
    #output_style = :expanded #:compressed
    output_style = :compressed
    environment = :production
    
    $shouldTouch = true
    
    on_stylesheet_saved do |filename|
      print "saved "
      print filename
      print "\n"
      if $shouldTouch
        $shouldTouch = false
        staticFile = "S4M/Server/Static.hs"
        `rm dist/build/S4M/Server/Static.o`
        `touch #{staticFile}`
        print "touched #{staticFile}\n"
      end
    end


*note that the last bit which should force yesod devel to recompile affected files doesn't seem to run right*

Now any `.scss` or `.sass` files you have in `compass/...` will be compiled into your static directory so you can refer to them using static routes.
