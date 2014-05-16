# Setup

    # Create a cabal sandbox
    $ cabal sandbox init

    # Install haskell base packages
    $ cabal install --only-dependencies

    # Build the site generator
    $ cabal build

    # Create a bundler environment
    $ bundle install --path=vendor/

    # Run Compass once (just to be sure)
    $ bundle exec compass compile

    # Build the site
    $ cabal run build

    # Watch the site
    $ cabal run watch

