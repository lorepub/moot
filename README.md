# Moot

This is the home of Moot, a CFP and event management system. To learn more, please see the [slides for the launch announcement](https://github.com/bitemyapp/presentations/blob/master/moot/moot.pdf).

## Helping out

If you're interested in learning Haskell or would like to help, _please_ contact me! We're still looking for:

- Backend and frontend developers
- Designers
- UI/UX pros
- People willing to help with tutoring and teaching

I'll ([Chris](https://github.com/bitemyapp)) be doing the heavy lifting but I'd like this application to be an opportunity for people to kick around a real Haskell application. To that end, I'm offering tutoring to folks interested in helping out. Don't be afraid to help out in one capacity (e.g. UI/UX) while asking for tutoring in another.

The easiest way to contact me is to send a message to the email address [on my GitHub profile](https://github.com/bitemyapp).


## Database Setup

After installing Postgres, run:

```
createuser moot --password moot --superuser
createdb moot
createdb moot_test
```

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

### Installing dependencies

This should install the `yesod` binary for the development server and also install the dependencies for the gulp build:

```
make deps
```

### Starting the web server for work

Start the gulp watch with:

```
make frontend-watch
```

Start a development server with:

```
make backend-watch
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag moot:library-only --flag moot:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
