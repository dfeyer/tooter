# Tooter

This project is a web client for Mastodon, currently a side project to improve
my personal skills with Elm and explore a bit the fediverse.

*Warning*: This project if a work in progress, please try to avoid connection to
a public Mastodon instances. For the development, I use a `pleroma` local instance.
Pleroma is Mastodon alternative, build on the fantastic Elixir/Phoenix stack.

## Getting started

First your need to install the required dependencies, including Elm 0.19:

    npm install

I try to avoid as much as possible Webpack or other complex build chain. So for
this project back to the good old `Makefile`. Don't even search for some fancy
CSS, SASS or LESS support ... for the CSS, I use `elm-css`.

You can build the project with:

    make build

For your development setup, you can watch for file changes with:

    make watch

You need to have a nice little binary on your system [entr](http://www.entrproject.org/)

Before openning your browser, please run:

    make serve

Now you can open your browser at `http://localhost:5001`

## Resources

- We found intersting patterns and inspirations in the [Tooty](https://github.com/n1k0/tooty) project.
- We try to use the vocabulary of the official [Mastodon API](https://github.com/tootsuite/documentation/blob/master/Using-the-API/API.md#status)