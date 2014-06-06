# heroku-build

Interact with the Heroku Build API to start builds, check the status of 
builds, or release built slugs to other applications.

## Installation

*Note: not released yet...*

```
% cabal update
% cabal install heroku-build
```

## Prerequisites

You'll need to set the `HEROKU_API_KEY` environment variable. You can 
probably find a suitable value in your `~/.netrc`.

You should either `export` this value:

```
% export HEROKU_API_KEY="..."
```

Or prefix each call explicitly:

```
% HEROKU_API_KEY="..." heroku-build ...
```

## Usage

**start**

Initiate a build of version 1 (*v1*) of *my-app*'s sources on a 
*my-compile* Heroku instance. This call will return a *build id*, which 
you will need for subsequent calls.

```
% heroku-build --app my-compile start http://example.com/my-app.tar.gz v1
abcd-abcdabcdabcd-abcd
```

*NOTE*: The sources must be reachable by Heroku. For private GitHub 
repos, you'll need to include an access token in the URL. You can 
generate a Personal OAuth Token via your account settings page.

**status**

Check the progress of the build:

```
% heroku-build --app my-compile status abcd-abcdabcdabcd-abcd
pending
```

Eventually (hopefully), it will succeed:

```
% heroku-build --app my-compile status abcd-abcdabcdabcd-abcd
succeeded
```

**release**

Release the slug to your production application (called *my-prod* here):

```
% heroku-build --app my-compile release abcd-abcdabcdabcd-abcd my-prod
Success
```

## Automation

This process is meant to be scripted. Below is a simple example:

```sh
#!/bin/sh
set -e

HEROKU_API_KEY="..."

compile_app="..."
release_app="..."
sources="..."

status="pending"
version="$(git rev-parse --short HEAD)" # short sha
build_id="$(heroku-build --app "$compile_app" start "$sources" "$version")"

while [ "$status" = 'pending' ]; do
  printf '.'
  status="$(heroku-build --app "$compile_app" status "$build_id")"
done
printf "\n"

[ "$status" = 'succeeded' ] && \
  heroku-build --app "$compile_app" release "$build_id" "$release_app"
```
