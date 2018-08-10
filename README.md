[![Available on Hackage][badge-hackage]][hackage]
[![License MIT][badge-license]][license]
[![Build Status][badge-travis]][travis]
# socket-unix
A Unix domain socket API for the [socket](https://github.com/lpeterse/haskell-socket) library.

## Usage
Creating the Unix domain socket:
```haskell
import System.Socket
import System.Socket.Protocol.Default
import System.Socket.Type.Stream
import System.Socket.Family.Unix

s <- socket :: IO (Socket Unix Stream Default)
```

Creating the address for binding/connecting
```haskell
address <- case socketAddressUnixPath "example.sock" of
             Just addr -> pure addr
             Nothing -> fail "invalid pathname for socket"
```
### Symlinks
Binding to a socket with a filename creates a socket in the filesystem, but does not unlink it after `close` called. You should handle deleting links yourself.
## Portability
Linux and OS X are supported.


[badge-travis]: https://img.shields.io/travis/VyacheslavHashov/haskell-socket-unix.svg
[travis]: https://travis-ci.org/VyacheslavHashov/haskell-socket-unix
[badge-hackage]: https://img.shields.io/hackage/v/socket-unix.svg?dummy
[hackage]: https://hackage.haskell.org/package/socket-unix
[badge-license]: https://img.shields.io/badge/license-MIT-blue.svg?dummy
[license]: https://github.com/vyacheslavhashov/haskell-socket-unix/blob/master/LICENSE
