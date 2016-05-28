# stack-offline

[![Join the chat at https://gitter.im/cblp/stack-offline](https://badges.gitter.im/cblp/stack-offline.svg)](https://gitter.im/cblp/stack-offline?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Build [stack](http://docs.haskellstack.org) projects offline

## Design

### Utility `stack-offline`

Packs everything you need to develop without internet connection.
Packing itself does use internet connection.

Parameters:
- `--resolver`
  - (mandatory) use specified snapshot
- `--minimal`
  - pack only build tools, without testing, profiling and other tools
- `--system-ghc`
  - do not pack GHC (default)
- `--no-system-ghc`
  - do pack GHC

Output:
- big `tgz` archive (from hundreds of megabytes in minimal case to several gigabytes and beyond) with packages and compiler.
- a script extracting everything into proper positions in user's `~/.stack`.

## Development

### Testing

    $ stack test --pedantic
