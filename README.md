# Minimal Concrete Haskell library

## Simple installation

```bash
cabal install concrete-haskell
```

## Custom installation

This requires Thrift 0.9.3 and assumes you are in this repository.

### Generate source code

```bash
generate_source.sh PATH\_TO\_CONCRETE/thrift PATH\_TO\_CONCRETE\_SERVICES/thrift
```

### Compile and install

```bash
cabal install
```
