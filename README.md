# ocelot-hs

Haskell bindings for the [ocelot](https://github.com/keharriso/ocelot) library.

Parse C headers to identify function prototypes, type declarations, and global variables.

## Building

```bash
$ git clone --recursive https://github.com/keharriso/ocelot-hs.git
$ stack build
$ stack test
```

If the build process can't find `llvm-config`, you will need to specify the following two environment variables:

* `OCELOT_LLVM_LIB_DIR`
* `OCELOT_LLVM_INCLUDE_DIR`

These tell `ocelot` where to find the libclang dependencies.

## License

Copyright (c) 2023 Kevin Harrison, released under the MIT License (see LICENSE for details).