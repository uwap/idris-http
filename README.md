![Travis Status](https://travis-ci.org/uwap/idris-http.svg "Travis Build Status")

# idris-http
An HTTP library for idris 0.9.18.1.

## How to install

Clone the repository and install it using the idris package manager.

```
git clone https://github.com/uwap/idris-http.git
cd idris-http
idris --install http.ipkg
```

## Documentation

The documentation is available at [http://uwap.github.io/idris-http/](http://uwap.github.io/idris-http/).  
Note: Due to [Issue #2161](https://github.com/idris-lang/Idris-dev/issues/2161) the documentation isn't complete.

## Running/Testing an example

First install http by typing

```
idris --install http.ipkg
```

Then move into the directory of the example you want to test.
For example move into examples/simple/ and then build and run it:

```
cd examples/simple/
idris --build simple.ipkg
./simple
```

## Contributing

Feel free to fork and change idris-http. We are glad to see pull-requests and issue reports.
