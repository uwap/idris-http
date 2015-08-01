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
