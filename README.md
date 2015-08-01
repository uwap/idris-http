# idris-http
An HTTP library for idris 0.9.18.1.

## How to install

Clone the repository and install it using the idris package manager.

```
git clone https://github.com/uwap/idris-http.git
cd idris-http
idris --install http.ipkg
```

## To test

Be in `src/` then run:

```
idris -p http -p contrib HttpTest.idr -o test && ./test
```
