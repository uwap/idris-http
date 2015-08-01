# idris-http
An HTTP library for idris 0.9.18.1.

## How to install

Clone the repository and install it using the idris package manager.

```
git clone https://github.com/uwap/idris-http.git
cd idris-http
idris --install http.ipkg
```

## To test (maybe)

```
Idris> :l src/Http.i
*src/Http> :let a = MkURIAuth Nothing Nothing "da.gd" 80
*src/Http> :let b = MkURI "http" a "/ip" "" ""
*src/Http> :let c = MkRequest GET b []
*src/Http> :x sendRequest c
```
