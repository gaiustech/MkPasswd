rm -f mkpasswd mkpasswd.tix 

ghc -fhpc -optl-static -optl-pthread -XPackageImports --make -o mkpasswd MkPasswd.hs -hide-package monads-fd
