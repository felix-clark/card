# README #

### What is this repository for? ###

Playing card library / engines written for the author's Haskell practice. Plan to generate strategy / probability tables.

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### Setup instructions ###

* stack build
* stack install (might be necessary)

To analyze insurance probabilities for given counts of the knockout (KO) counting system, run e.g.
   (nominal turnaround point for 6 deck is at 22)
stack exec -- insurance 0 20 22 24

To generate dealer bust rates (with admitedly poor statistics), do similarly
stack exec -- dealer-bust 0 10 20 30

