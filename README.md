# README #

## What is this repository for?

Playing card library / engines written for the author's Haskell practice. Plan to generate strategy / probability tables.

## Setup instructions

```
stack build
stack install
```

The last step may or may not be necessary.

To analyze insurance probabilities for a few given counts of the knockout (KO) counting system, run e.g. (nominal turnaround point for 6 deck is at 22)
`stack exec -- insurance 0 20 22 24`

To generate dealer bust rates (with admitedly poor statistics), you can similarly run
`stack exec -- dealer-bust 0 10 20 30`.

