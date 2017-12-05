#!/usr/bin/env bash

stack exec use-cloudhaskell-exe worker localhost 8000 &
stack exec use-cloudhaskell-exe worker localhost 8001 &
stack exec use-cloudhaskell-exe worker localhost 8002 &
stack exec use-cloudhaskell-exe worker localhost 8003 &
stack exec use-cloudhaskell-exe manager localhost 8004 $1
