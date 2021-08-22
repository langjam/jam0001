#!/usr/bin/env boros

{- "Prints the first 100 fibonacci numbers."; -}

let n = {- 0 -} in
let a = {- 1 -} in
let b = {- 2 -} in

if n == 100 then
  halt ();

comments.[1] <- n + 1;
comments.[2] <- comments.[3];
comments.[3] <- a + b;

a
