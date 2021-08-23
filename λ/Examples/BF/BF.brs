#!/usr/bin/env boros

let iter f l =
  let end = length l in
  let go i =
    if i != end then
      (f l.[i]; go $ i + 1)
  in
  go 0
in

let wrap n =
  if n == 256 then
    0
  else if n == -1 then
    255
  else
    n
in

let state = {- { tape = [0], crr = 0 } -} in

let evalOp op =
  if op == "+" then
    state.tape.[state.crr] <- wrap $ state.tape.[state.crr] + 1
  else if op == "-" then
    state.tape.[state.crr] <- wrap $ state.tape.[state.crr] - 1
  else if op == "<" then
    if state.crr == 0 then
      state.tape <- [0] + state.tape
    else
      state.crr <- state.crr - 1
  else if op == ">" then
    (if state.crr == length state.tape - 1 then
      state.tape <- state.tape + [0];

    state.crr <- state.crr + 1)
  else if op == "," then
    state.tape.[state.crr] <- getChar ()
  else if op == "." then
    putChar state.tape.[state.crr]
  else
    if state.tape.[state.crr] then
      (iter evalOp op.ops; evalOp op)
in

let parse ops =
  let end = length ops in
  let go scope stack i =
    if i == end then
      if not stack then
        reverse scope
      else
        throw "Unmatched '['"
    else if contains (explode "+-<>,.") ops.[i] then
      go ([ops.[i]] + scope) stack (i + 1)
    else if ops.[i] == "[" then
      go [] ([scope] + stack) (i + 1)
    else if ops.[i] == "]" then
      if not stack then
        throw "Unmatched ']'"
      else
        let parent = pop stack in
        go ([{ ops = reverse scope }] + parent) stack (i + 1)
    else
      go scope stack (i + 1)
  in
  go [] [] 0
in

let ops = {- parse $ explode $ readFile args.[0] -} in
let opIdx = {- 0 -} in

if opIdx == length ops then
  halt ();

comments.[0] <- state;
comments.[1] <- show ops;
comments.[2] <- opIdx + 1;

evalOp ops.[opIdx]
