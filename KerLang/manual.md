---
title : The KerLang Programming Language Manual
subtitle : an introduction to comment-oriented programming using the *Glorious KerLang Compiler*
author : Arthur Correnson, Anima Libera, Igor Martayan
---

# Introduction

Commenting the code is good practice,
but no one ever said that coding the comments is good practice.
In our programming language *KerLang*,
the compiler/interpreter does it for the programmer,
thus reducing the boiler plate code that merely parahases the comments.

# What is comment-oriented programming ?

COP (Comment-Oriented Programming) is a programming paradigm that relies on
the fact that programmers shouln't be trusted at the task of writing 
code matching the comments.
Instead of relying of first-class code,
the language relies on first-class comments that describes what to do
to some extent.

# How to use the Glorious KerLang Compiler

COP is all about a tight relationship between the programmer and its compiler. A COP programmer typically start a new project by writing tons of comment as a blueprint of a piece of software. Then, the compiler can be asked to generate code based on the provided comments.

If the comments are not precise enough, the compiler may get stuck and report the impossibility to synthesize a piece of code.

## Gklc basics

# Step by step examples