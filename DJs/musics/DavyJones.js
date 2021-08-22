const execSync = require('child_process').execSync;
// import { execSync } from 'child_process';  // replace ^ if using ES modules
const { playNote, notes, types } = require("../src/sound")


playNote("sox", [notes.get("re")[3]], "minim", "pluck");
playNote("sox",[notes.get("mi")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("fa")[3]], "minim", "pluck");
playNote("sox",[notes.get("sol")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("la")[3]], "minim", "pluck");
playNote("sox",[notes.get("do")[4]], "quaver", "pluck");
playNote("sox",[notes.get("siB")[3]], "quaver", "pluck");
playNote("sox",[notes.get("la")[3]], "minim", "pluck");
playNote("sox",[notes.get("silence")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("la")[3]], "minim", "pluck");
playNote("sox",[notes.get("siB")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("do")[4]], "minim", "pluck");
playNote("sox",[notes.get("re")[4]], "quaver", "pluck");
playNote("sox",[notes.get("siB")[3]], "quaver", "pluck");
playNote("sox",[notes.get("la")[3]], "minim", "pluck");
playNote("sox",[notes.get("sol")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("la")[3]], "minimD", "pluck");

playNote("sox",[notes.get("siB")[3]], "minim", "pluck");
playNote("sox",[notes.get("do")[4]], "crotchet", "pluck");
playNote("sox",[notes.get("la")[3]], "minim", "pluck");
playNote("sox",[notes.get("fa")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("sol")[3]], "minim", "pluck");
playNote("sox",[notes.get("siB")[3]], "quaver", "pluck");
playNote("sox",[notes.get("la")[3]], "quaver", "pluck");
playNote("sox",[notes.get("fa")[3]], "minim", "pluck");
playNote("sox",[notes.get("re")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("mi")[3]], "minim", "pluck");
playNote("sox",[notes.get("do")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("la")[2]], "minim", "pluck");
playNote("sox",[notes.get("mi")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("re")[3]], "minim", "pluck");
playNote("sox",[notes.get("silence")[3]], "crotchet", "pluck");
playNote("sox",[notes.get("re")[3]], "minim", "pluck");
playNote("sox",[notes.get("silence")[3]], "crotchet", "pluck");
