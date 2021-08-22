
/**
 * @var a re 3 re 2
 * @var b mi 3 mi 2
 * @waveType sine
 */
function sum(a, b) {
  return a + b
}

/**
 * @var a fa 3 fa 2
 * @var b sol 3 sol 2
 * @var c siB 3 siB 2
 * @var d la 3 la 2
 */
function x(a, b, alternate, c, d) {
  if (alternate) {
    a = b + c + d
  } else {
    a = b + d
  }
}


/**
 * @var a do 4 do 3
 * @var b siB 3 siB 2
 * @var c la 3 la 2
 * @var d silence
 */
function series(a, b, c, d) {
  if (a < 4) {
    console.log(b)
    c = d + 1
  }
  return false
}

/**
 * @var a la 3 la 2 la 1
 * @var b siB 3 siB 2 siB1
 * @var c do 4 do 2 do 1
 */
 function sub(a, b, c) {
  let d = a - b
  console.log(c)
  return d
}

/**
 * @var a re 4 re 2 re 1
 * @var b siB 3 siB 2 siB 1
 * @var c la 3 la 2 la 1
 * @var d sol 3 sol 2 sol 1
 * @var e la 3 la 2 la 1
 */
function thisIsGettingLong(a, b, c, d, e) {
  console.log(a, b, c, d, e)
}


/**
 * @var a siB 3 sol 2 sol 3
 * @var b do 4 do 3 re 3
 * @var c la 3 re 2 fa 3
 * @var d fa 3 re 3
 * @var e sol 3 re 2 mi 3
 * @var f siB 3 reB mi 3
 * @var g la 3 la 2
 * @var h fa 3 re 2 re 3
 * @var i re 3 re 2
 * @var j mi 3 do 2
 * @var k do 3 do 2
 * @var l la 3 do 2
 * @var m mi 3 la 3
 * @var n re 3 la 2 re 2
 */
function theWayOfTheDjs(a, b, c, d, e, f, g, h, i, j, k, l, m, n) {
  a = b + c + d + e + f + g + h + i + j + k + l + m + n
  console.log("A pirate's life for me")
}

res = sum(3, 2)
x(2, 1, false, 0, 2)
series(0, 0, 2, 1)

sub(2, 1, 2)
thisIsGettingLong(0, 0, 2, 1, 3)

theWayOfTheDjs(2, 1, 2, 1, 2, 0, 0, 2, 1, 2, 1, 2, 1, 3)