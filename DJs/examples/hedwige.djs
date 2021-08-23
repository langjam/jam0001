/**
 * @var a mi 3
 * @var b sol 3
 * @waveType pl
 */
function sum(a, b) {
  return a + b
}

/**
 * @var prev si 3
 * @var next do 4
 * @waveType pl
 */
function lastTwo(next, prev) {
  return next + prev;
}

/**
 * @var a solB 3
 * @var b mi 3
 * @var c si 3
 * @waveType pl
 */
function div(a, b, c) {
  return a / (b + c);
}

/**
 * @var freq1 la 3
 * @var freq2 solB 3
 * @var isAvailable silence 3
 * @waveType pl
 */
function quad(freq1, freq2, isAvailable) {
  freq1 = (isAvailable ? freq2: isAvailable);
}

/**
 * @var freq1 re 4
 * @var freq2 reB 4
 * @var reverb1 do 4
 * @var reverb2 laB 3
 * @waveType pl
 */
function otherQuad(freq1, freq2, reverb1, reverb2) {
  freq1 = (freq2 + reverb1) / reverb2;
}

/**
 * @var toMinor solB 3
 * @var toMajor sol 3
 * @var tmp2 siB 3
 * @var tmp mi 3
 * @waveType pl
 */
function majorToMinor(toMinor, toMajor, tmp, isFirst) {
  if (isFirst === 0) {
    tmp = (toMajor + toMinor) % 2 + 1;
  } else {
    var tmp2 = (toMinor - toMajor) + 1;
    tmp = 5;
  }
}



/**
 * @var args si 2
 * @var freq2 solB 3
 * @var isAvailable silence 3
 * @var will miB 3
 * @var diff fa 3
 * @waveType pl
 */
function main(diff) {
  let isFirst = 0;
  while (isFirst <= 1) {
    args = sum(2, 1) % 3;
    div(1, 2, 1)
    if (isFirst == 0) {
      quad(2, 2, 1);
      majorToMinor(1, 1, 1, isFirst);
      var will = diff + (args += 1) - 1;
      var isAvailable = 1;
    } else {
      otherQuad(2, 1, 2, 1);
      lastTwo(2, 1);
      majorToMinor(1, 1, 5, isFirst);
    }
    isFirst++;
  }
}

main(1);
