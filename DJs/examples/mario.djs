/**
 * @var b C 5
 */
function run(b) {
    console.log("I'm running at full speed toward Peach");
    console.log("She's still " + b + " worlds away");
}

/**
 * @var b E 5
 */
function jump(b) {
    console.log("*jump*");
    b++;
}

/**
 * @var a E 5 
 */
function start(a) {
    console.log("Hey It's me");
    for (let i = 0; i < 3; i++) {
        console.log("Ma-"); // E5
        a;
    }
    console.log("riiiiiiio");
    run(1); // C5
    jump(1) // E5
}

/**
 * @var pieces E 4
 */
function takePieces(pieces) {
    console.log("Oh some pieces here, there are : " + pieces + " let's grab them !")
}

/**
 * @var meters C 5
 */
function goIntoPipe(meters) {
    console.log("This pipe is around " + (meters + 2) + " meters high, let's explore it");
}

/**
 * @var casualties A 4
 */
function killGoomba(casualties) {
    console.log("BAAAAM, " + (casualties + 1) + " less goomba !");
}

/**
 * @var brick B 4
 */
function breakBrick(brick) {
    console.log("Outch, head is hurting after breaking those " + brick + 2 + " bricks");
}

/**
 * @var plant A# 4
 */
function burnPlant(plant) {
    console.log("Another plant down, counter till now: " + plant);
}

/**
 * @var life G 4
 */
function liveLeft(life) {
    console.log("I have " +  (life + 1) + " life left")
}

/**
 * @var world G 5
 */
function announceWorld(world) {
    console.log("It's the world number " + (world + 1));
}

/**
 * @var seconds F 5
 */
function getStar(seconds) {
    console.log("Let's go ! I'm invincible for " + (seconds + 10) + " seconds");
}

/**
 * @var flower A 5
 */
function eatFlower(flower) {
    console.log("NOMNOMNOM, Fiiiii" + "i".repeat(flower) + "re");
}

/**
 * @var plant D 5
 */
function killPlant(plant) {
    console.log(plant + " plants down, I don't like them anyway");
}


function goThroughWorld(world, life) {
    let burnedPlant = 1;
    announceWorld(2) // G5
    liveLeft(2); // G4
    goIntoPipe(1); // C5
    liveLeft(0); // G4
    takePieces(1); // E4
    killGoomba(0); // A4
    breakBrick(0); // B4
    burnPlant(burnedPlant); // A# 4
    killGoomba(1); // A4
    liveLeft(1); // G4
    jump(1); // E5
    announceWorld(1); // G5
    eatFlower(1); // A5
    getStar(1); // F5
    announceWorld(1); // G5
    jump(0); // E5
    run(0); // C5
    killPlant(0); // D5
    breakBrick(0); // B4
    run(1); // C5
    liveLeft(0); // G4
    takePieces(1); // E4
    killGoomba(0); // A4
    breakBrick(0); // B4
    burnPlant(0); // A# 4
    killGoomba(1); // A4
    liveLeft(1); // G4
    jump(1); // E5
    announceWorld(1); // G5
    eatFlower(1); // A5
    getStar(0); // F5
    announceWorld(0); // G5
    jump(0); // E5
    run(0); // C5
    killPlant(0); // D5
    breakBrick(0); // B4
}

start(1);

goThroughWorld(2, 1);