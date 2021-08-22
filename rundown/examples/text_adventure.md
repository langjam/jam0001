# Precinct Escape

```rundown
fun restart() {
    goto "precinct-escape";
}
```

You've been sitting at an officer's desk in the precinct for hours at this point.
Thankfully, the office has a great view of the impound lot.
You can even see your own car out there; just waiting for this misunderstanding to be resolved so you can go home.

Suddenly, you hear a crash from behind you.
As if a table full of expensive equipment just fell over.

Your draw the officers attention.

"What are you doing over there?
I told you to remain SILENT!
Don't make me come over there."

1. Insist it wasn't you
1. Look out the window

```rundown
let response = read();
if (response == 1) {
    goto "insist-it-wasnt-you";
} else {
    goto "look-out-the-window";
}
```

## Insist it wasnt you

You speak up.

"It wasn't me.
That sound from outside"

The once noisy room falls into silence.
The officer shouts from acrossed the room.
"What did you say, criminal?"

"That it wasn't me.
That came from outside"

The officer leaves the watercooler in a huff and to your chair by his desk.

1. Put your head down and stare at the floor
1. Look out the window for the source of the sound

```rundown
let response = read();
if (response == 1) {
    goto "head-down";
} else {
    goto "look-out-the-window";
}
```

## Head down

The cop slams his hand down on his desk.

You startle, but keep your head down.

"That's more like it, criminal
Keep your head down, I don't want to hear another PEEP out of you.
Do you UNDERSTAND me?"

1. Say "Yes I do."
1. Nod silently

```rundown
let response = read();
if (response == 1) {
    goto "yes-i-do";
} else {
    goto "nod";
}
```

## Yes I Do

"I Just told you to stay silent!" he shouts.
"Come with me".

The officer takes roughly pulls you out of the chair, slaps on some cuffs, and hauls you towards one of the many interrogation rooms.
The officer shoves you into the seat, and hurredly locks your cuffs to the table.

"Now I don't want to hear another PEEP out of you while you are in this room.
I will come get you in a moment."

A few moments pass, and a great commotion breaks out.
Gunfire, shouts, screams.

In a few minutes it is over.
You call out for some assistance.
Nobody comes in the room.

After what feels like ages, the earth rends open and you and the police station fall deep into the new chasm.

1. Restart

```rundown
let response = read();
restart();
```

## Nod

"Good choice."

The officer turns and heads back to his conversation.
The Window behind you explodes inwards.

Shattered glass rains down on your cutting up your limbs with hundreds of tiny cuts.

The officer turns around sharply.

"What did you do now, criminal?!" the officer shouts.

1. Ask the officer for a first aid kit
1. Jump out the window

```rundown
let response = read();
if (response == 1) {
    goto "first-aid";
} else {
    goto "jump-out-the-window";

}
```

## First aid

"HA! Come with me we will fix you right up."

The officer leads you to an interrogation room, you are promptly chained to the desk.

The officer walks out laughing.

Your blood starts to pool on the desk.
You shout.
Everthing goes black.

A commotion stirs you awake.
Gunshots.
Black.

You wake up again.

The wall where the doorway used to be is gone.
The precinct is gone.

You shout for help until the bloodloss takes over.

1. Restart

```rundown
let response = read();
restart();
```

## Look out the window

You look out the window.
Outside you see absolute chaos.

The impound lot seems to be exploding.
Cars are being tossed in the air.
You see your own car, tossed up, and ripped in half.

Whatever is destorying those cars is headed this way.

1. Take cover under your desk
1. Watch the chaos unfold

```rundown
let response = read();
if (response == 1) {
    goto "take-cover";
} else {
    goto "watch-the-chaos-unfold";
}
```

## Take Cover

You hide under your desk while the precinct unaware of the danger.

The window above you shatters, and rains down onto your desk.

Your hands, which are still cuffed to the desk, sustain a few cuts.

The room errupts into chaos.

1. Jump out the window
1. Stay hidden

```rundown
let response = read();
if (response == 1) {
    goto "jump-out-the-window";
} else {
    goto "stay-hidden";
}
```

## Stay Hidden

You stay under the desk.
The Cops scramble, grabbing weapon and all their SWAT equipment thinking it is an invasion.
Not realizing the supernatural situation they have found themselves in.

You stay hunkered under the desk as something huge enters the building.

The cops start firing at something.
You hear the bullets richochet off something metal.

You feel something hot, and painful.
It all goes black.

1. Restart

```rundown
let response = read();
restart();
```

## Watch the chaos unfold

You stare, entranced by the destruction taking place outside.
You watch and watch.
A small crowd of officers look over your shoulder.

The destructive force heads straight for your window.

The window bursts apart, raining glass over you and the crowd.

You stagger, trying to move away before there is another attack.
Your legs give out, and you collapse amongst the glass and debris.

1. Restart

```rundown
let response = read();
restart();
```

## Jump Out The Window

You plant your arm down on the window still, and leap out the first-story window.
Pain reaches you.

You look down at your now bloodied hand, a peice of glass embedded from the jump.

A Police Cruiser is on its side in front of you, trunk ajar.
You can see the first aid kit, and a shotgun in the trunk.

1. Grab first aid
1. Grab shotgun

```rundown
let response = read();
if (response == 1) {
    goto "grab-first-aid";
} else {
    goto "grab-shotgun";
}
```

## Grab first aid

When you get to the car, you start looking for the first aid box.

You see it, but when you grab it, it's stuck fast.

You begin tugging on it, trying to break it free.

An officer starts shouting at you to stop, just as the first aid kit comes free.

You quickly spin from the trunk from the force of your tugs.

The officer fires.

1. Restart

```rundown
let response = read();
restart();
```

## Grab shotgun

The police car has a shotgun sitting on top.
You easily grab it and get away from the scene.

As you step off out of the parking lot, a hulking creature appears before you.
It shouts in your face.

1. Fire gun
1. Drop gun and run

```rundown
let response = read();
if (response == 1) {
    goto "fire-gun";
} else {
    goto "drop-gun";
}
```

## Fire Gun

You pull the trigger.

The beast reacts violently.

1. Restart

```rundown
let response = read();
restart();
```

## Drop Gun

You drop the gun and run as fast as possible.

Shots ring out, the cops are shooting the beast from the windows.
It crashes through the wall, leaving you an opportunity to escape on foot.

Bloodied, and bruised, you walk for hours out of the city.
Once in the woods you take a nap to recover.
Tomorrow will be better.

**The End**
