# Keylo - Keyboard Layout Optimizer

Given a set of constraints, desired properties, etc., Keylo generates the optimal keyboard layout for you.
In other words, you can make your own layout in the spirit of Dvorak that is customized for your needs.

## How it Works

Keylo does not assume that your keyboard hardware is a standard ANSI/ISO keyboard with 101~105 keys.
Based on what physical keys each finger of your hand is responsible for (hash data structure), Keylo takes into account penalties and letter frequency to find the optimal layout.
The "letter" here means the printed symbols on top of keycaps for the existing known keyboard layouts.
For example, QWERTY has letters "q", "w", "e", "r", but BÉPO layout has "é", "ê", "à", "è" as letters.
This definition of "letter" is on purpose --- we could just as easily define an entirely new set of "letters" for a brand new layout, but this is a level of customization that occurs at the software level (operating system); Keylo was designed to simpy answer the question: "if I could rearrange my keycaps the way I wanted to, what would be the best way?"

`Physical_Key` is a compound type that stores a unique name, what row and column it belongs to.
If you are designing a new layout and want to reuse existing hardware (i.e., going from a standard 104-ANSI keyboard to the same 104-ANSI keyboard), you should probably name the physical keys as they exist already on your QWERTY keyboard for ease of reference.
You could just as well name the keys with arabic numerals (the idea is that each physical key must have a unique name), but for us humans it's easier to use letters and words.

For the QWERTY layout on 104-key ANSI, we could have the following pseudocode hash data structure for the left pinky finger:

	(Key_Name 'q' Row 1 Column 0, Left Pinky)
	(Key_Name 'a' Row 0 Column 0, Left Pinky)
	(Key_Name 'z' Row -1 Column 0, Left Pinky)
	(Key_Name 'TAB' Row 1, Column -1 , Left Pinky)
	(Key_Name 'CAPS_LOCK' Row 0, Column -1 , Left Pinky)
	...

.

Each finger has its own 'universe' of rows and columns.
E.g., in QWERTY layout on 104-key ANSI, the left pinky finger's Column 0 has `q`, `a`, and `z` keys, while Column 1 has `TAB`, `CAPS_LOCK`, and `LEFT_SHIFT`.
On the other hand, keys `w`, `s`, and `x` have Column 0 for the left ring finger.

Keylo also takes a penalty list for each physical key.
By default, Keylo penalizes keys away from Row 0 (the "home row") or Column 0.
You can define a penalty list for the rows like this (increasing penalty): `[Row 0, Row 1, Row -1, Row 2]`, which will make Keylo consider Row 1 superior over Row -1.
For columns, it is just like rows --- you can define positive and negative columns, and then it's up to you to define a penalty list, such as `[Column 0, Column 1, Column -1, Column 2]`, etc., although most known keyboard layouts out in the world use perhaps 2 columns max per finger (common ones are pinky finger for regular and modifier keys).
We can also add a custom penalty for each physical key, like this:

	(Key_Name 'z', Penalty 2)
	(Key_Name 'q', Penalty 1)
	(Key_Name 'TAB, Penalty -3)
	(Key_Name 'ENTER', Penalty -4)

. This will make the 'z' physical key less desirable than the 'q' physical key when Keylo begins its algorithm to assign symbols to physical keys.
On the other hand, the 'TAB' and 'ENTER' physical keys have penalty bonuses because on a standard 104-key keyboard, these keys are larger than letter keys and are easy to press.

Upon invocation, Keylo takes an input corpus (a list of files) and generates optimal mappings of physical keys to symbols.
It takes into account individual letter frequencies, bigram frequencies, and word frequencies.
The algorithm is as follows:
	- Sort letters by individual letter frequency (for English, this creates the typesetter's phrase "ETAOIN SRHLDCU" [1])
	- Boost each letter's score depending on how often that letter appears for word frequency
	- Assign the most important letters to the physical keys with the lowest penalty, but avoid cases where typing in a common bigram (sequence of two letters) must be done by the same finger. You can assign a score for this part (how important it is to you), where 100 is maximum importance, and 0 means no importance (ignore this 'one finger bigram' consideration entirely).

You can define 'fixed' keys by refusing to define certain keys --- i.e., by limiting the available universe of keys that Keylo will consider.
E.g., if you leave out the left bracket `[` key, then Keylo will not be able to optimize this key, leaving it up to you to decide where it should go.
Ultimately it is up to you to interpret what `Row 0` and `Column 0` mean for each finger, so Keylo leaves that up to you to define as well.

[1]: http://norvig.com/mayzner.html
