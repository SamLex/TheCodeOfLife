The Code Of Life
By Euan Hunter (s1322315)
https://github.com/SamLex/TheCodeOfLife

Renders a 3D interactive model of DNA (a double helix) in a recursive manner in which the bonds of the DNA are made up of more double helices and so on. 
Be Warned! This program can be very tasking on your PC
Entry for the Inf1–FP Programming Competition 2013

Usage:
Compile the code using ghc --make Main (or using Cabel) and then run the executable that it generates. Not all features may work on Windows and OS X
Can also be run with a digit between 1 and 9 as it's argument. This sets the recursion level to that number and disables interactivity. This is so that recursion 
levels too high for interactive mode can still be seen. Please be patient as these can take a long time to render

Keyboard Controls:

	Q,W,E,A,S & D - these keys rotate the model
	Arrows - these keys move the model up, down, left and right
 	Plus and Minus - these keys zoom the model in and out
 	* and / - these keys increase and decrease the recursion level. BE WARNED! Setting this to high WILL cause the program to stop responding to keyboard input (default 2, recommended max 4)
 	1 - changes the mode to line mode (default)
 	2 - changes the mode to sphere mode
 	0 - secret mode
 	F11 - toggles fullscreen mode
