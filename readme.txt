================================================================================

  Conway's Game of Life - Commodore C64 v0.2
  (c) Paul Alan Freshney 2022-2023

  paul@freshney.org

  Source code and ROM
    https://github.com/MaximumOctopus/GoLC64
  
  Assembled using 64tass
    https://sourceforge.net/projects/tass64/
  
================================================================================

Assemble: 64tass gol.asm -o gol.prg

Cell animation runs at 10Hz.

  Joystick 1:
    fire, set a cell "live" (when draw mode active)
	up/left/right/down, move the cursor around the screen

  Joystick 2:
    fire, toggles mode
      draw mode: pauses game of life animation. Cells can now be drawn using joystick 1.
      gol mode : starts game of life animation.
    up, clears the screen
    down, clears the screen with random data
   
Known issues:
Cells at the edges are not correctly processed... to do.

There are further optimisations to be done!

================================================================================

 Credits:

   All coding       : Paul A Freshney
   Development Cats : Rutherford, Freeman, and Maxwell
					  
   Dedicated to Julie, Adam, and Dyanne.

All of my software is free and open source; please consider donating to a local cat charity or shelter.

Thanks.

================================================================================

Release History

0.2 / February 6th 2023

First public release.

================================================================================