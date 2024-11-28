# Sandworm
Atari 2600 Game

## [Credit]
 forked codebase from Robin Sergeant's Snake (https://github.com/RobinSergeant/2600-Snakes)

## [SANDWORM]
 Sandworm is an asymmetrical two-player game. Player 1 controls the sandworm and attempts to consume as many sandpeople as he can (similar to classic game, Snake). Player 2 controls the sandpeople, mastering the sandwalk while escaping to the corners of the desert. CPU will control movements for one of the players in 1P mode.
 
## [PLAY]
 Play using Stella emulator/core or via https://javatari.org

## [Objective]
 Player 1 wins when sandworm reaches 99 segments in length ('99' will display in top-right)
 Player 2 wins when 10 sandpeople successfully escape the sandworm ('A' will display in bottom-right)
 Player 1 loses when sandworm collides with boundary or itself

## [Controls]
### Select: Toggles Player 1 controlling sandworm (black background) or the sandpeople (dark-gray background)
### Reset: Resets game, including high score
### Player 1:
 - Joystick: Changes direction for the head of sandworm
 - Fire: Enters tunneling mode (also resets board after game over)
### Player 2:
 - Joystick: Moves sandperson
 - Fire: Aborts any queued movements commands, to stand still

## [Game tracking]
### Top left: High score (max length of sandworm)
### Top right: Current score (length of sandworm)
### Bottom left: Tunneling charges (# of times P1 can enter tunneling mode)
### Bottom right: Escapes (# of times sandpeople have escaped the sandworm)

## [Game mechanics]
### Eating: Sandworm eats a person when the head occupies same location as a sandperson. Sandworm will then grow in length and a new sandperson will spawn in a random location (Spawn will avoid current body of snake or a position within 3 movements of the corner).
### Escape: Sandpeople escape the sandworm by exiting at one of the four corners of the desert. Sandpeople initially move 4x slower than the sandworm but will gradually speed up as the number of escapes increase.
### Tunneling: Sandworm will occasionally enter tunneling mode (either by consuming a charge with FIRE button or by sandperson failing to sandwalk properly). While tunneling is active, sandpeople will move 15x slower than the sandworm and the screen will shake.
### Sandwalking: Sandpeople will trigger a free tunnel by the sandworm if they engage in frequent or repetitious movements.
### Awareness: In reverse 1P mode, the sandworm will gradually become more aware of the location of a sandperson, based on frequency of moves.
