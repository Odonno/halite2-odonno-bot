# halite2-odonno-bot

Bot written in F# for Halite2 contest

## Current version

* Basic features
  * Logging (new *.log files)
  * 2D geometry functions (angle, collision, etc..)
  * Communication with Halite server (receiving info from cli, sending commands to cli)
* Analyse planets (statistics)
  * Valid planets to mine
  * Closest planets
* Create groups of one ship
  * With a target (Planet or Ship)
  * With a mission (Mining, Defending, Attacking)
  * Priority order: Mining >> Defending >> Attacking
* Pathfinding
  * Try to move forward
  * Navigate to a destination (Planet or Ship)
  * Heatmap of circles
  * Avoid obstacles (left/right tangents)
  * Multi-dimensional heatmap (for each future turn)
  
## Planned versions

* Analyse players (statistics)
  * Ranking of player (simple analysis of stronger enemy)
* Redux-like pattern to get information of all previous turns
  * Analyse evolution of players (predict future enemy)
  * Analyze evolution of ships (predict future target / defense strategy)
  * Analyse weakness (of enemy or of my bot)
* Group strategy with multiple ship / Try various strategy
  * n mining group = (Mining, Planet, AffectedShips)
  * n defending group = (Defending, Planet, AffectedShips)
  * n attacking group = (Attacking, PlanetOrShip, AffectedShips)
  * Squad of 3 ships? (the minimum number of ships to be able to move, defend and attack)
  * Triangle strategy? Roman military strategy-like? (triangle, circle, square)
  * Extending/Reducing group (all ships vs. small squads)
