module Constants

// Games will not have more players than this
let MAX_PLAYERS = 4

// Max number of units of distance a ship can travel in a turn
let MAX_SPEED = 7

// Radius of a ship
let SHIP_RADIUS = 0.5

// Starting health of ship, also its max
let MAX_SHIP_HEALTH = 255

// Starting health of ship, also its max
let BASE_SHIP_HEALTH = 255

// Weapon cooldown period
let WEAPON_COOLDOWN = 1

// Weapon damage radius
let WEAPON_RADIUS = 5.0

// Weapon damage
let WEAPON_DAMAGE = 64

// Radius in which explosions affect other entities
let EXPLOSION_RADIUS = 10.0

// Distance from the edge of the planet at which ships can try to dock
let DOCK_RADIUS = 4.0

// Number of turns it takes to dock a ship
let DOCK_TURNS = 5

// Number of production units per turn contributed by each docked ship
let BASE_PRODUCTIVITY = 6

// Distance from the planets edge at which new ships are created
let SPAWN_RADIUS = 2.0