module Halite

open System
open Constants
open Collisions

// #region Entities

// Unit in space
type SpaceUnit = int

// Radius
type Radius = float

// Health
type Health = int

// Velocity
type Velocity = float

// DockingStatus represents possible ship.DockingStatus values
type DockingStatus = 
    | Undocked
    | Docking
    | Docked
    | Undocking

// Entity captures spacial and ownership state for Planets and Ships
type Entity = { 
    Id: int;
    OwnerId: int;
    Position: Position;
    Radius: Radius;
    Health: Health;
}

// Ship is a player controlled Entity made for the purpose of doing combat and mining Halite
type Ship = {
    Entity: Entity;
    VelX: Velocity;
    VelY: Velocity;
    PlanetId: int;
    DockingStatus: DockingStatus;
    DockingProgress: SpaceUnit;
    WeaponCooldown: SpaceUnit;
}

// Planet object from which Halite is mined
type Planet = {
    Entity: Entity;
    NumDockingSpots: int;
    NumDockedShips: int;
    CurrentProduction: int;
    RemainingResources: int;
    DockedShipIds: int[];
    DockedShips: Ship[] option;
    Owned: int;
}

// Player of the game
type Player = {
    Id: int;
    Ships: Ship[];
}

// Current map state
type Map = {
    MyId: int;
    Width: int;
    Height: int;
    Planets: Planet[];
    Players: Player[];
    Entities: Entity[];
}

// Current connection (to communicate with the Halite server)
type Connection = {
    PlayerTag: int;
    Width: int;
    Height: int;
}

// #endregion

// #region Commands

// thrust generates a string describing the ship's intension to move during the current turn    
let thrust (ship: Ship) (magnitude: int) (angle: int) =
    let boundedAngle = ((angle % 360) + 360) % 360            

    sprintf "t %s %s %s" (ship.Entity.Id |> string) (magnitude |> string) (boundedAngle |> string)

// dock generates a string describing the ship's intension to dock during the current turn
let dock (ship: Ship) (planet: Planet) =
    sprintf "d %s %s" (ship.Entity.Id |> string) (planet.Entity.Id |> string)

// undock generates a string describing the ship's intension to undock during the current turn
let undock (ship: Ship) =
    sprintf "u %s %s" (ship.Entity.Id |> string)         

// canDock indicates that a ship is close enough to a given planet to dock
let canDock (ship: Ship) (planet: Planet) =
    let dist = calculateDistanceTo ship.Entity.Position planet.Entity.Position
    dist <= (ship.Entity.Radius + planet.Entity.Radius + DOCK_RADIUS)

// #endregion

// #region Networking 1/2

let sendString (input: string) = 
    Console.WriteLine input

let getString () =
    Console.ReadLine().Trim()

let getInt () =
    getString() |> int  

// newConnection initializes a new connection for one of the bots participating in a match
let newConnection botName = 
    let playerTag = getInt()
    let sizeInfo = getString().Split " "
    let width = sizeInfo.[0] |> int
    let height = sizeInfo.[1] |> int

    sendString botName

    {
        PlayerTag = playerTag;
        Width = width;
        Height = height;
    }

// #endregion

// #region Parsing

// intToDockingStatus converts an int to a DockingStatus
let intToDockingStatus i =
    match i with
        | 0 -> Undocked
        | 1 -> Docking
        | 2 -> Docked
        | 3 -> Undocking
        | _ -> Undocked

// parseShip from a slice of game state tokens
let parseShip playerId (tokens: string[]) =
    let shipId = tokens.[0] |> int
    let shipX = tokens.[1] |> float
    let shipY = tokens.[2] |> float
    let shipHealth = tokens.[3] |> int
    let shipVelX = tokens.[4] |> float
    let shipVelY = tokens.[5] |> float
    let shipDockingStatus = tokens.[6] |> int
    let shipPlanetId = tokens.[7] |> int
    let shipDockingProgress = tokens.[8] |> int
    let shipWeaponCooldown = tokens.[9] |> int

    let shipEntity = {
        Id = shipId;
        Position = { X = shipX; Y = shipY; };
        Radius = SHIP_RADIUS;
        Health = shipHealth;
        OwnerId = playerId;
    }

    let ship = {
        Entity = shipEntity;
        PlanetId = shipPlanetId;
        DockingStatus = intToDockingStatus shipDockingStatus;
        DockingProgress = shipDockingProgress;
        WeaponCooldown = shipWeaponCooldown;
        VelX = shipVelX;
        VelY = shipVelY;
    }

    (ship, tokens.[10..])

// parsePlanet from a slice of game state tokens
let parsePlanet (tokens: string[]) =
    let planetId = tokens.[0] |> int
    let planetX = tokens.[1] |> float
    let planetY = tokens.[2] |> float
    let planetHealth = tokens.[3] |> int
    let planetRadius = tokens.[4] |> float
    let planetNumDockingSpots = tokens.[5] |> int
    let planetCurrentProduction = tokens.[6] |> int
    let planetRemainingResources = tokens.[7] |> int
    let planetOwned = tokens.[8] |> int
    let planetOwnerId = tokens.[9] |> int
    let planetNumDockedShips = tokens.[10] |> int

    let planetEntity = {
        Id = planetId;
        Position = { X = planetX; Y = planetY; };
        Radius = planetRadius;
        Health = planetHealth;
        OwnerId = planetOwnerId;
    }

    let dockedShipIds = 
        Array.init planetNumDockedShips
            (fun i -> tokens.[11 + i] |> int)

    let planet = {
        Entity = planetEntity;
        NumDockingSpots = planetNumDockingSpots;
        NumDockedShips = planetNumDockedShips;
        CurrentProduction = planetCurrentProduction;
        RemainingResources = planetRemainingResources;
        DockedShipIds = dockedShipIds;
        DockedShips = Option.None;
        Owned = planetOwned;
    }

    (planet, tokens.[(11 + planetNumDockedShips)..])

// parsePlayer from a slice of game state tokens
let parsePlayer (tokens: string[]) =
    let playerId = tokens.[0] |> int
    let playerNumShips = tokens.[1] |> float

    let playerNumShipsInt = playerNumShips |> int

    let mutable remainingTokens = tokens.[2..]
    let ships = 
        Array.init playerNumShipsInt
            (
                fun _ -> 
                    let ship, tokensnew = parseShip playerId remainingTokens
                    remainingTokens <- tokensnew
                    ship
            )         

    let player = {
        Id = playerId;
        Ships = ships;
    }

    (player, remainingTokens)

// parseGameString from a slice of game state tokens
let parseGameString connection (gameString: string) =
    let mutable tokens = gameString.Split(" ")
    let numPlayers = tokens.[0] |> int
    tokens <- tokens.[1..]

    let players = 
        Array.init numPlayers
            (fun _ -> 
                let player, tokensnew = parsePlayer tokens
                tokens <- tokensnew
                player
            )

    let numPlanets = tokens.[0] |> int
    tokens <- tokens.[1..]

    let planets = 
        Array.init numPlanets
            (fun _ ->
                let planet, tokensnew = parsePlanet tokens
                tokens <- tokensnew
                planet
            )

    let entities = 
        let shipEntities = 
            players
            |> Array.map (fun p -> p.Ships)
            |> Array.reduce Array.append
            |> Array.map (fun s -> s.Entity)
        let planetEntities = 
            planets
            |> Array.map (fun p -> p.Entity)

        Array.append shipEntities planetEntities

    {
        MyId = connection.PlayerTag;
        Width = connection.Width;
        Height = connection.Height;
        Planets = planets;
        Players = players;
        Entities = entities;
    }

// #endregion

// #region Networking 2/2

// updateMap decodes the current turn's game state from a string
let updateMap connection =
    let gameString = getString()
    parseGameString connection gameString

// submitCommands encodes the player's commands into a string
let submitCommands (commandQueue: string[]) =
    let commandString = String.Join(" ", commandQueue)
    sendString commandString
    
// #endregion