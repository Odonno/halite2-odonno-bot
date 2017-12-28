module Collisions

open System

// Position in 2D space
type Position = { 
    X: float; 
    Y: float;
}

// Circle in 2D space
type Circle = {
    Position: Position;
    Radius: float;
}

// Rectangle in 2D space
type Rectangle = {
    TopLeft: float;
    TopRight: float;
    BottomLeft: float;
    BottomRight: float;
}

// calculateDistanceTo returns a euclidean distance to the target
let calculateDistanceTo p1 p2 =
    let dX = p2.X - p1.X;
    let dY = p2.Y - p1.Y;
    sqrt (dX*dX + dY*dY);

// converts degrees to radians
let degreesToRadians (d: float) = (d / 180.0 * Math.PI);

// converts radians to degrees
let radiansToDegrees (r: float) = r / Math.PI * 180.0;

// calculateRadAngleTo returns an angle in radians to the target
let calculateRadAngleTo p1 p2 =
    let dX = p2.X - p1.X;
    let dY = p2.Y - p1.Y;
    atan2 dY dX

// calculateAngleTo returns an angle in degrees to the target
let calculateAngleTo p1 p2 =
    calculateRadAngleTo p1 p2 |> radiansToDegrees

// detects if two circles collides
let circlesCollide c1 c2 =
    let dX = c2.Position.X - c1.Position.X;
    let dY = c2.Position.Y - c1.Position.Y;
    let radiusSum = c1.Radius + c2.Radius
    (dX*dX + dY*dY) <= (radiusSum*radiusSum)

// detect if 2D object is in a circle
let isInCircle position circle =
    let dX = circle.Position.X - position.X;
    let dY = circle.Position.Y - position.Y;
    (dX*dX + dY*dY) <= (circle.Radius*circle.Radius)