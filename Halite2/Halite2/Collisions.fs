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

// detects if two circles collides
let circlesCollideWithExpandedRadius c1 c2 expandedRadius =
    let dX = c2.Position.X - c1.Position.X;
    let dY = c2.Position.Y - c1.Position.Y;
    let radiusSum = c1.Radius + c2.Radius + expandedRadius
    (dX*dX + dY*dY) <= (radiusSum*radiusSum)

// detect if 2D object is in a circle
let isInCircle position circle =
    let dX = circle.Position.X - position.X;
    let dY = circle.Position.Y - position.Y;
    (dX*dX + dY*dY) <= (circle.Radius*circle.Radius)

// get tangent of the circle based on a point
let circleTangentsFromPoint position circle =
    let dX = circle.Position.X - position.X;
    let dY = circle.Position.Y - position.Y;

    let squareDistance = dX*dX + dY*dY
    let squareRadius = circle.Radius*circle.Radius

    let a = asin (squareRadius / squareDistance)
    let b = atan2 dY dX

    let left = 
        { 
            X = circle.Radius * sin (b - a);
            Y = circle.Radius * -1.0 * cos (b - a);  
        }

    let right = 
        { 
            X = circle.Radius * -1.0 * sin (b + a);
            Y = circle.Radius * cos (b + a);  
        }

    [| left; right; |]

// create a circle using two points
let createCircleFromPoints p1 p2 =
    let dX = abs (p2.X - p1.X);
    let dY = abs (p2.Y - p1.Y);

    {
        Position = 
            {
                X = (min p1.X p2.X) + (dX / 2.0);
                Y = (min p1.Y p2.Y) + (dY / 2.0);
            };
        Radius = (sqrt (dX*dX + dY*dY)) / 2.0;
    }