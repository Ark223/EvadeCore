# EvadeCore

## Introduction

Geometry library for evade utility and skillshot simulation.

It has been a long time since people began inventing extensive solutions for problems with pathfinding around dynamic and static obstacles in 2D and 3D environments, yet no one has distributed an optimal way of finding the best path for the game object to get outside of a dangerous area.

This is a potential solution for artificial intelligence capabilities that I have decided to provide to others in a core format as an easy-to-use library. The solution can be used on any sort of shooting games as well as MMORPG games like League of Legends or Dota, but it can have a purpose in different areas too.

The skillshot types that are supported are linear, circular, conic, polygonal and annular. Skillshots are divided into two disparate forms depending on the type of movement. The first one follows a specific path with a constant motion from a fixed starting point to its destination, it contains a circular hitbox or conic shape depending on the type. The second form has no position displacement, so the skillshot points at a certain location. This form applies to all skillshot types that have their own defined collision boxes. For conic and linear skillshots, both of the forms mentioned above are applicable, and the user simply needs to select one.

The following documentation will help you understand how the library is supposed to work in your environment. The introduced API will mostly focus on a skillshot structure and core functions. More details about vectors, polygons and paths can be found inside library file.

## Skillshot API

Properties:

* **id** [number] - unique serial number id of an instantiated skillshot
* **arcStep** [number] - step length needed to construct an arc in a vertex offsetting
    + offsetting process is needed to visualise a true hitbox area of a skillshot
* **coneAngle** [radians] - angle formed by two rays of a cone structure
* **dangerLevel** [number] - state of a vulnerability of a game object against skillshot
* **extraDuration** [number] - extra amount of life time for a static skillshot
* **height** [number] - vertical component for drawing in a 3D perspective
* **offset** [number] - amount of a displacement of the starting position
* **preDelay** [number] - this property provides two disparate functions:
    + when a skillshot speed is huge, it determines a time left for skillshot to expire
    + otherwise it defines a delay before a skillshot begins a travel over a set distance
* **rotAngle** [radians] - angle of rotation of the ending position around starting position
* **radius** [number] - size of the skillshot hitbox (from center to circumference)
* **range** [number] - distance from the starting position to the ending position
* **sideRange** [number] - side range of the skillshot rotated perpendicularly ('rotate90')
* **speed** [number] - scalar quantity that refers to how fast a skillshot is moving
    + an immobile skillshot has a huge speed and covers the area in amount of life time
* **startTime** [number] - a runtime (in seconds) when a skillshot has been created
* **crowdControl** [boolean] - indicates if a skillshot impacts a victim's movement ability
* **fixedRange** [boolean] - indicates if a skillshot range is constantly fixed
* **fogOfWar** [boolean] - indicates if a skillshot was created in a fog of war
* **hitbox** [boolean] - considers a hitbox of a game object to the skillshot area
* **invert** [boolean] - indicates if a skillshot should be inverted
* **processed** [boolean] - indicates if a skillshot has been processed
* **rotate90** [boolean] - custom perpendicular rotation of a skillshot
    + besides being rotated, the middle point becomes an original end position
* **caster** [game_object] - game object who has casted a skillshot
* **casterName** [string] - original name of a skillshot caster
* **name** [string] - original name of instantiated skillshot
* **slot** [string] - determines a hotkey that is used to summon a skillshot
* **collisions** [table(CollisionFlag)] - collection of collision flags for skillshot
* **offsetPolygon** [Polygon] - skillshot area offsetted by hitbox of a game object
* **polygon** [Polygon] - skillshot area constructed from input properties
    + both polygons are only used by linear and polygonal skillshots
* **geometry** [Geometry] - instance of the geometry class providing useful methods
* **destPos** [Vector] - the raw and unprocessed destination point of a skillshot
* **direction** [Vector] - vector defining in which way the skillshot is pointing
* **endPos** [Vector] - ending position of a skillshot on a travel path
    + this is a fixed position whose the ancestor is 'destPos' property
* **perpendicular** [Vector] - skillshot direction rotated perpendicularly
* **position** [Vector] - current skillshot position (manually updated)
* **startPos** [Vector] - starting position of a skillshot on a travel path
* **detectionType** [DetectionType] - method that is used to detect a skillshot
* **skillshotType** [SkillshotType] - sets a skillshot type

Methods:

* **Draw(number color1, number color2)** [void] - draws a skillshot hitbox area
* **FixOrigin()** [void] - executes a fix for skillshot origin placements
* **IsDangerous(Vector)** [boolean] - indicates if input point is inside of skillshot area
* **IsDangerousPath(Path)** [boolean] - indicates if input path will collide with a skillshot
* **IsExpired()** [boolean] - indicates if a skillshot has already expired
* **IsSafe(Vector)** [boolean] - indicates if input point is outside of skillshot area
* **LoadData(table)** [void] - overrides skillshot properties with input table values
* **PathIntersection(Vector, Vector)** [Linq(Vector)] - returns skillshot-path intersection points
* **Position(number)** [Vector] - returns a predicted skillshot position after input time delta
* **TimeToHit(Vector)** [number] - returns a time for skillshot to enter a collision with object
* **TotalLifeTime()** [number] - returns a total skillshot life time
* **Update()** [void] - updates the skillshot hitbox area

## Core API

Properties:

* **skillshots** [Linq(Skillshot)] - a reference to the collection of active skillshot instances
* **angleStep** [degrees] - an angle step used in path generation around game object

Methods:

* **IsDangerous(Vector)** [boolean] - indicates if input point is inside of any skillshot area
* **IsDangerousPath(Path)** [boolean] - indicates if input path will collide with any skillshot
* **IsSafe(Vector)** [boolean] - indicates if input point is outside of all skillshot areas
* **FindSafeSpots(Path path, number maxRange, boolean fixedRange, number buffer)** [table(Vector)]
    - returns safe destinations for game object within a limited range (default value is 500)
    - 'fixedRange' value is used in case of the preference of paths generated at fixed range
    - 'buffer' input limits the outputs in case they were too close to other skillshot areas
* **GetEvadeSpot(Path path, number maxRange, boolean fixedRange, number buffer)** [Vector]
    - based on generated spots it prioritises the ones which are closer to mouse position
    - in case the paths were too close to reach a collision, it takes the shortest path
* **TimeToHit(Vector)** [number] - returns a time for skillshots to enter a collision with object

## Demo

```lua
local lib = require "EvadeCore"
local Linq, Vector, Path, Line, Core = lib.Linq,
    lib.Vector, lib.Path, lib.Line, lib.Core

local safeSpots = {}
local skillshots = Linq() -- initialise collection
local core = Core:New(skillshots) -- inject collection to core

client:set_event_callback("on_wnd_proc", function(msg, wparam)
    -- mouse left-click detection
    if msg ~= 514 or wparam ~= 0 then return end
    local dest = Vector:New(myHero.origin)
    local start = Vector:New(2700, 1800)
    local timer = game.game_time
    table.insert(skillshots, Line:New({
        arcStep = 10, extraDuration = 0,
        height = 100, preDelay = 0.25,
        radius = 70, range = 1000,
        speed = 1800, fixedRange = true,
        hitbox = true, startTime = timer,
        destPos = dest, startPos = start
    }))
end)

client:set_event_callback("on_tick", function()
    local pos = Vector:New(myHero.origin)
    if core:IsDangerous(pos) then
        local speed = myHero.move_speed
        local latency = game.ping / 2000
        local path = Path:New(speed, latency, 0.1)
        safeSpots = core:FindSafeSpots(path, 600)
    end
    skillshots:RemoveWhere(function(s) return s:IsExpired() end)
    skillshots:ForEach(function(s) s:Update() end)
end)

client:set_event_callback("on_draw", function()
    local hitbox = myHero.bounding_radius
    for _, point in ipairs(safeSpots) do
        renderer:draw_circle(point.x, 100,
            point.y, hitbox, 255, 192, 255, 255)
    end
    local c1, c2 = 0xFFFFFFFF, 0xC0FFFFFF
    skillshots:ForEach(function(s) s:Draw(c1, c2) end)
end)
```
