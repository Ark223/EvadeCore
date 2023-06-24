
--[[

    Author: Ark223
    Version: 1.1.9
    Date: 24 June 2023
    Copyright (c) 2023 Arkadiusz Kwiatkowski

    [License]
    Use, modification & distribution is subject to Boost Software License Ver 1.
    https://www.boost.org/LICENSE_1_0.txt

    [Geometry API]
    Vector - represents two numeric values:
    Vector:New(...) - initial call, returns instance
        overloads:
            () [null vector] => Vector(0, 0)
            (50, 25) [number, number] => Vector(50, 25)
            ({x = 50, y = 25}) [xy-table] => Vector(50, 25)
            ({x = 50, z = 25}) [xz-table] => Vector(50, 25)
            ({x = 50, y = 25, z = 10}) [xyz-table] => Vector(50, 10)
        properties:
            .x - the x value (floating-point number or integer)
            .y - the y value (floating-point number or integer)
        metamethods:
            -> Vector == Vector (vector equality comparison)
            -> Vector + Vector (sum of two vectors)
            -> Vector - Vector (difference of two vectors)
            -> Vector * number or number * Vector (scalar multiplication)
            -> Vector / number or number / Vector (scalar division)
        functions:
            :AngleBetween(Vector, Vector) - returns the angle formed from a point to both input points
            :Append(Vector, number dist) - appends the target vector through the direction by set distance
            :Clone() - returns a new vector object that is the exact copy of the current vector object
            :ClosestOnSegment(Vector, Vector) - calculates the closest point on a line segment to the point
            :CrossProduct(Vector) - calculates the cross product of two vectors (determinant of 2x2 matrix)
            :Distance(Vector) - calculates the euclidean distance between two vectors
            :DistanceSquared(Vector) - calculates the squared distance between two vectors
            :DotProduct(Vector) - calculates the dot product (sum of the products) of two vectors
            :Extend(Vector, number dist) - extends the current vector towards a target by a certain distance
            :IsZero() - indicates if vector is a zero vector, meaning that both of its components are zeros
            :LengthSquared(Vector) - calculates the squared length of the vector represented by current object
            :Length(Vector) - calculates the length (square root) of the vector represented by current object
            :LineIntersection(Vector, Vector, Vector) - calculates the intersection point of two linear objects
            :Normalize() - returns a normalized version of the current vector object (output range from -1 to 1)
            :Perpendicular() - returns a vector that is rotated 90° counterclockwise relative to current object
            :Perpendicular2() - returns a vector that is rotated 90° clockwise relative to the current object
            :Rotate(number phi, Vector) - rotates a vector object by an angle (in radians) around a pivot point
            :Round() - rounds the components of a vector to the nearest integer ("round to nearest" convention)
            :SegmentIntersection(Vector, Vector, Vector) - calculates the intersection point of two line segments

    Polygon - represents a closed polygonal chain described by line segments created from points
    Polygon:New(...) - initial call, returns instance
        overloads:
            () [empty polygon] => Polygon({points = Linq()})
            (Linq()) [empty polygon] => Polygon({points = Linq()})
            (Linq({Vector, Vector, ...})) => Polygon({points = Linq({...})})
        properties:
            .points - connected points to form a closed polygon
            .size - a finite number of points
        functions:
            :Add(Vector) - inserts a new point to polygon
            :Append(table) - inserts a collection of points to polygon
            :Area() - calculates an area of a polygon and returns it
            :Clear() - cleares all points making a polygon empty
            :Draw(number color, number height) - draws a polygon on screen with ARGB color
                + height - defines a height of drawn segments (used in 3D perspective)
            :Get(number index) - returns a specific point at input index
            :IsInside(Vector) - indicates if input point is inside of a polygon
            :IsOutside(Vector) - indicates if input point is outside of a polygon
            :Offset(number delta, number step) - offsets a polygon by the input delta
                + step - defines a length step in arc generation for vertex offsetting
            :Orientation() - returns a boolean based on the orientation of a polygon
            :PathIntersection(Vector, Vector) - returns a path-polygon intersection points
            :Remove(number index) - removes a point from the polygon at specific index
            :Reverse() - performs an in-place order reversal for points of a polygon
            :Set(number index, Vector) - overrides the polygon's point at specific index

    Path - represents a route for a travel from starting to ending point
    Path:New(speed, delay, delta, startPos, endPos) - initial call, returns instance
        properties:
            .speed - pathing movement speed (default value: huge number)
            .delay - delay before a game object starts pathing (default value: 0)
            .delta - error propagation to enhance collision time calculations (default value: 0)
            .startPos - starting position of a path (default value: null vector)
            .endPos - ending position of a path (default value: null vector)

--]]

--------------------------------------------------------
-- Customizable directives for platform functionality --

local atan2 = math.atan2 or math.atan
local load = loadstring or load
local myHero = game.local_player

local directives = {
    timer = function() return game.game_time end,
    mousePos = function() return game.mouse_pos end,
    position = function() return myHero.origin end,
    hitbox = function() return myHero.bounding_radius end,
    drawPolygon = function(points, height, a, r, g, b)
        local points, a = points:Select(function(p) return
            vec3.new(p.x, height or 0, p.y) end), a / 8
        renderer:draw_filled_polygon(points, r, g, b, a)
        renderer:draw_polygon(points, r, g, b, a * 8, 2)
    end,
    drawCircle = function(pos, radius, height, a, r, g, b)
        local p, a = vec3.new(pos.x, height or 0, pos.y), a / 8
        renderer:draw_circle_filled(p.x, p.y, p.z, radius, r, g, b, a)
        renderer:draw_circle_thickness(p.x, p.y, p.z, radius, 2, r, g, b, a * 8)
    end
}

------------------------------------------------
-- Class constructor with inheritance support --

local Class = function(...)
    local cls, bases = {}, {...}
    for _, base in ipairs(bases) do
        for param, value in pairs(base) do
            cls[param] = value
        end
    end
    cls.__index = cls
    function cls:New(...)
        local instance = setmetatable({}, cls)
        cls.__init(instance, ...)
        return instance
    end
    cls.__call = function(_, ...) return cls:New(...) end
    return setmetatable(cls, {__call = cls.__call})
end

--------------------------------------
-- Language INtegrated Query (LINQ) --

local function ParseFunc(func)
    if func == nil then return function(x) return x end end
    if type(func) == "function" then return func end
    local index = string.find(func, "=>")
    local arg = string.sub(func, 1, index - 1)
    local func = string.sub(func, index + 2, #func)
    return load(string.format("return function"
        .. " %s return %s end", arg, func))()
end

local function Linq(source)
    return setmetatable(source or {}, {__index = table})
end

function table.AddRange(source, collection)
    local index = #source
    for _, value in ipairs(collection) do
        index = index + 1
        source[index] = value
    end
end

function table.Aggregate(source, func, seed)
    local result = seed or 0
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        result = func(result, value, index)
    end
    return result
end

function table.All(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if not func(value, index) then
            return false
        end
    end
    return true
end

function table.Any(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if func(value, index) then
            return true
        end
    end
    return false
end

function table.Clear(source)
    for index = 1, #source do
        source[index] = nil
    end
end

function table.Concat(source, collection)
    local result, index = Linq(), 0
    for _, value in ipairs(source) do
        index = index + 1
        result[index] = value
    end
    for _, value in ipairs(collection) do
        index = index + 1
        result[index] = value
    end
    return result
end

function table.Contains(source, element)
    for _, value in ipairs(source) do 
        if value == element then
            return true
        end
    end
    return false
end

function table.Copy(source, index)
    local result, iteration = Linq(), 0
    for i = (index or 1), #source do
        iteration = iteration + 1
        result[iteration] = source[i]
    end
    return result
end

function table.Distinct(source)
    local result = Linq()
    local hash, index = {}, 0
    for _, value in ipairs(source) do
        if hash[value] == nil then
            index = index + 1
            result[index] = value
            hash[value] = true
        end
    end
    return result
end

function table.Except(first, second)
    return first:Where(function(value)
        return not second:Contains(value) end)
end

function table.First(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if func(value, index) then
            return value
        end
    end
    return nil
end

function table.ForEach(source, func)
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        func(value, index)
    end
end

function table.Intersect(first, second)
    return first:Where(function(value)
        return second:Contains(value) end)
end

function table.Max(source, func)
    local result = -math.huge
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        local num = func(value, index)
        if type(num) == "number" and num >
            result then result = num end
    end
    return result
end

function table.Min(source, func)
    local result = math.huge
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        local num = func(value, index)
        if type(num) == "number" and num <
            result then result = num end
    end
    return result
end

function table.OrderBy(source, comparer)
    local result = source:Copy()
    table.sort(result, comparer or
        function(a, b) return a > b end)
    return result
end

function table.RemoveWhere(source, func)
    local size = #source
    local func = ParseFunc(func)
    for index = size, 1, -1 do
        local value = source[index]
        if func(value, index) then
            source:remove(index)
        end
    end
    return size ~= #source
end

function table.Reverse(source)
    local result, iteration = Linq(), 0
    for index = #source, 1, -1 do
        iteration = iteration + 1
        result[iteration] = source[index]
    end
    return result
end

function table.Select(source, func)
    local result = Linq()
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        result[index] = func(value, index)
    end
    return result
end

function table.SelectMany(source, selector, collector)
    local result = Linq()
    local selector = ParseFunc(selector)
    local collector = ParseFunc(collector)
    for index, value in ipairs(source) do
        local position = #result
        local values = selector(value, index)
        for iteration, element in ipairs(values) do
            local index = position + iteration
            result[index] = collector(value, element)
        end
    end
    return result
end

function table.Union(source, collection)
    return source:Concat(collection):Distinct()
end

function table.Where(source, func)
    local result, iteration = Linq(), 0
    local func = ParseFunc(func)
    for index, value in ipairs(source) do
        if func(value, index) then
            iteration = iteration + 1
            result[iteration] = value
        end
    end
    return result
end

---------------
-- Utilities --

local function Argb2Hex(a, r, g, b)
    return tonumber(string.format(
        "0x%.2X%.2X%.2X%.2X", a, r, g, b))
end

local function Hex2Argb(value)
    local floor = math.floor
    local alpha = floor(value / 2 ^ 24) % 256
    local red = floor(value / 2 ^ 16) % 256
    local green = floor(value / 2 ^ 8) % 256
    local blue = value % 256
    return alpha, red, green, blue
end

---------------------------------------
-- Vector class (two numeric values) --

local Vector = Class()

local function IsVector(v)
    return v and v.x and type(v.x) == "number"
        and v.y and type(v.y) == "number"
end

local function IsZero(v)
    return math.abs(v) < 1e-6
end

function Vector:__init(x, y)
    if x and y then
        self.x, self.y = x, y
    elseif x and not y then
        self.x, self.y = x.x, x.z or x.y
    else
        self.x, self.y = 0, 0
    end
end

function Vector:__type()
    return "Vector"
end

function Vector:__eq(p)
    return IsZero(self.x - p.x) and IsZero(self.y - p.y)
end

function Vector:__add(p)
    return Vector:New(self.x + p.x, self.y + p.y)
end

function Vector:__sub(p)
    return Vector:New(self.x - p.x, self.y - p.y)
end

function Vector:__unm()
    return Vector:New(-self.x, -self.y)
end

function Vector.__mul(a, b)
    if type(a) == "number" and IsVector(b) then
        return Vector:New(b.x * a, b.y * a)
    elseif type(b) == "number" and IsVector(a) then
        return Vector:New(a.x * b, a.y * b)
    end
    error("Multiplication error!")
end

function Vector.__div(a, b)
    if type(a) == "number" and IsVector(b) then
        return Vector:New(a / b.x, a / b.y)
    elseif type(b) == "number" and IsVector(a) then
        return Vector:New(a.x / b, a.y / b)
    end
    error("Division error!")
end

function Vector:__tostring()
    return string.format("(%f, %f)", self.x, self.y)
end

function Vector:AngleBetween(p1, p2, normalize)
    local angle = atan2(p2.y - self.y, p2.x - self.x)
        - atan2(p1.y - self.y, p1.x - self.x)
    if angle < 0 then angle = angle + math.pi * 2 end
    local fix = normalize and angle - math.pi > 0
    return fix and math.pi * 2 - angle or angle
end

function Vector:Append(v, dist)
    if IsZero(dist) then return v:Clone() end
    return v + (v - self):Normalize() * dist
end

function Vector:Clone()
    return Vector:New(self.x, self.y)
end

function Vector:ClosestOnSegment(s1, s2)
    local ap, ab = self - s1, s2 - s1
    local t = ap:DotProduct(ab) / ab:LengthSquared()
    return t < 0 and s1 or t > 1 and s2 or s1 + ab * t
end

function Vector:CrossProduct(v)
    return self.x * v.y - self.y * v.x
end

function Vector:Distance(v)
    return math.sqrt(self:DistanceSquared(v))
end

function Vector:DistanceSquared(v)
    local dx = v.x - self.x
    local dy = v.y - self.y
    return dx * dx + dy * dy
end

function Vector:DistSqrToSegment(s1, s2)
    local v = self:ClosestOnSegment(s1, s2)
    return self:DistanceSquared(v)
end

function Vector:DotProduct(v)
    return self.x * v.x + self.y * v.y
end

function Vector:Extend(v, dist)
    if IsZero(dist) then return self:Clone() end
    return self + (v - self):Normalize() * dist
end

function Vector:IsZero()
    return IsZero(self.x) and IsZero(self.y)
end

function Vector:LengthSquared()
    return self.x * self.x + self.y * self.y
end

function Vector:Length()
    return math.sqrt(self:LengthSquared())
end

function Vector:LineIntersection(a2, b1, b2)
    local a, b = a2 - self, b2 - b1
    local axb = a:CrossProduct(b)
    if IsZero(axb) then return nil end
    local bsa = b1 - self
    local t = bsa:CrossProduct(b)
    return self + a * t / axb
end

function Vector:Normalize()
    local len = self:Length()
    if IsZero(len) then return Vector:New() end
    return Vector:New(self.x / len, self.y / len)
end

function Vector:Perpendicular()
    return Vector:New(-self.y, self.x)
end

function Vector:Perpendicular2()
    return Vector:New(self.y, -self.x)
end

function Vector:Rotate(phi, p)
    if IsZero(phi) then return self:Clone() end
    local c, s = math.cos(phi), math.sin(phi)
    local p = p or Vector:New()
    local d = self - p
    local x = c * d.x - s * d.y + p.x
    local y = s * d.x + c * d.y + p.y
    return Vector:New(x, y)
end

function Vector:Round()
    local x = math.floor(self.x + 0.5)
    local y = math.floor(self.y + 0.5)
    return Vector:New(x, y)
end

function Vector:SegmentIntersection(a2, b1, b2)
    local a, b = a2 - self, b2 - b1
    local axb = a:CrossProduct(b)
    if IsZero(axb) then return nil end
    local bsa = b1 - self
    local t1 = bsa:CrossProduct(b) / axb
    local t2 = bsa:CrossProduct(a) / axb
    return t1 >= 0 and t1 <= 1 and t2 >= 0
        and t2 <= 1 and self + a * t1 or nil
end

--------------------
-- Geometry class --

local Geometry = Class()

function Geometry:__init() end

function Geometry:Arc(p1, p2, phi, step)
    local angle, result = -phi * 0.5, {}
    local length = p1:Distance(p2) * phi
    if length == 0 then return {p1} end
    if step > length then step = length end
    local steps = math.floor(length / step)
    for i = 1, steps + 1 do
        local rotated = p2:Rotate(angle, p1)
        table.insert(result, rotated)
        angle = angle + phi / steps
    end
    return result
end

function Geometry:ArcSegmentIntersection(p1, p2, a1, a2, range, angle)
    return self:CircleSegmentIntersection(
        p1, p2, a1, range):Where(function(int)
        local phi = a1:AngleBetween(int, a2)
        phi = math.min(phi, math.pi * 2 - phi)
        return phi <= angle * 0.5 end)
end

function Geometry:CircleSegmentIntersection(p1, p2, circle, radius)
    local result = Linq()
    if radius <= 0 then return result end
    local dp, dc = p2 - p1, p1 - circle
    local a = dp:LengthSquared()
    local b = 2 * dc:DotProduct(dp)
    local c = dc:LengthSquared() - radius * radius
    local delta = b * b - 4 * a * c
    if delta < 0 then return result end
    local delta = math.sqrt(delta)
    local t1 = (-b - delta) / (2 * a)
    local t2 = (-b + delta) / (2 * a)
    if t1 >= 0 and t1 <= 1 then
        local pos = p1 + dp * t1
        table.insert(result, pos)
    end
    if t2 >= 0 and t2 <= 1 then
        local pos = p1 + dp * t2
        table.insert(result, pos)
    end
    return result
end

function Geometry:DrawCircle(pos, radius, color, height)
    local a, r, g, b = Hex2Argb(color)
    directives.drawCircle(pos, radius, height, a, r, g, b)
end

function Geometry:DynamicCollision(a1, a2, b1, b2, sa, sb, ra, rb)
    local dist = (ra + rb) * (ra + rb)
    local va = (a2 - a1):Normalize() * sa
    local vb = (b2 - b1):Normalize() * sb
    local dp, dv = a1 - b1, va - vb
    local a = dv:LengthSquared()
    local b = 2 * dp:DotProduct(dv)
    local c = dp:LengthSquared() - dist
    local delta = b * b - 4 * a * c
    if delta < 0 then return nil end
    local delta = math.sqrt(delta)
    local t1 = (-b - delta) / (2 * a)
    local t2 = (-b + delta) / (2 * a)
    return math.min(t1, t2)
end

function Geometry:Interception(p1, p2, vel, speed, offset)
    local multi, dir = offset * speed, p1 - p2
    local a = vel:LengthSquared() - speed * speed
    local b = 2 * (vel:DotProduct(dir) - multi)
    local c = dir:LengthSquared() - offset * offset
    local delta = b * b - 4 * a * c
    if delta < 0 then return nil end
    local delta = math.sqrt(delta)
    local t1 = (-b - delta) / (2 * a)
    local t2 = (-b + delta) / (2 * a)
    local max = math.max(t1, t2)
    local min = math.min(t1, t2)
    local time = min < 0 and max or min
    return math.max(0, time)
end

function Geometry:IsInsideCone(p1, p2, pos, range, angle)
    if range <= 0 then return false end
    local distance = p1:DistanceSquared(pos)
    local outside = distance > range * range
    if outside == true then return false end
    local phi = p1:AngleBetween(pos, p2)
    phi = math.min(phi, math.pi * 2 - phi)
    return phi <= angle * 0.5
end

-------------------
-- Polygon class --

local Polygon = Class()

function Polygon:__init(points)
    self.points = points or Linq()
    self.size = #self.points
end

function Polygon:Add(point)
    self.size = self.size + 1
    self.points[self.size] = point
end

function Polygon:Append(points)
    for _, point in ipairs(points) do
        self:Add(point)
    end
end

function Polygon:Area()
    local area = 0
    for index = 1, self.size do
        local next = index == self.size and 1 or (index + 1)
        local p1, p2 = self.points[index], self.points[next]
        area = area + (p2.x - p1.x) * (p2.y + p1.y)
    end
    return -area * 0.5
end

function Polygon:Clear()
    for index = 1, self.size do
        self.points[index] = nil
    end
    self.size = 0
end

function Polygon:Draw(color, height)
    local poly, a, r, g, b = self.points, Hex2Argb(color)
    poly = poly.Select ~= nil and poly or Linq(poly)
    directives.drawPolygon(poly, height, a, r, g, b)
end

function Polygon:Get(index)
    return self.points[index]
end

function Polygon:IsInside(point)
    -- Based on Jianqiang Hao's public paper:
    -- "Optimal Reliable Point-in-Polygon Test and
    -- Differential Coding Boolean Operations on Polygons"
    local result = 0
    for index = 1, self.size do
        local next = index == self.size and 1 or (index + 1)
        local a, b = self.points[index], self.points[next]
        local v1, v2 = a.y - point.y, b.y - point.y
        if not (v1 < 0 and v2 < 0 or v1 > 0 and v2 > 0) then
            local u1, u2 = a.x - point.x, b.x - point.x
            if v2 > 0 and v1 <= 0 then
                local crs = u1 * v2 - u2 * v1
                if crs > 0 then result = result + 1 end
                if crs == 0 then return true end
            elseif v1 > 0 and v2 <= 0 then
                local crs = u1 * v2 - u2 * v1
                if crs < 0 then result = result + 1 end
                if crs == 0 then return true end
            elseif v2 == 0 and v1 < 0 then
                local crs = u1 * v2 - u2 * v1
                if crs == 0 then return true end
            elseif v1 == 0 and v2 < 0 then
                local crs = u1 * v2 - u2 * v1
                if crs == 0 then return true end
            elseif v1 == 0 and v2 == 0 then
                if u2 <= 0 and u1 >= 0 then return true end
                if u1 <= 0 and u2 >= 0 then return true end
            end
        end
    end
    return result % 2 ~= 0
end

function Polygon:IsOutside(point)
    return not self:IsInside(point)
end

function Polygon:Offset(delta, step)
    local result = Linq()
    local poly = self:Orientation() and
        self.points or self.points:Reverse()
    for index = 1, self.size do
        local p1 = poly[index]
        local p2 = poly[(index - 2) % self.size + 1]
        local p3 = poly[(index - 3) % self.size + 1]
        local d1 = (p2 - p1):Normalize():Perpendicular() * delta
        local d2 = (p3 - p2):Normalize():Perpendicular() * delta
        local a, b, c, d = p1 + d1, p2 + d1, p2 + d2, p3 + d2
        local int = a:LineIntersection(b, c, d)
        if int == nil then goto continue end -- parallel
        local prev = #result -- previous size of result
        local phi = math.pi - p2:AngleBetween(p1, p3)
        if phi < 0 then table.insert(result, int) end
        if prev ~= #result then goto continue end
        local vertex = p2:Extend(int, delta)
        local arc = Geometry:Arc(p2, vertex, phi, step)
        result:AddRange(arc)
        ::continue::
    end
    return Polygon:New(result)
end

function Polygon:Orientation()
    return self:Area() >= 0
end

function Polygon:PathIntersection(p1, p2)
    local result = Linq()
    for index = 1, self.size do
        local next = index == self.size and 1 or (index + 1)
        local a, b = self.points[index], self.points[next]
        local int = p1:SegmentIntersection(p2, a, b)
        if int then table.insert(result, int) end
    end
    return result:Distinct()
end

function Polygon:Remove(index)
    self.size = self.size - 1
    table.remove(self.points, index)
end

function Polygon:Reverse()
    for index = 1, math.floor(self.size / 2) do
        local temp = self.points[index]
        local next = self.size - index + 1
        self.points[index] = self.points[next]
        self.points[next] = temp
    end
end

function Polygon:Set(index, point)
    if index > self.size then
        self:Add(point) return end
    self.points[index] = point
end

------------------------------------------
-- Path structure (for IsPathDangerous) --

local Path = Class()

function Path:__init(speed, delay, delta, startPos, endPos)
    self.startPos = startPos or Vector:New()
    self.endPos = endPos or Vector:New()
    self.speed = speed or math.huge
    self.delay = delay or 0
    self.delta = delta or 0
end

--------------------------
-- Skillshot superclass --

_G.DetectionType = {
    ["UNDEFINED"] = 0,
    ["ON_ACTIVE_SPELL"] = 1,
    ["ON_NEW_PATH"] = 2,
    ["ON_OBJECT_CREATED"] = 3,
    ["ON_PROCESS_SPELL"] = 4,
    ["ON_WND_PROC"] = 5
}

_G.CollisionFlag = {
    ["CHAMPION"] = 0,
    ["MINION"] = 1,
    ["TERRAIN_WALL"] = 2,
    ["WIND_WALL"] = 3,
}

_G.SkillshotType = {
    ["CIRCLE"] = 0,
    ["CONE"] = 1,
    ["LINE"] = 2,
    ["POLYGON"] = 3,
    ["RING"] = 4
}

local uniqueId = 0
local Skillshot = Class()

function Skillshot:__init()
    uniqueId = uniqueId + 1
    self.id = uniqueId
    self.arcStep = 0
    self.coneAngle = 0
    self.dangerLevel = 0
    self.extraDuration = 0
    self.height = 0
    self.offset = 0
    self.preDelay = 0
    self.rotAngle = 0
    self.radius = 0
    self.range = 0
    self.sideRange = 0
    self.speed = 0
    self.startTime = 0
    self.crowdControl = false
    self.fixedRange = false
    self.fogOfWar = false
    self.hitbox = false
    self.invert = false
    self.processed = false
    self.rotate90 = false
    self.caster = myHero
    self.casterName = ""
    self.name = ""
    self.slot = ""
    self.collisions = {}
    self.offsetPolygon = Polygon:New()
    self.polygon = Polygon:New()
    self.geometry = Geometry:New()
    self.destPos = Vector:New()
    self.direction = Vector:New()
    self.endPos = Vector:New()
    self.perpendicular = Vector:New()
    self.position = Vector:New()
    self.startPos = Vector:New()
    self.detectionType = _G.DetectionType.UNDEFINED
    self.skillshotType = _G.SkillshotType.CIRCLE
end

function Skillshot:Draw(color)
    local polygon = self.hitbox == false and
        self.polygon or self.offsetPolygon
    polygon:Draw(color, self.height)
end

function Skillshot:FixOrigin()
    local unfixed = not self.fixedRange
    local dist = self.startPos:Distance(self.destPos)
    if unfixed and dist < self.range then self.range = dist end
    self.direction = (self.destPos - self.startPos):Normalize()
    self.direction = self.direction:Rotate(self.rotAngle)
    self.perpendicular = self.direction:Perpendicular()
    self.endPos = self.startPos + self.direction * self.range
    self.startPos = self.startPos + self.direction * self.offset
    self.range, self.offset = self.range - self.offset, 0
    if self.rotate90 then
        local perp = self.perpendicular
        local distance = perp * self.sideRange
        self.startPos = self.endPos - distance * 0.5
        self.endPos = self.endPos + distance * 0.5
        self.direction = self.perpendicular:Clone()
        self.perpendicular = self.direction:Perpendicular()
        self.range, self.rotate90 = self.sideRange, false
    end
    if not self.invert then return end
    self.direction = -self.direction
    self.perpendicular = -self.perpendicular
    local spos, epos = self.startPos, self.endPos
    self.startPos, self.endPos = epos, spos
    self.invert = false
end

function Skillshot:Initialise(data)
    self:LoadData(data)
    self:FixOrigin()
    self:Update()
end

function Skillshot:IsExpired()
    local timer = directives.timer()
    local elapsed = timer - self.startTime
    return elapsed >= self:TotalLifeTime()
end

function Skillshot:IsPathDangerousStatic(path)
    -- supports all skillshots with no displacement
    local safe = self:IsSafe(path.endPos)
    if safe == false then return true end
    local timeToHit = self:TimeToHitStatic()
    local left, pos = timeToHit - path.delay, {}
    if left <= 0 then return self:IsDangerous(
        path.startPos) or #self:PathIntersection(
        path.startPos, path.endPos) > 0 end
    if path.speed == math.huge then return false end
    local dir = (path.endPos - path.startPos):Normalize()
    local signs = path.delta > 0 and { -1, 1 } or { 1 }
    for i, sign in ipairs(signs) do
        local offset = left + path.delta * sign
        pos[i] = path.startPos + dir * (path.speed * offset)
        if self:IsDangerous(pos[i]) then return true end
    end
    return #pos > 1 and #self:PathIntersection(pos[1], pos[2]) > 0
end

function Skillshot:IsSafe(pos)
    return not self:IsDangerous(pos)
end

function Skillshot:LoadData(data)
    for parameter, value in pairs(data) do
        if self[parameter] ~= nil then
            self[parameter] = value
        end
    end
end

function Skillshot:TimeToHitStatic()
    local timer = directives.timer()
    return math.max(0, self.startTime - timer +
        self.range / self.speed + self.preDelay)
end

function Skillshot:TotalLifeTime()
    return self.range / self.speed +
        self.preDelay + self.extraDuration
end

-- Abstract methods

function Skillshot:IsDangerous(pos) end

function Skillshot:IsPathDangerous(path) end

function Skillshot:PathIntersection(p1, p2) end

function Skillshot:Position(delta) end

function Skillshot:TimeToHit(pos) end

function Skillshot:Update() end

---------------------------------
-- Circular skillshot subclass --

local Circle = Class(Skillshot)

function Circle:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.endPos
    self.skillshotType = _G.SkillshotType.CIRCLE
end

function Circle:Draw(color) -- @override
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    self.geometry:DrawCircle(self.position,
        self.radius + hitbox, color, self.height)
end

function Circle:IsDangerous(pos)
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local dist = pos:DistanceSquared(self.position)
    return dist <= (self.radius + hitbox) ^ 2
end

function Circle:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Circle:PathIntersection(p1, p2)
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    return self.geometry:CircleSegmentIntersection(
        p1, p2, self.position, self.radius + hitbox)
end

function Circle:Position(delta)
    return self.position
end

function Circle:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Circle:Update() end

------------------------------
-- Conic skillshot subclass --

local Cone = Class(Skillshot)

function Cone:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.startPos
    self.skillshotType = _G.SkillshotType.CONE
end

function Cone:IsDangerous(pos)
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local startPos, endPos = self.startPos, self.endPos
    local inside = self.geometry:IsInsideCone(startPos,
        endPos, pos, self.range + hitbox, self.coneAngle)
    if self.hitbox == true and inside == false then
        local arc, last = self.endArc, #self.endArc
        local d1 = pos:DistSqrToSegment(startPos, arc[1])
        local d2 = pos:DistSqrToSegment(startPos, arc[last])
        inside = d1 <= hitbox ^ 2 or d2 <= hitbox ^ 2
    end
    if inside == false then return false end
    local offset = startPos - self.direction * hitbox
    local dist = startPos:DistanceSquared(self.position)
    return dist <= offset:DistanceSquared(pos)
end

function Cone:IsPathDangerous(path)
    local safe = self:IsSafe(path.endPos)
    if safe == false then return true end
    if self.speed == math.huge then return
        self:IsPathDangerousStatic(path) end
    local time = self:TimeToHit(path.startPos)
    if time >= 0 and time <= path.delay +
        path.delta then return true end
    local infinite = path.speed == math.huge
    if infinite then return false end
    -- predict position after delay
    local dir = path.endPos - path.startPos
    local delay = math.max(0, self.preDelay -
        directives.timer() + self.startTime)
    dir = dir:Normalize() * path.speed
    local pos = path.startPos + dir * delay
    -- interception test
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local offset = path.speed * path.delta +
        (directives.timer() - self.startTime
        + path.delay) * self.speed + hitbox
    local time = self.geometry:Interception(pos,
        self.startPos, dir, self.speed, offset)
    if time == nil then return false end
    -- check if a collision point is unsafe
    return self:IsDangerous(pos + dir * time)
end

function Cone:PathIntersection(p1, p2)
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local result, pos = Linq(), self.position
    local arcIntersects = function(range, spos, epos, phi)
        return self.geometry:ArcSegmentIntersection(p1,
        p2, spos or self.startPos, epos or self.endPos,
        range or 0, phi or self.coneAngle or 0.5) end
    -- first case: cone's side edges
    local c1, c2 = #self.startArc, #self.endArc
    local left = {self.startArc[c1], self.endArc[c2]}
    local right = {self.startArc[1], self.endArc[1]}
    local edge1, edge2, dir1, dir2 = left, right
    if self.hitbox == true then
        dir1 = (left[2] - left[1]):Normalize() * hitbox
        dir2 = (right[2] - right[1]):Normalize() * hitbox
        local perp1 = dir1:Perpendicular()
        local perp2 = dir2:Perpendicular2()
        edge1 = {left[1] + perp1, left[2] + perp1}
        edge2 = {right[1] + perp2, right[2] + perp2}
    end
    local seg1 = edge1[1]:SegmentIntersection(edge1[2], p1, p2)
    local seg2 = edge2[1]:SegmentIntersection(edge2[2], p1, p2)
    if seg1 ~= nil then table.insert(result, seg1) end
    if seg2 ~= nil then table.insert(result, seg2) end
    -- next case: cone's ending (and starting) arc
    local distance = pos:Distance(self.startPos)
    result:AddRange(arcIntersects(distance - hitbox))
    result:AddRange(arcIntersects(self.range + hitbox))
    if not self.hitbox then return result:Distinct() end
    -- last case: rounded corners
    local angle, corners = math.pi * 0.5, {}
    corners[1] = {left[1], edge1[1], left[1] - dir1}
    corners[2] = {left[2], edge1[2], left[2] + dir1}
    corners[3] = {right[1], edge2[1], right[1] - dir2}
    corners[4] = {right[2], edge2[2], right[2] + dir2}
    for index, corner in ipairs(corners) do
        result:AddRange(arcIntersects(hitbox, corner[1],
            (corner[2] + corner[3]) * 0.5, angle))
    end
    return result:Distinct()
end

function Cone:Position(delta)
    local delta = delta or 0
    local infinite = self.speed == math.huge
    if infinite then return self.startPos end
    local t = math.max(0, delta - self.preDelay
        + directives.timer() - self.startTime)
    local x = math.min(self.speed * t, self.range)
    return self.startPos + self.direction * x
end

function Cone:TimeToHit(pos)
    if self:IsSafe(pos) then return -1
    elseif self.speed == math.huge then
        return self:TimeToHitStatic() end
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local dist = self.startPos:Distance(pos)
    local time = (dist - hitbox) / self.speed
    return math.max(0, time + self.preDelay -
        directives.timer() + self.startTime)
end

function Cone:Update()
    self.position = self:Position()
    if self.position == self.endPos
        then self.offsetPolygon:Clear()
        self.polygon:Clear() return end
    -- build an updated arc / sector structure
    self.startArc = self.geometry:Arc(self.startPos,
        self.position, self.coneAngle, self.arcStep)
    self.endArc = self.geometry:Arc(self.startPos,
        self.endPos, self.coneAngle, self.arcStep)
    -- build a polygon area for drawings
    self.polygon = Polygon:New()
    self.polygon:Append(self.startArc)
    self.polygon:Reverse()
    self.polygon:Append(self.endArc)
    if not self.hitbox then return end
    self.offsetPolygon = self.polygon:Offset(
        directives.hitbox(), self.arcStep)
end

-------------------------------
-- Linear skillshot subclass --

local Line = Class(Skillshot)

function Line:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.startPos
    self.skillshotType = _G.SkillshotType.LINE
end

function Line:IsDangerous(pos)
    local inside = self.polygon:IsInside(pos)
    if not self.hitbox or inside then return inside end
    return self.polygon.points:Min(function(p1, index)
        local p2 = self.polygon:Get(index % 4 + 1)
        return pos:DistSqrToSegment(p1, p2)
    end) <= directives.hitbox() ^ 2
end

function Line:IsPathDangerous(path)
    local safe = self:IsSafe(path.endPos)
    if safe == false then return true end
    if self.speed == math.huge then return
        self:IsPathDangerousStatic(path) end
    local time = self:TimeToHit(path.startPos)
    if time >= 0 and time <= path.delay +
        path.delta then return true end
    local infinite = path.speed == math.huge
    if infinite then return false end
    -- predict positions after delay
    local pos = self:Position(path.delay)
    local delay = math.max(0, self.preDelay -
        directives.timer() + self.startTime)
    local dir = path.endPos - path.startPos
    dir = dir:Normalize() * path.speed * delay
    local startPos = path.startPos + dir
    local endPos = path.endPos + dir
    -- dynamic collision test
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local time = self.geometry:DynamicCollision(
        pos, self.endPos, startPos, endPos,
        self.speed, path.speed, self.radius,
        hitbox + path.speed * path.delta)
    -- no collision detected, path is safe
    if time == nil then return false end
    -- collision detected, but if it will happen
    -- after skillshot expiration then return false
    return time + delay <= self:TimeToHitStatic()
end

function Line:PathIntersection(p1, p2)
    local excl, poly = not self.hitbox, self.polygon
    if excl then return poly:PathIntersection(p1, p2) end
    if poly.size ~= 4 then return Linq() end
    local hitbox = directives.hitbox()
    local dir = self.direction * hitbox
    local perp = self.perpendicular * hitbox
    local result, cases = Linq(), {}
    cases[1] = {perp, poly:Get(1) - dir + perp}
    cases[2] = {dir, poly:Get(2) + dir + perp}
    cases[3] = {-perp, poly:Get(3) + dir - perp}
    cases[4] = {-dir, poly:Get(4) - dir - perp}
    for index, case in ipairs(cases) do
        local current = poly:Get(index)
        local o1 = current + case[1]
        local o2 = poly:Get(index % 4 + 1) + case[1]
        local int = o1:SegmentIntersection(o2, p1, p2)
        if int ~= nil then table.insert(result, int) end
        result:AddRange(self.geometry:ArcSegmentIntersection(
            p1, p2, current, case[2], hitbox, math.rad(90)))
    end
    return result:Distinct()
end

function Line:Position(delta)
    local delta = delta or 0
    local infinite = self.speed == math.huge
    if infinite then return self.startPos end
    local t = math.max(0, delta - self.preDelay
        + directives.timer() - self.startTime)
    local x = math.min(self.speed * t, self.range)
    return self.startPos + self.direction * x
end

function Line:TimeToHit(pos)
    if self:IsSafe(pos) then return -1
    elseif self.speed == math.huge then
        return self:TimeToHitStatic() end
    local hitbox = directives.hitbox()
    if not self.hitbox then hitbox = 0 end
    local time = self.geometry:DynamicCollision(
        self.position, self.endPos, pos, pos,
        self.speed, 0, self.radius, hitbox)
    local delay = math.max(0, self.preDelay -
        directives.timer() + self.startTime)
    return delay + math.max(0, time or 0)
end

function Line:Update()
    self.position = self:Position()
    if self.position == self.endPos
        then self.offsetPolygon:Clear()
        self.polygon:Clear() return end
    local dir = self.perpendicular * self.radius
    self.polygon:Set(1, self.position + dir)
    self.polygon:Set(2, self.endPos + dir)
    self.polygon:Set(3, self.endPos - dir)
    self.polygon:Set(4, self.position - dir)
    if not self.hitbox then return end
    self.offsetPolygon = self.polygon:Offset(
        directives.hitbox(), self.arcStep)
end

----------------------------------
-- Polygonal skillshot subclass --

local Poly = Class(Skillshot)

function Poly:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.position = self.endPos
    self.skillshotType = _G.SkillshotType.POLYGON
end

function Poly:IsDangerous(pos)
    return self.offsetPolygon:IsInside(pos)
end

function Poly:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Poly:PathIntersection(p1, p2)
    return self.offsetPolygon:PathIntersection(p1, p2)
end

function Poly:Position(delta)
    return self.position
end

function Poly:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Poly:Update()
    self.offsetPolygon = self.polygon
    if not self.hitbox then return end
    self.offsetPolygon = self.polygon:Offset(
        directives.hitbox(), self.arcStep)
end

-------------------------------
-- Ringed skillshot subclass --

local Ring = Class(Skillshot)

function Ring:__init(data)
    Skillshot.__init(self)
    self:Initialise(data)
    self.innerRadius = data.innerRadius
    self.outerRadius = data.outerRadius
    self.position = self.endPos
    self.skillshotType = _G.SkillshotType.RING
end

function Ring:Draw(color) -- @override
    self.geometry:DrawCircle(self.position,
        self.innerRadius, color, self.height)
    self.geometry:DrawCircle(self.position,
        self.outerRadius, color, self.height)
end

function Ring:IsDangerous(pos)
    local ri, ro = self.innerRadius, self.outerRadius
    local dist = pos:DistanceSquared(self.position)
    return dist >= ri * ri and dist <= ro * ro
end

function Ring:IsPathDangerous(path)
    return self:IsPathDangerousStatic(path)
end

function Ring:PathIntersection(p1, p2)
    return self.geometry:CircleSegmentIntersection(
        p1, p2, self.position, self.innerRadius)
    :Concat(self.geometry:CircleSegmentIntersection(
        p1, p2, self.position, self.outerRadius))
end

function Ring:Position(delta)
    return self.position
end

function Ring:TimeToHit(pos)
    return self:IsDangerous(pos) and
        self:TimeToHitStatic() or -1
end

function Ring:Update() end

---------------------
-- Core algorithms --

_G.SortMode = {
    ["SHORTEST_PATH"] = 0,
    ["MOUSE_DIRECTION"] = 1
}

local Core = Class()

function Core:__init(skillshots, step)
    self.angleStep = step or 12
    self.skillshots = skillshots
    self.sortingModes = {
        [_G.SortMode.SHORTEST_PATH] = function(a, b)
            local hero = Vector:New(directives.position())
            return hero:DistanceSquared(a) <
                hero:DistanceSquared(b) end,
        [_G.SortMode.MOUSE_DIRECTION] = function(a, b)
            local mouse = Vector:New(directives.mousePos())
            local hero = Vector:New(directives.position())
            return hero:AngleBetween(a, mouse, true) <
                hero:AngleBetween(b, mouse, true) end
    }
end

function Core:IsDangerous(pos)
    return self.skillshots:Any(function(s)
        return s:IsDangerous(pos) end)
end

function Core:IsPathDangerous(path)
    return self.skillshots:Any(function(s)
        return s:IsPathDangerous(path) end)
end

function Core:IsSafe(pos)
    return not self:IsDangerous(pos)
end

function Core:FindSafeSpots(path, maxRange, fixedRange, buffer)
    local buffer, results = buffer or 1, {}
    path.startPos = not path.startPos:IsZero() and
        path.startPos or Vector:New(directives.position())
    for i = 0, 360 - self.angleStep, self.angleStep do
        -- rotate around hero and create a one-way path
        local rotated = Vector:New(0, 1):Rotate(math.rad(i))
        path.endPos = path.startPos + rotated * (maxRange or 500)
        -- get and analyse intersection points or fixed path
        local destination = fixedRange and path.endPos
            or self.skillshots:Select(function(s) return
                s:PathIntersection(path.startPos, path.endPos) end)
            :Aggregate(function(r, i) return r:Concat(i) end, Linq())
            :OrderBy(function(a, b) return a:DistanceSquared(path
                .startPos) < b:DistanceSquared(path.startPos) end)
            :First(function(i) local after = i + rotated
                return not self:IsDangerous(after) end)
        if destination == nil then goto continue end
        path.endPos = destination + rotated * buffer
        local dangerous = self:IsPathDangerous(path)
        if dangerous == true then goto continue end
        path.endPos = destination + rotated
        table.insert(results, path.endPos)
        ::continue::
    end
    return results
end

function Core:GetEvadeSpot(path, maxRange, fixedRange, buffer)
    local delta = path.delta
    for index = 1, 0, -1 do
        path.delta = delta + 0.1 * index
        local spots = self:FindSafeSpots(
            path, maxRange, fixedRange, buffer)
        table.sort(spots, self.SortMode[index])
        if #spots > 0 then return spots[1] end
    end
    return nil
end

function Core:TimeToHit(pos)
    local times = self.skillshots:Select(function(s)
        return s:TimeToHit(pos) end):Where("(t) => t >= 0")
    table.sort(times, function(a, b) return a < b end)
    return #times > 0 and times[1] or -1
end

return {
    Linq = Linq,
    Vector = Vector,
    Polygon = Polygon,
    Path = Path,
    Circle = Circle,
    Cone = Cone,
    Line = Line,
    Poly = Poly,
    Ring = Ring,
    Core = Core
}

