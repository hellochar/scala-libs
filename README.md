# What is this?

My own personal library of Scala utility functions/classes. Note that this library depends on Processing (and Scala of course). It also has a few utility methods for ControlP5.

## How to use?

Download Misc.jar and include it in your projects.

### Documentation? How do I use this?

Unfortunately, there's a lack of proper documentation for this codebase since it was never meant to be a public release. If you are confused about how to use some parts of the code, feel free to ask me, or read the source. Sorry!

##Contents

The most important/widely used items are: org.zhang.geom.Vec2 and org.zhang.lib.MyPApplet

1. ```org.zhang.lib``` - generic libraries
    1. ```package``` - utility methods
    2. ```MyPApplet``` - Enhances PApplet with integration with Vec2/Vec3
    3. ```P5Util``` - mostly deprecated; holds more esoteric methods for PApplet
    4. ```TravList``` - a list with the notion of a "current location" that you can move and select from
    5. ```HasMV``` - adds mouseVec
2. ```org.zhang.geom``` - Geometry related libraries
    1. ```package``` - random shit
    2. ```Vec2``` - 2D Vector class; used extensively throughout the rest of the library
    3. ```Vec3``` - 3D Vector class; used extensively throughout the rest of the library
    4. Has basic mesh manipulation

3. ```org.zhang.parse``` - Library for parsing and evaluating Algebraic/Mathematical expressions
    1. ```expr.ExprParser``` - Parses a string like "sqrt(x*3)+exp(PI+y)" and lets you evaulate it in an environment like Map(x -> 3, y -> 9)
    2. ```expr.FloatParser``` - Easier than ExprParser but much slower.

