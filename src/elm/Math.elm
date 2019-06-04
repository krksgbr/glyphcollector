module Math exposing (scale)


scale : Float -> Float -> Float -> Float -> Float -> Float
scale inMin inMax outMin outMax n =
    (n - inMin) * (outMax - outMin) / (inMax - inMin) + outMin
