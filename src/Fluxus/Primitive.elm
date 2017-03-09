module Fluxus.Primitive exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import WebGL exposing (Mesh)

import Fluxus.Link exposing (Vertex)

cube : Mesh Vertex
cube =
    [ ( 0, 0 ), ( 90, 0 ), ( 180, 0 ), ( 270, 0 ), ( 0, 90 ), ( 0, -90 ) ]
        |> List.concatMap rotatedSquare
        |> WebGL.triangles


rotatedSquare : ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
rotatedSquare ( angleXZ, angleYZ ) =
    let
        transformMat =
            Mat4.mul
                (Mat4.makeRotate (degrees angleXZ) Vec3.j)
                (Mat4.makeRotate (degrees angleYZ) Vec3.i)

        transform vertex =
            { vertex
                | position =
                    Mat4.transform transformMat vertex.position
            }

        transformTriangle ( a, b, c ) =
            ( transform a, transform b, transform c )
    in
        List.map transformTriangle square


square : List ( Vertex, Vertex, Vertex )
square =
    let
        topLeft =
            Vertex (vec3 -1 1 1) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 1) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
    in
        [ ( topLeft, topRight, bottomLeft )
        , ( bottomLeft, topRight, bottomRight )
        ]
