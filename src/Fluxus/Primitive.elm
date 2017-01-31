module Fluxus.Primitive exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import WebGL exposing (Mesh, Shader, Entity)
import Fluxus.Texture exposing (Texture)
import Fluxus.State exposing (State, Environment)

type alias Primitive =
    { mesh: Mesh Vertex
    , texture: Maybe Texture
    -- , parent: Primitive
    -- , children: List Primitive
    }

drawCube : State -> State
drawCube state =
    state |> draw buildCube

draw : Mesh Vertex -> State -> State
draw mesh (env, entities) =
    ( env
    , entities ++ [ toInitialEntity env mesh ]
    )

type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


buildCube : Mesh Vertex
buildCube =
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

toEntity : Environment -> Primitive -> Entity
toEntity environment primitive  =
    WebGL.entity
        vertexShader
        fragmentShader
        primitive.mesh
        { perspective = environment.perspective
        , transform = environment.transform
        -- , texture = primitive.texture
        , color = environment.color
        }

toInitialEntity : Environment -> Mesh Vertex -> Entity
toInitialEntity environment mesh =
    toEntity
        environment
        { mesh = mesh
        , texture = Maybe.Nothing
        }

type alias Uniforms =
    { color : Vec3
    , transform: Mat4
    , perspective : Mat4
    }


vertexShader : Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        uniform mat4 transform;
        varying vec2 vcoord;

        void main () {
          gl_Position = perspective * transform * vec4(position, 1.0);
          vcoord = coord;
        }

    |]


fragmentShader : Shader {} Uniforms { vcoord : Vec2 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform vec3 color;
        varying vec2 vcoord;

        void main () {
          gl_FragColor = vec4(color, 1.0);
        }

    |]
