module Fluxus.Link exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import Fluxus.Form exposing (Form)
import Fluxus.State exposing (Environment)

type alias Uniforms = Environment

type alias Vertex =
    { position : Vec3
    , coord : Vec2
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

toEntity : Environment -> Form -> Entity
toEntity environment primitive  =
    WebGL.entity
        vertexShader
        fragmentShader
        primitive.mesh
        environment

toInitialEntity : Environment -> Mesh Vertex -> Entity
toInitialEntity environment mesh =
    toEntity
        environment
        { mesh = mesh
        , texture = Maybe.Nothing
        }
