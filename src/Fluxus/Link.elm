module Fluxus.Link exposing
    ( init
    , toEntity
    , Vertex
    , Uniforms
    )

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import WebGL exposing (Entity, Mesh, Shader)

type alias Uniforms =
    { color: Vec3
    , transform: Mat4
    , perspective: Mat4
    }

type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }

init : Uniforms
init =
    { color = (vec3 1 1 1)
    , transform = Mat4.identity
    , perspective = Mat4.identity
    }

toEntity : Uniforms -> Mesh Vertex -> Entity
toEntity uniforms mesh =
    WebGL.entity
        vertexShader
        fragmentShader
        mesh
        uniforms

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
