module Fluxus.Link exposing (..)

import List.Extra exposing (getAt)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)

import WebGL exposing (Entity, Mesh, Shader)

import Fluxus.Form exposing (Form)

type alias Meshes = List (Mesh Vertex)

type alias Uniforms =
    { color: Vec3
    , transform: Mat4
    , perspective: Mat4
    , delta: Float
    , time: Float
    , meshes : Meshes
    --. textures: List Texture
    }

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

toEntity : Uniforms -> Form -> Maybe Entity
toEntity uniforms form =
    case form.meshId of
        Just meshId ->
            let
                locatedMesh = locateMesh uniforms.meshes meshId
            in
                case locatedMesh of
                    Just mesh ->
                        Just
                            (WebGL.entity
                                vertexShader
                                fragmentShader
                                mesh
                                uniforms)
                    Nothing -> Nothing
        Nothing -> Nothing

-- toInitialEntity : Uniforms -> Mesh Vertex -> Maybe Entity
-- toInitialEntity uniforms mesh =
--     toEntity
--         uniforms
--         { meshId = mesh
--         , textureId = Maybe.Nothing
--         }

locateMesh : Meshes -> Int -> Maybe (Mesh Vertex)
locateMesh meshes index =
    getAt index meshes
