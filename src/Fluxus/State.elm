module Fluxus.State exposing (..)

import List.Extra exposing (getAt)

import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

import WebGL exposing (Entity, Mesh)

import Fluxus.Core exposing (toRadians)
import Fluxus.Link as Link exposing (Uniforms, Vertex)
import Fluxus.Form exposing (Form)

type alias Environment = Uniforms

type alias State =
    { time: Float
    , delta: Float
    , environment: Environment
    --, entities: List Entity
    , meshes: List (Mesh Vertex)
    --, textures: List Texture
    }

init : State
init =
    ( { color = (vec3 1 1 1)
      , transform = Mat4.identity
      , perspective = Mat4.identity
      , delta = 0
      , time = 0
      }
    , []
    )

color : Vec3 -> State -> State
color newColor ( env, entities ) =
    ( { env | color = newColor }
    , entities
    )

rotate : Vec3 -> State -> State
rotate angles (env, entities) =
    let
        ( angleX, angleY, angleZ ) = Vec3.toTuple angles
    in
        ( { env | transform = env.transform |> Mat4.rotate (toRadians angleX) (vec3 1 0 0)
                                            |> Mat4.rotate (toRadians angleY) (vec3 0 1 0)
                                            |> Mat4.rotate (toRadians angleZ) (vec3 0 0 1) }
        , entities
        )

-- rotate : Vec3 -> State -> State
-- rotate angles state =
--     let
--         ( angleX, angleY, angleZ ) = Vec3.toTuple angles
--     in
--         state
--             |> rotateX angleX
--             |> rotateY angleY
--             |> rotateZ angleZ

rotateX : Float -> State -> State
rotateX angleX state =
    rotateByAxis angleX (vec3 1 0 0) state

rotateY : Float -> State -> State
rotateY angleY state =
    rotateByAxis angleY (vec3 0 1 0) state

rotateZ : Float -> State -> State
rotateZ angleZ state =
    rotateByAxis angleZ (vec3 0 0 1) state

rotateByAxis : Float -> Vec3 -> State -> State
rotateByAxis angle axis ( env, entities ) =
    ( { env | transform = env.transform |> Mat4.rotate (toRadians angle) axis }
    , entities
    )

translate : Vec3 -> State -> State
translate position ( env, entities ) =
    ( { env | transform = env.transform |> Mat4.translate position }
    , entities
    )

scale : Vec3 -> State -> State
scale amount ( env, entities ) =
    ( { env | transform = env.transform |> Mat4.scale amount  }
    , entities
    )

advance : Float -> State -> State
advance dt ( env, entities ) =
    ( { env
      | delta = dt
      , time = env.time + dt }
    , entities
    )

next : Mat4 -> Float ->  State -> State
next perspective dt ( env, _ ) =
    ( { env | perspective = perspective }, [] ) |> advance dt

setEntities : List Entity -> State -> State
setEntities newEntities (env, _) =
    ( env, newEntities )

withState : (State -> State) -> State -> State
withState fn outer =
    let
        inner = fn outer
        ( innerEnv, innerEntities ) = inner
        ( outerEnv, outerEntities ) = outer
    in
        ( outerEnv, innerEntities )

time : State -> Float
time { time } = time / 1000

delta : State -> Float
delta { delta } = delta / 1000

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
                            (Link.toEntity uniforms mesh)
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

