namespace Garnet.Samples.Flocking

open Garnet.Graphics

module Resources =
    let shaderSet = {
        VertexShader = "texture-color.vert"
        FragmentShader = "texture-color.frag"
        Layout = PositionTextureColorVertex.Description
        }
    
    let atlas = "textures"
    let hexTexture = "hex.png"
    let triangleTexture = "triangle.png"
    
    let pipeline = {
        Blend = Blend.Alpha
        Filtering = Filtering.Linear
        ShaderSet = shaderSet
        Texture = atlas
        }
    
    let vehicleLayer = {
        LayerId = 2
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = pipeline 
        }

    let trailLayer = {
        LayerId = 1
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = pipeline 
        }
