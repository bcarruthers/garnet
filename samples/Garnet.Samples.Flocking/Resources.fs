namespace Garnet.Samples.Flocking

open Garnet.Graphics

module Resources =
    let shaderSet = {
        VertexShader = "texture-dual-color.vert"
        FragmentShader = "texture-dual-color.frag"
        Layout = PositionTextureDualColorVertex.Description
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
        Depth = 2
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = pipeline 
        }

    let trailLayer = {
        Depth = 1
        CameraId = 0
        Primitive = Quad
        FlushMode = FlushOnDraw
        Pipeline = pipeline 
        }
