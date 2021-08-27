#version 450
layout(location = 0) in vec2 fsin_texCoords;
layout(location = 1) in vec4 fsin_color;
layout(location = 0) out vec4 fsout_color;
layout(set = 1, binding = 1) uniform texture2D SurfaceTexture;
layout(set = 1, binding = 2) uniform sampler SurfaceSampler;
void main()
{
    vec4 texColor = texture(sampler2D(SurfaceTexture, SurfaceSampler), fsin_texCoords);
    fsout_color =  texColor * fsin_color;
}