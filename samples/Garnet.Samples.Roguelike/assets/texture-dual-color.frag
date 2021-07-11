#version 450
layout(location = 0) in vec2 fsin_texCoords;
layout(location = 1) in vec4 fsin_fg;
layout(location = 2) in vec4 fsin_bg;
layout(location = 0) out vec4 fsout_color;
layout(set = 1, binding = 2) uniform texture2D SurfaceTexture;
layout(set = 1, binding = 3) uniform sampler SurfaceSampler;
void main()
{
    vec4 texColor = texture(sampler2D(SurfaceTexture, SurfaceSampler), fsin_texCoords);    
    vec4 fg = fsin_fg;
    fg.rgb *= texColor.rgb;
    fsout_color =  mix(fsin_bg, fg, texColor.a);
}