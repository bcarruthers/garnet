#version 450
layout(set = 0, binding = 0) uniform ProjectionBuffer
{
    mat4 Projection;
};
layout(set = 0, binding = 1) uniform ViewBuffer
{
    mat4 View;
};
layout(set = 1, binding = 0) uniform WorldBuffer
{
    mat4 World;
};
layout(set = 1, binding = 1) uniform TexTransformBuffer
{
    mat4 TexTransform;
};
layout(location = 0) in vec3 Position;
layout(location = 1) in vec2 TexCoords;
layout(location = 2) in vec4 FgColor;
layout(location = 3) in vec4 BgColor;
layout(location = 0) out vec2 fsin_texCoords;
layout(location = 1) out vec4 fsin_fg;
layout(location = 2) out vec4 fsin_bg;
void main()
{
    vec4 worldPosition = World * vec4(Position, 1);
    vec4 viewPosition = View * worldPosition;
    vec4 clipPosition = Projection * viewPosition;
    gl_Position = clipPosition;
    fsin_texCoords = (TexTransform * vec4(TexCoords, 1, 1)).xy;
    fsin_fg = FgColor;
    fsin_bg = BgColor;
}
