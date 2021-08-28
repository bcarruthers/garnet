static float4 _9;
static float4 _11;

struct SPIRV_Cross_Input
{
    float4 _11 : TEXCOORD0;
};

struct SPIRV_Cross_Output
{
    float4 _9 : SV_Target0;
};

void frag_main()
{
    _9 = _11;
}

SPIRV_Cross_Output main(SPIRV_Cross_Input stage_input)
{
    _11 = stage_input._11;
    frag_main();
    SPIRV_Cross_Output stage_output;
    stage_output._9 = _9;
    return stage_output;
}
