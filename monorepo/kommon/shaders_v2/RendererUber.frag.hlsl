Texture2D<float4> Textures[2] : register(t0, space2);
SamplerState      Samplers[2] : register(s0, space2);

struct Input
{
    float4 Color : TEXCOORD0;
    float2 TexCoord : TEXCOORD1;
    nointerpolation uint TextureId : TEXCOORD2;
    nointerpolation uint MaterialId : TEXCOORD3;
};

float4 sampleTexture(uint id, float2 uv)
{
    switch (id)
    {
        case 0:  return float4(1,1,1,1); // no texture = pure white
        // TODO: investigate about mipmaps
        case 1:  return Textures[0].SampleLevel(Samplers[0], uv, 0.0f);
        case 2:  return Textures[1].SampleLevel(Samplers[1], uv, 0.0f);
        // case 3:  return Textures[2].SampleLevel(Samplers[2], uv, 0.0f);
        default: return float4(1, 0, 1, 1); // magenta = missing texture
    }
}

float4 main(Input input) : SV_Target0
{
    return input.Color * sampleTexture(input.TextureId, input.TexCoord);
}
