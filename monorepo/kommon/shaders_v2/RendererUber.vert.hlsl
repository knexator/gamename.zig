struct Drawable
{
    float2 camera_center;
    float2 camera_axis_x;
    float2 camera_axis_y;
    float4 color;
    uint  texture_id;  
    uint  material_id;  
};

struct Input
{
    float2 LocalPos : TEXCOORD0;
    float2 TexCoord : TEXCOORD1;
};

struct Output
{
    float4 Color                    : TEXCOORD0;
    float2 TexCoord                 : TEXCOORD1;
    nointerpolation uint TextureId  : TEXCOORD2;
    nointerpolation uint MaterialId : TEXCOORD3;
    float4 Position                 : SV_Position;
};

StructuredBuffer<Drawable> Draws : register(t0, space0);

Output main(Input input, uint instanceId : SV_InstanceID)
{
    Drawable d = Draws[instanceId];

    float2 p = input.LocalPos;
    float2 clip = d.camera_center + p.x * d.camera_axis_x + p.y * d.camera_axis_y;

    Output output;
    output.Color = d.color;
    output.TexCoord = input.TexCoord;
    output.TextureId = d.texture_id;
    output.MaterialId = d.material_id;
    output.Position = float4(clip, 0.0f, 1.0f);
    return output;
}