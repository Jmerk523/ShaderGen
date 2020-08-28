namespace ShaderGen
{
    public enum ShaderResourceKind
    {
        Uniform,
        Texture1D,
        Texture2D,
        Texture3D,
        Texture2DArray,
        Texture2DRect,
        TextureCube,
        Texture2DMS,
        TextureBuffer,
        Sampler,
        StructuredBuffer,
        RWStructuredBuffer,
        RWTexture2D,
        SamplerComparison,
        DepthTexture2D,
        DepthTexture2DArray,
        AtomicBuffer,
        Local,
        Emit,
        BuiltIn
    }
}
