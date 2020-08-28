using System;

namespace ShaderGen
{
    public enum ShaderFunctionType
    {
        Normal = 0,
        VertexEntryPoint,
        FragmentEntryPoint,
        ComputeEntryPoint,
        GeometryEntryPoint
    }

    public static class ShaderFunctionTypes
    {
        public static ShaderStage Stage(this ShaderFunctionType type)
        {
            switch (type)
            {
                case ShaderFunctionType.VertexEntryPoint:
                    return ShaderStage.Vertex;
                case ShaderFunctionType.GeometryEntryPoint:
                    return ShaderStage.Geometry;
                case ShaderFunctionType.FragmentEntryPoint:
                    return ShaderStage.Fragment;
                case ShaderFunctionType.ComputeEntryPoint:
                    return ShaderStage.Compute;
                default:
                    throw new NotSupportedException(type.ToString());
            }
        }
    }
}
