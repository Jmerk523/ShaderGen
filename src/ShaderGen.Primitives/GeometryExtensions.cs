using System;

namespace ShaderGen
{
    [AttributeUsage(AttributeTargets.Field)]
    public sealed class EmitVertexAttribute : Attribute
    {
    }

    public static class GeometryExtensions
    {
        public static void EmitVertex(object variable) => throw new ShaderBuiltinException();

        public static void EndStreamPrimitive(int index) => throw new ShaderBuiltinException();

        public static void EmitStreamVertex(int index, object variable) => EmitVertex(variable);

        public static void EndPrimitive() => EndStreamPrimitive(0);

        public static int VertexCount(this PrimitiveType primitive)
        {
            switch (primitive)
            {
                case PrimitiveType.Points:
                    return 1;
                case PrimitiveType.Lines:
                case PrimitiveType.LineStrip:
                case PrimitiveType.LineList:
                    return 2;
                case PrimitiveType.LinesAdjacency:
                case PrimitiveType.LineStripAdjacency:
                    return 4;
                case PrimitiveType.Triangles:
                case PrimitiveType.TriangleStrip:
                case PrimitiveType.TriangleFan:
                    return 3;
                case PrimitiveType.TrianglesAdjacency:
                case PrimitiveType.TriangleStripAdjacency:
                    return 6;
                default:
                    throw new NotSupportedException();
            }
        }
    }
}
