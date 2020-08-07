using System;

namespace ShaderGen
{
    public enum PrimitiveType
    {
        Points,
        Lines,
        LineStrip,
        LineList,
        LinesAdjacency,
        LineStripAdjacency,
        Triangles,
        TriangleStrip,
        TriangleFan,
        TrianglesAdjacency,
        TriangleStripAdjacency
    }

    public static class GeometryExtensions
    {
        public static void EmitVertex<T>(T variable)
        {
            throw new ShaderBuiltinException();
        }

        public static void EndStreamPrimitive(int index)
        {
            throw new ShaderBuiltinException();
        }

        public static void EmitStreamVertex<T>(int index, T variable) => EmitVertex(variable);

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
