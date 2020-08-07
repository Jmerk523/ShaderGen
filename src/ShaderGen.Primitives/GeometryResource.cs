using System.Numerics;

namespace ShaderGen
{
    public struct TessGeomPerVertex
    {
        public Vector4 Position;
        public float PointSize;
        public float[] ClipDistance;
    }
}
