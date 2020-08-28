using System;
using System.Collections.Generic;
using System.Text;

namespace ShaderGen
{
    public enum GeometrySemantic
    {
        None = 0,
        Position,
        PointSize,
        ClipDistance,
        Instance,
        Primitive,
        Layer,
        Viewport,
        PerVertex,
    }

    [AttributeUsage(AttributeTargets.Field, AllowMultiple = true)]
    public class GeometrySemanticAttribute : Attribute
    {
        public GeometrySemantic Semantic { get; }

        public GeometrySemanticAttribute(GeometrySemantic semantic)
        {
            Semantic = semantic;
        }
    }
}
