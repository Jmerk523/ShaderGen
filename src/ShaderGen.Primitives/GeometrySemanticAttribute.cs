using System;
using System.Collections.Generic;
using System.Text;

namespace ShaderGen
{
    public enum GeometrySemantic
    {
        Position,
        PointSize,
        ClipDistance,
        Instance,
        Primitive,
        Layer,
        Viewport,
    }

    public class GeometrySemanticAttribute : Attribute
    {
        public GeometrySemantic Semantic { get; }

        public GeometrySemanticAttribute(GeometrySemantic semantic)
        {
            Semantic = semantic;
        }
    }
}
