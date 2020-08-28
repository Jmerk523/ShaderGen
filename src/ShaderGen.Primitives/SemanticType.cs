using System;

namespace ShaderGen
{
    public enum SemanticType
    {
        None = 0,
        // Vertex
        Position = 1,
        Normal,
        TextureCoordinate,
        Color,
        Tangent,
        //Draw,
        //BaseVertex,
        //BaseInstance,
        // Fragment
        SystemPosition,// = Position | Fragment, // FragCoord
        ColorTarget,// = Color | Fragment,
        //Depth,
        //Point,
        //FrontFacing,
        //PointSize,
        //Sample,
        //SamplePosition,
        //SampleMask,
        // Geometry
        //ClipDistance,
        //Instance,
        //Primitive,
        //Layer,
        //Viewport,
    }

    public enum BuiltinSemantic
    {
        None,
        Position,
        Normal,
        TextureCoordinate,
        Color,
        Tangent,
        Draw,
        BaseVertex,
        BaseInstance,
        Depth,
        Point,
        FrontFacing,
        PointSize,
        Sample,
        SamplePosition,
        SampleMask,
        ClipDistance,
        PerVertex,
        Instance,
        Primitive,
        Layer,
        Viewport,
        Vertex,
        PatchVertices,
        TesselatedCoordinate,
        TesselationLevelOuter,
        TesselationLevelInner,
        MaxSemantic = TesselationLevelInner,
    }

    [Flags]
    public enum ShaderStage
    {
        None = 0,
        Vertex = 1,
        TesselationControl = 2,
        TesselationEvaluation = 4,
        Geometry = 8,
        Fragment = 16,
        PerSample = 32,
        Compute = 64,
        LastGraphicsStage = Fragment,
    }

    public readonly struct ShaderBuiltin : IEquatable<ShaderBuiltin>
    {
        private static readonly ShaderStage[] stageInputs = new ShaderStage[(int)BuiltinSemantic.MaxSemantic + 1];

        static ShaderBuiltin()
        {
            stageInputs[(int)BuiltinSemantic.Position] = ShaderStage.Vertex | ShaderStage.TesselationControl | ShaderStage.TesselationEvaluation | ShaderStage.Geometry | ShaderStage.Fragment | ShaderStage.PerSample;
            stageInputs[(int)BuiltinSemantic.Normal] = ShaderStage.Vertex;
            stageInputs[(int)BuiltinSemantic.TextureCoordinate] = ShaderStage.Vertex | ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.Color] = ShaderStage.Vertex | ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.Tangent] = ShaderStage.Vertex;
            stageInputs[(int)BuiltinSemantic.Draw] = ShaderStage.Vertex;
            stageInputs[(int)BuiltinSemantic.BaseVertex] = ShaderStage.Vertex;
            stageInputs[(int)BuiltinSemantic.BaseInstance] = ShaderStage.Vertex;
            stageInputs[(int)BuiltinSemantic.Depth] = ShaderStage.PerSample;
            stageInputs[(int)BuiltinSemantic.Point] = ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.FrontFacing] = ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.PointSize] = ShaderStage.TesselationControl | ShaderStage.TesselationEvaluation | ShaderStage.Geometry;
            stageInputs[(int)BuiltinSemantic.Sample] = ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.SamplePosition] = ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.SampleMask] = ShaderStage.Fragment | ShaderStage.PerSample;
            stageInputs[(int)BuiltinSemantic.ClipDistance] = ShaderStage.TesselationControl | ShaderStage.TesselationEvaluation | ShaderStage.Geometry | ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.PerVertex] = ShaderStage.TesselationControl | ShaderStage.TesselationEvaluation | ShaderStage.Geometry;
            stageInputs[(int)BuiltinSemantic.Instance] = ShaderStage.Vertex | ShaderStage.TesselationControl | ShaderStage.Geometry;
            stageInputs[(int)BuiltinSemantic.Primitive] = ShaderStage.TesselationControl | ShaderStage.TesselationEvaluation | ShaderStage.Geometry | ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.Layer] = ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.Viewport] = ShaderStage.Fragment;
            stageInputs[(int)BuiltinSemantic.Vertex] = ShaderStage.Vertex;
            stageInputs[(int)BuiltinSemantic.PatchVertices] = ShaderStage.TesselationControl | ShaderStage.TesselationEvaluation;
            stageInputs[(int)BuiltinSemantic.TesselatedCoordinate] = ShaderStage.TesselationEvaluation;
            stageInputs[(int)BuiltinSemantic.TesselationLevelOuter] = ShaderStage.TesselationEvaluation;
            stageInputs[(int)BuiltinSemantic.TesselationLevelInner] = ShaderStage.TesselationEvaluation;
        }

        public static ShaderBuiltin FromSemanticType(SemanticType semantic)
        {
            switch (semantic)
            {
                case SemanticType.Color:
                    return new ShaderBuiltin(ShaderStage.Vertex, BuiltinSemantic.Color);
                case SemanticType.ColorTarget:
                    return new ShaderBuiltin(ShaderStage.Fragment, BuiltinSemantic.Color);
                case SemanticType.Normal:
                    return new ShaderBuiltin(ShaderStage.Vertex, BuiltinSemantic.Normal);
                case SemanticType.Position:
                    return new ShaderBuiltin(ShaderStage.Vertex, BuiltinSemantic.Position);
                case SemanticType.SystemPosition:
                    return new ShaderBuiltin(ShaderStage.Fragment, BuiltinSemantic.Position);
                case SemanticType.Tangent:
                    return new ShaderBuiltin(ShaderStage.Vertex, BuiltinSemantic.Tangent);
                case SemanticType.TextureCoordinate:
                    return new ShaderBuiltin(ShaderStage.Vertex, BuiltinSemantic.TextureCoordinate);
                default:
                    throw new NotSupportedException(semantic.ToString());
            }
        }

        public static ShaderStage GetStages(BuiltinSemantic semantic)
        {
            return stageInputs[(int)semantic];
        }

        public static ShaderStage NextStage(ShaderStage stage)
        {
            if (stage > ShaderStage.LastGraphicsStage)
            {
                return ShaderStage.None;
            }
            return (ShaderStage)((int)stage << 1);
        }

        public BuiltinSemantic Semantic { get; }

        public ShaderStage Stage { get; }

        public ShaderBuiltin(ShaderStage stage, BuiltinSemantic semantic)
        {
            Stage = stage;
            Semantic = semantic;
        }

        public ShaderBuiltin(BuiltinSemantic semantic)
        {
            Semantic = semantic;
            Stage = GetStages(semantic);
        }

        public ShaderBuiltin(SemanticType semantic)
        {
            this = FromSemanticType(semantic);
        }

        public ShaderBuiltin(GeometrySemantic semantic)
        {
            Stage = ShaderStage.Geometry;
            switch (semantic)
            {
                case GeometrySemantic.ClipDistance:
                    Semantic = BuiltinSemantic.ClipDistance;
                    break;
                case GeometrySemantic.Instance:
                    Semantic = BuiltinSemantic.Instance;
                    break;
                case GeometrySemantic.Layer:
                    Semantic = BuiltinSemantic.Layer;
                    break;
                case GeometrySemantic.PointSize:
                    Semantic = BuiltinSemantic.PointSize;
                    break;
                case GeometrySemantic.Position:
                    Semantic = BuiltinSemantic.Position;
                    break;
                case GeometrySemantic.Primitive:
                    Semantic = BuiltinSemantic.Primitive;
                    break;
                case GeometrySemantic.Viewport:
                    Semantic = BuiltinSemantic.Viewport;
                    break;
                case GeometrySemantic.PerVertex:
                    Semantic = BuiltinSemantic.PerVertex;
                    break;
                default:
                    Semantic = BuiltinSemantic.None;
                    break;
            }
        }

        public ShaderBuiltin WithStage(ShaderStage stage)
        {
            return new ShaderBuiltin(stage, Semantic);
        }

        public ShaderBuiltin WithSemantic(BuiltinSemantic semantic)
        {
            return new ShaderBuiltin(Stage, semantic);
        }

        public override string ToString()
        {
            return $"{Semantic} in {Stage}";
        }

        public override bool Equals(object obj)
        {
            return obj is ShaderBuiltin builtin && Equals(builtin);
        }

        public bool Equals(ShaderBuiltin other)
        {
            return Semantic == other.Semantic &&
                   Stage == other.Stage;
        }

        public override int GetHashCode()
        {
            int hashCode = 396976950;
            hashCode = hashCode * -1521134295 + Semantic.GetHashCode();
            hashCode = hashCode * -1521134295 + Stage.GetHashCode();
            return hashCode;
        }

        public static bool operator ==(ShaderBuiltin left, ShaderBuiltin right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(ShaderBuiltin left, ShaderBuiltin right)
        {
            return !(left == right);
        }
    }
}
