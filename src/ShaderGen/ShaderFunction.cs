namespace ShaderGen
{
    public class ShaderFunction
    {
        public string DeclaringType { get; }
        public string Name { get; }
        public TypeReference ReturnType { get; }
        public int ColorOutputCount { get; } // TODO: This always returns 0.
        public ParameterDefinition[] Parameters { get; }
        public StructureDefinition InputType { get; internal set; }
        public StructureDefinition OutputType { get; internal set; }
        public FieldDefinition[] InputFields { get; internal set; }
        public FieldDefinition[] OutputFields { get; internal set; }
        public PrimitiveType InputPrimitive { get; internal set; }
        public PrimitiveType OutputPrimitive { get; internal set; }
        public int MaxVertices { get; internal set; }
        public bool IsEntryPoint => Type != ShaderFunctionType.Normal;
        public ShaderFunctionType Type { get; }
        public UInt3 ComputeGroupCounts { get; }
        public bool UsesVertexID { get; internal set; }
        public bool UsesInstanceID { get; internal set; }
        public bool UsesDispatchThreadID { get; internal set; }
        public bool UsesGroupThreadID { get; internal set; }
        public bool UsesFrontFace { get; internal set; }
        public bool UsesTexture2DMS { get; internal set; }
        public bool UsesStructuredBuffer { get; internal set; }
        public bool UsesRWTexture2D { get; internal set; }
        public bool UsesInterlockedAdd { get; internal set; }

        public ShaderFunction(
            string declaringType,
            string name,
            TypeReference returnType,
            ParameterDefinition[] parameters,
            ShaderFunctionType type,
            UInt3 computeGroupCounts)
        {
            DeclaringType = declaringType;
            Name = name;
            ReturnType = returnType;
            Parameters = parameters;
            Type = type;
            ComputeGroupCounts = computeGroupCounts;
        }

        public override string ToString() => $"{DeclaringType}.{Name} [{Type}]";
    }
}
