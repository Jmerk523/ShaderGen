namespace ShaderGen
{
    public class FieldDefinition
    {
        public string Name { get; }
        public TypeReference Type { get; }
        public SemanticType SemanticType { get; }
        public GeometrySemantic GeometrySemantic { get; }
        public bool IsBuiltIn => SemanticType != SemanticType.None || GeometrySemantic != GeometrySemantic.None;

        /// <summary>
        /// The number of elements in an array, if this is an array field.
        /// Returns 0 if the field is not an array.
        /// </summary>
        public int ArrayElementCount { get; }
        public bool IsArray => ArrayElementCount > 0;
        public AlignmentInfo Alignment { get; }

        public FieldDefinition(
            string name,
            TypeReference type,
            SemanticType semanticType,
            int arrayElementCount,
            AlignmentInfo size)
        {
            Name = name;
            Type = type;
            SemanticType = semanticType;
            ArrayElementCount = arrayElementCount;
            Alignment = size;
        }

        public FieldDefinition(
            string name,
            TypeReference type,
            GeometrySemantic semanticType,
            int arrayElementCount,
            AlignmentInfo size)
        {
            Name = name;
            Type = type;
            GeometrySemantic = semanticType;
            ArrayElementCount = arrayElementCount;
            Alignment = size;
        }
    }
}
