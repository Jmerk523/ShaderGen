namespace ShaderGen
{
    /// <summary>
    /// Represents the concrete output of a shader generation for a specific shader set,
    /// created using a specific LanguageBackend.
    /// </summary>
    public class GeneratedShaderSet
    {
        public string Name { get; }
        public string VertexShaderCode { get; }
        public string GeometryShaderCode { get; }
        public string FragmentShaderCode { get; }
        public string ComputeShaderCode { get; }
        public ShaderFunction VertexFunction { get; }
        public ShaderFunction GeometryFunction { get; }
        public ShaderFunction FragmentFunction { get; }
        public ShaderFunction ComputeFunction { get; }
        public ShaderModel Model { get; }

        public GeneratedShaderSet(
            string name,
            string vsCode,
            string gsCode,
            string fsCode,
            string csCode,
            ShaderFunction vertexfunction,
            ShaderFunction geometryFunction,
            ShaderFunction fragmentFunction,
            ShaderFunction computeFunction,
            ShaderModel model)
        {
            if (string.IsNullOrEmpty(vsCode) && string.IsNullOrEmpty(gsCode) && string.IsNullOrEmpty(fsCode) && string.IsNullOrEmpty(csCode))
            {
                throw new ShaderGenerationException("At least one of vsCode, fsCode, or csCode must be non-empty");
            }

            Name = name;
            VertexShaderCode = vsCode;
            GeometryShaderCode = gsCode;
            FragmentShaderCode = fsCode;
            ComputeShaderCode = csCode;
            VertexFunction = vertexfunction;
            GeometryFunction = geometryFunction;
            FragmentFunction = fragmentFunction;
            ComputeFunction = computeFunction;
            Model = model;
        }
    }
}
