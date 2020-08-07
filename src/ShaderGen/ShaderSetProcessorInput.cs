namespace ShaderGen
{
    public class ShaderSetProcessorInput
    {
        public string SetName { get; }
        public ShaderFunction VertexFunction { get; }
        public ShaderFunction GeometryFunction { get; }
        public ShaderFunction FragmentFunction { get; }
        public ShaderModel Model { get; }

        public ShaderSetProcessorInput(
            string name,
            ShaderFunction vertexFunction,
            ShaderFunction fragmentFunction,
            ShaderModel model)
            : this(name, vertexFunction, null, fragmentFunction, model)
        {
        }

        public ShaderSetProcessorInput(
            string name,
            ShaderFunction vertexFunction,
            ShaderFunction geometryFunction,
            ShaderFunction fragmentFunction,
            ShaderModel model)
        {
            SetName = name;
            VertexFunction = vertexFunction;
            GeometryFunction = geometryFunction;
            FragmentFunction = fragmentFunction;
            Model = model;
        }
    }
}
