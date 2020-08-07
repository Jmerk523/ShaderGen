using System;

namespace ShaderGen
{
    [AttributeUsage(AttributeTargets.Assembly, AllowMultiple = true)]
    public class ShaderSetAttribute : Attribute
    {
        public string Name { get; }
        public string VertexShader { get; }
        public string GeometryShader { get; }
        public string FragmentShader { get; }

        public ShaderSetAttribute(string name, string vs, string fs)
        {
            Name = name;
            VertexShader = vs;
            FragmentShader = fs;
        }

        public ShaderSetAttribute(string name, string vs, string gs, string fs)
            : this(name, vs, fs)
        {
            GeometryShader = gs;
        }
    }
}
