using System.Runtime.InteropServices.ComTypes;
using Microsoft.CodeAnalysis;

namespace ShaderGen
{
    public class TypeReference
    {
        public string Name { get; }
        public ITypeSymbol TypeInfo { get; }
        public int FixedSize { get; }

        public TypeReference(string name, ITypeSymbol typeInfo, int fixedSize = 1)
        {
            Name = name;
            TypeInfo = typeInfo;
            FixedSize = fixedSize;
        }

        public override string ToString() => Name;
    }
}
