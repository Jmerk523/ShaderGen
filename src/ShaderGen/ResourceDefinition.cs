using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;

namespace ShaderGen
{
    public class ResourceDefinition
    {
        public string Name { get; }
        public int Set { get; }
        public int Binding { get; }
        public TypeReference ValueType { get; }
        public ShaderResourceKind ResourceKind { get; internal set; }
        public BuiltinSemantic Semantic { get; internal set; }

        public ResourceDefinition(string name, int set, int binding, TypeReference valueType, ShaderResourceKind kind)
        {
            Name = name;
            Set = set;
            Binding = binding;
            ValueType = valueType;
            ResourceKind = kind;
        }

        public bool Matches(StructureDefinition sd)
        {
            if (ValueType.FixedSize > 0 && ValueType.TypeInfo is IArrayTypeSymbol array)
            {
                return SymbolEqualityComparer.Default.Equals(sd.Type, array.ElementType);
            }
            else
            {
                return SymbolEqualityComparer.Default.Equals(sd.Type, ValueType.TypeInfo);
            }
        }

        public void MarkWritten()
        {
            if (ResourceKind <= ShaderResourceKind.AtomicBuffer)
            {
                ResourceKind = ShaderResourceKind.Emit;
            }
        }
    }
}
