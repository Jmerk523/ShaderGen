using System;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace ShaderGen
{
    public class StructureDefinition
    {
        public ITypeSymbol Type { get; }
        public string Name { get; }
        public FieldDefinition[] Fields { get; }
        public AlignmentInfo Alignment { get; }
        public bool CSharpMatchesShaderAlignment { get; }

        public StructureDefinition(ITypeSymbol type, FieldDefinition[] fields, AlignmentInfo? size = null)
        {
            Type = type ?? throw new ArgumentNullException(nameof(type));
            Name = type.GetFullMetadataName();
            Fields = fields;
            Alignment = size ?? TypeSizeCache.Get(type);
            if (Alignment.CSharpSize == Alignment.ShaderSize)
            {
                CSharpMatchesShaderAlignment = true;
            }
            else
            {
                CSharpMatchesShaderAlignment = GetCSharpMatchesShaderAlignment();
            }
        }

        private bool GetCSharpMatchesShaderAlignment()
        {
            int csharpOffset = 0;
            int shaderOffset = 0;
            for (int i = 0; i < Fields.Length; i++)
            {
                if (!CheckAlignments(Fields[i], ref csharpOffset, ref shaderOffset))
                {
                    return false;
                }
            }

            return true;
        }

        private bool CheckAlignments(FieldDefinition fd, ref int cs, ref int shader)
        {
            if ((cs % fd.Alignment.CSharpAlignment) != 0 || (shader % fd.Alignment.ShaderAlignment) != 0)
            {
                return false;
            }

            cs += fd.Alignment.CSharpSize;
            shader += fd.Alignment.CSharpSize;
            return cs == shader;
        }
    }
}
