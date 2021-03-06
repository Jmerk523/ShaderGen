﻿using Microsoft.CodeAnalysis;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;

namespace ShaderGen
{
    public static class TypeSizeCache
    {
        private static readonly IReadOnlyDictionary<string, int> s_knownSizes = new Dictionary<string, int>()
        {
            { "System.Byte", 1 },
            { "System.SByte", 1 },
            { "System.UIn16", 2 },
            { "System.Int16", 2 },
            { "System.UInt32", 4 },
            { "System.Int32", 4 },
            { "System.UInt64", 8 },
            { "System.Int64", 8 },
            { "System.Single", 4 },
            { "System.Double", 8 },
        };

        private static readonly IReadOnlyDictionary<string, int> s_shaderAlignments = new Dictionary<string, int>()
        {
            { "System.Numerics.Vector2", 8 },
            { "System.Numerics.Vector3", 16 },
            { "System.Numerics.Vector4", 16 },
            { "System.Numerics.Matrix4x4", 16 },
            { "ShaderGen.UInt2", 8 },
            { "ShaderGen.UInt3", 16 },
            { "ShaderGen.UInt4", 16 },
            { "ShaderGen.Int2", 8 },
            { "ShaderGen.Int3", 16 },
            { "ShaderGen.Int4", 16 },
        };

        private static readonly ConcurrentDictionary<ITypeSymbol, AlignmentInfo> s_cachedSizes = new ConcurrentDictionary<ITypeSymbol, AlignmentInfo>();

        public static AlignmentInfo Get(ITypeSymbol symbol)
        {
            Debug.Assert(symbol.Kind != SymbolKind.ArrayType);
            return s_cachedSizes.TryGetValue(symbol, out AlignmentInfo alignmentInfo)
                ? alignmentInfo
                : Analyze(symbol);
        }

        private static System.Reflection.BindingFlags internalGetProperty
            = System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.GetProperty;

        private static int GetOffset(IFieldSymbol field)
        {
            var symbol = field.GetType().InvokeMember("UnderlyingSymbol",
                internalGetProperty, null, field, null);
            var offset = symbol.GetType().InvokeMember("TypeLayoutOffset",
                internalGetProperty, null, symbol, null);
            return (offset as int?) ?? 0;
        }

        private static int GetSize(ITypeSymbol type)
        {
            var symbol = type.GetType().InvokeMember("UnderlyingSymbol",
                internalGetProperty, null, type, null);
            var layout = symbol.GetType().InvokeMember("Layout",
                internalGetProperty, null, symbol, null);
            return (int)layout.GetType().InvokeMember("Size",
                internalGetProperty | System.Reflection.BindingFlags.Public, null, layout, null);
        }

        private static int Align(int value, int alignment)
        {
            int mult = Math.DivRem(value, alignment, out var rem);
            if (rem > 0)
            {
                return (mult + 1) * alignment;
            }
            return value;
        }

        private static AlignmentInfo Analyze(ITypeSymbol typeSymbol)
        {
            // Check if we already know this type
            if (s_cachedSizes.TryGetValue(typeSymbol, out AlignmentInfo alignmentInfo))
            {
                return alignmentInfo;
            }

            string symbolFullName = typeSymbol.GetFullMetadataName();

            // Get any specific shader alignment
            int? specificShaderAlignment = s_shaderAlignments.TryGetValue(symbolFullName, out int sa)
                ? (int?)sa
                : null;

            // Check if this in our list of known sizes
            if (s_knownSizes.TryGetValue(symbolFullName, out int knownSize))
            {
                alignmentInfo = new AlignmentInfo(knownSize, knownSize, knownSize, specificShaderAlignment ?? knownSize);
                s_cachedSizes.TryAdd(typeSymbol, alignmentInfo);
                return alignmentInfo;
            }

            // Check if enum
            if (typeSymbol.TypeKind == TypeKind.Enum)
            {
                string enumBaseType = ((INamedTypeSymbol)typeSymbol).EnumUnderlyingType.GetFullMetadataName();
                if (!s_knownSizes.TryGetValue(enumBaseType, out int enumSize))
                {
                    throw new ShaderGenerationException($"Unknown enum base type: {enumBaseType}");
                }

                alignmentInfo = new AlignmentInfo(enumSize, enumSize, enumSize, specificShaderAlignment ?? enumSize);
                s_cachedSizes.TryAdd(typeSymbol, alignmentInfo);
                return alignmentInfo;
            }

            // NOTE This check only works for known types accessible to ShaderGen, but it will pick up most non-blittable types.
            if (BlittableHelper.IsBlittable(symbolFullName) == false)
            {
                throw new ShaderGenerationException($"Cannot use the {symbolFullName} type in a shader as it is not a blittable type.");
            }

            // Unknown type, get the instance fields.
            var fields = typeSymbol.GetMembers()
                .Where(symb => symb.Kind == SymbolKind.Field && !symb.IsStatic)
                .Select(symb => (IFieldSymbol)symb)
                .ToArray();

            if (fields.Length == 0)
            {
                throw new ShaderGenerationException($"No fields on type {symbolFullName}, cannot assess size of structure.");
            }

            int csharpSize = 0;
            int shaderSize = 0;
            int csharpAlignment = 0;
            int shaderAlignment = 0;

            // Calculate size of struct from its fields alignment infos
            foreach (IFieldSymbol field in fields)
            {
                // Determine if type is blittable
                if (field.Type is IArrayTypeSymbol arrayType)
                {
                    // We can only analyze array size as fields since the field has the attribute, not the type
                    var elementSizeAndAlignment = Get(arrayType.ElementType);
                    var arraySizeAttribute = field.GetAttributes().Single(a => a.AttributeClass.Name == nameof(ArraySizeAttribute));
                    var arraySize = (int)arraySizeAttribute.ConstructorArguments[0].Value;
                    var shaderAlign = Align(elementSizeAndAlignment.ShaderAlignment, 16);
                    alignmentInfo = new AlignmentInfo(
                        elementSizeAndAlignment.CSharpSize * arraySize,
                        Align(elementSizeAndAlignment.ShaderSize * arraySize, shaderAlign),
                        elementSizeAndAlignment.CSharpAlignment,
                        shaderAlign);
                }
                else
                {
                    alignmentInfo = Analyze(field.Type);
                }

                // If specified, the field offset dictates the size of the csharp structure at the point where the field starts
                csharpAlignment = Math.Max(csharpAlignment, alignmentInfo.CSharpAlignment);
                var offset = GetOffset(field);
                if (offset > 0)
                {
                    csharpSize = offset;
                }
                else
                {
                    csharpSize = Align(csharpSize, alignmentInfo.CSharpAlignment);
                }
                csharpSize += alignmentInfo.CSharpSize;

                shaderAlignment = Math.Max(shaderAlignment, alignmentInfo.ShaderAlignment);
                shaderSize = Align(shaderSize, alignmentInfo.ShaderAlignment);
                shaderSize += alignmentInfo.ShaderSize;
            }

            // Structures always align to a multiple of 16
            if (typeSymbol.TypeKind == TypeKind.Struct)
            {
                shaderAlignment = Align(specificShaderAlignment ?? shaderAlignment, 16);
            }
            else
            {
                shaderAlignment = specificShaderAlignment ?? shaderAlignment;
            }

            // The overall shader object size is always a multiple of its alignment
            shaderSize = Align(shaderSize, shaderAlignment);

            // If specified, the overall csharp object size is fixed
            var sizeOf = GetSize(typeSymbol);
            if (sizeOf > 0)
            {
                csharpSize = sizeOf;
            }

            // Return new alignment info after adding into cache.
            alignmentInfo = new AlignmentInfo(csharpSize, shaderSize, csharpAlignment, shaderAlignment);
            s_cachedSizes.TryAdd(typeSymbol, alignmentInfo);
            return alignmentInfo;
        }
    }
}
