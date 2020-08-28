using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Text;
using System.Linq;
using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using System.Diagnostics;

namespace ShaderGen
{
    internal class ShaderSyntaxWalker : CSharpSyntaxWalker
    {
        private readonly StringBuilder _sb = new StringBuilder();
        private readonly Compilation _compilation;
        private readonly LanguageBackend[] _backends;
        private readonly ShaderSetInfo _shaderSet;

        private readonly Dictionary<int, int> _setCounts = new Dictionary<int, int>();

        public ShaderSyntaxWalker(Compilation compilation, LanguageBackend[] backends, ShaderSetInfo ss)
            : base(SyntaxWalkerDepth.Token)
        {
            _compilation = compilation;
            _backends = backends;
            _shaderSet = ss;
        }

        private SemanticModel GetModel(SyntaxNode node) => _compilation.GetSemanticModel(node.SyntaxTree);

        public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            ShaderFunctionAndMethodDeclarationSyntax sfab = Utilities.GetShaderFunction(node, _compilation, true);
            foreach (LanguageBackend b in _backends)
            {
                b.AddFunction(_shaderSet.Name, sfab);

                foreach (var calledFunction in sfab.OrderedFunctionList)
                {
                    b.AddFunction(_shaderSet.Name, calledFunction);
                }
            }
        }

        public override void VisitStructDeclaration(StructDeclarationSyntax node)
        {
            TryGetStructDefinition(GetModel(node), node, out var sd);
            foreach (var b in _backends) { b.AddStructure(_shaderSet.Name, sd); }
        }

        public static bool TryGetStructDefinition(SemanticModel model, StructDeclarationSyntax node, out StructureDefinition sd)
        {
            //string fullNestedTypePrefix = Utilities.GetFullNestedTypePrefix(node, out bool nested);
            //string structName = node.Identifier.ToFullString().Trim();
            //if (!string.IsNullOrEmpty(fullNestedTypePrefix))
            //{
            //    string joiner = nested ? "+" : ".";
            //    structName = fullNestedTypePrefix + joiner + structName;
            //}

            int structCSharpSize = 0;
            int structShaderSize = 0;
            int structCSharpAlignment = 0;
            int structShaderAlignment = 0;
            List<FieldDefinition> fields = new List<FieldDefinition>();
            foreach (MemberDeclarationSyntax member in node.Members)
            {
                if (member is FieldDeclarationSyntax fds && !fds.Modifiers.Any(x => x.IsKind(SyntaxKind.ConstKeyword)))
                {
                    VariableDeclarationSyntax varDecl = fds.Declaration;
                    foreach (VariableDeclaratorSyntax vds in varDecl.Variables)
                    {
                        string fieldName = vds.Identifier.Text.Trim();
                        string typeName = model.GetFullTypeName(varDecl.Type, out bool isArray);
                        int arrayElementCount = 0;
                        if (isArray)
                        {
                            arrayElementCount = GetArrayCountValue(vds, model);
                        }

                        TypeInfo typeInfo = model.GetTypeInfo(varDecl.Type);

                        AlignmentInfo fieldSizeAndAlignment;

                        int fixedSize = 1;
                        if (typeInfo.Type.Kind == SymbolKind.ArrayType)
                        {
                            var arraySize = fds.DescendantNodes().OfType<AttributeSyntax>().FirstOrDefault(
                                attrSyntax => attrSyntax.Name.ToString().EndsWith("ArraySize"));
                            fixedSize = (int)model.GetConstantValue(arraySize.ArgumentList.Arguments.First().Expression).Value;
                            ITypeSymbol elementType = ((IArrayTypeSymbol)typeInfo.Type).ElementType;
                            AlignmentInfo elementSizeAndAlignment = TypeSizeCache.Get(elementType);
                            fieldSizeAndAlignment = new AlignmentInfo(
                                elementSizeAndAlignment.CSharpSize * arrayElementCount,
                                elementSizeAndAlignment.ShaderSize * arrayElementCount,
                                elementSizeAndAlignment.CSharpAlignment,
                                elementSizeAndAlignment.ShaderAlignment);
                        }
                        else
                        {
                            fieldSizeAndAlignment = TypeSizeCache.Get(typeInfo.Type);
                        }

                        structCSharpSize += structCSharpSize % fieldSizeAndAlignment.CSharpAlignment;
                        structCSharpSize += fieldSizeAndAlignment.CSharpSize;
                        structCSharpAlignment = Math.Max(structCSharpAlignment, fieldSizeAndAlignment.CSharpAlignment);

                        structShaderSize += structShaderSize % fieldSizeAndAlignment.ShaderAlignment;
                        structShaderSize += fieldSizeAndAlignment.ShaderSize;
                        structShaderAlignment = Math.Max(structShaderAlignment, fieldSizeAndAlignment.ShaderAlignment);

                        TypeReference tr = new TypeReference(typeName, model.GetTypeInfo(varDecl.Type).Type, fixedSize);
                        SemanticType semanticType = GetSemanticType(vds);
                        if (semanticType == SemanticType.None)
                        {
                            var geometrySemantic = GetGeometrySemantic(vds);
                            if (geometrySemantic != GeometrySemantic.None)
                            {
                                fields.Add(new FieldDefinition(fieldName, tr, geometrySemantic, arrayElementCount, fieldSizeAndAlignment));
                                continue;
                            }
                        }

                        fields.Add(new FieldDefinition(fieldName, tr, semanticType, arrayElementCount, fieldSizeAndAlignment));
                    }
                }
            }

            var type = model.GetDeclaredSymbol(node);
            sd = new StructureDefinition(
                type,
                fields.ToArray(),
                new AlignmentInfo(structCSharpSize, structShaderSize, structCSharpAlignment, structShaderAlignment));
            return true;
        }

        private static int GetArrayCountValue(VariableDeclaratorSyntax vds, SemanticModel semanticModel)
        {
            AttributeSyntax[] arraySizeAttrs = Utilities.GetMemberAttributes(vds, "ArraySize");
            if (arraySizeAttrs.Length != 1)
            {
                throw new ShaderGenerationException(
                    "Array fields in structs must have a constant size specified by an ArraySizeAttribute.");
            }
            AttributeSyntax arraySizeAttr = arraySizeAttrs[0];
            return GetAttributeArgumentIntValue(arraySizeAttr, 0, semanticModel);
        }

        private static int GetAttributeArgumentIntValue(AttributeSyntax attr, int index, SemanticModel semanticModel)
        {
            if (attr.ArgumentList.Arguments.Count < index + 1)
            {
                throw new ShaderGenerationException(
                    "Too few arguments in attribute " + attr.ToFullString() + ". Required + " + (index + 1));
            }
            return GetConstantIntFromExpression(attr.ArgumentList.Arguments[index].Expression, semanticModel);
        }

        private static int GetConstantIntFromExpression(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            var constantValue = semanticModel.GetConstantValue(expression);
            if (!constantValue.HasValue)
            {
                throw new ShaderGenerationException("Expression did not contain a constant value: " + expression.ToFullString());
            }
            return (int)constantValue.Value;
        }

        private static SemanticType GetSemanticType(VariableDeclaratorSyntax vds)
        {
            AttributeSyntax[] attrs = Utilities.GetMemberAttributes(vds, "VertexSemantic");
            if (attrs.Length == 1)
            {
                AttributeSyntax semanticTypeAttr = attrs[0];
                string fullArg0 = semanticTypeAttr.ArgumentList.Arguments[0].ToFullString();
                if (fullArg0.Contains("."))
                {
                    fullArg0 = fullArg0.Substring(fullArg0.LastIndexOf('.') + 1);
                }
                if (Enum.TryParse(fullArg0, out SemanticType ret))
                {
                    return ret;
                }
                else
                {
                    throw new ShaderGenerationException("Incorrectly formatted attribute: " + semanticTypeAttr.ToFullString());
                }
            }
            else if (attrs.Length > 1)
            {
                throw new ShaderGenerationException("Too many vertex semantics applied to field: " + vds.ToFullString());
            }

            if (CheckSingleAttribute(vds, "SystemPositionSemantic"))
            {
                return SemanticType.SystemPosition;
            }
            else if (CheckSingleAttribute(vds, "PositionSemantic"))
            {
                return SemanticType.Position;
            }
            else if (CheckSingleAttribute(vds, "NormalSemantic"))
            {
                return SemanticType.Normal;
            }
            else if (CheckSingleAttribute(vds, "TextureCoordinateSemantic"))
            {
                return SemanticType.TextureCoordinate;
            }
            else if (CheckSingleAttribute(vds, "ColorSemantic"))
            {
                return SemanticType.Color;
            }
            else if (CheckSingleAttribute(vds, "TangentSemantic"))
            {
                return SemanticType.Tangent;
            }
            else if (CheckSingleAttribute(vds, "ColorTargetSemantic"))
            {
                return SemanticType.ColorTarget;
            }

            return SemanticType.None;
        }

        private static GeometrySemantic GetGeometrySemantic(VariableDeclaratorSyntax vds)
        {
            AttributeSyntax[] attrs = Utilities.GetMemberAttributes(vds, "GeometrySemantic");
            if (attrs.Length == 1)
            {
                AttributeSyntax semanticTypeAttr = attrs[0];
                string fullArg0 = semanticTypeAttr.ArgumentList.Arguments[0].ToFullString();
                if (fullArg0.Contains("."))
                {
                    fullArg0 = fullArg0.Substring(fullArg0.LastIndexOf('.') + 1);
                }
                if (Enum.TryParse(fullArg0, out GeometrySemantic ret))
                {
                    return ret;
                }
                else
                {
                    throw new ShaderGenerationException("Incorrectly formatted attribute: " + semanticTypeAttr.ToFullString());
                }
            }
            else if (attrs.Length > 1)
            {
                throw new ShaderGenerationException("Too many geometry semantics applied to field: " + vds.ToFullString());
            }

            return GeometrySemantic.None;
        }

        private static bool CheckSingleAttribute(VariableDeclaratorSyntax vds, string name)
        {
            AttributeSyntax[] attrs = Utilities.GetMemberAttributes(vds, name);
            return attrs.Length == 1;
        }

        public override void VisitFieldDeclaration(FieldDeclarationSyntax node)
        {
            if (node.Modifiers.Any(x => x.IsKind(SyntaxKind.ConstKeyword)))
            {
                return;
            }

            base.VisitFieldDeclaration(node);
        }

        public override void VisitVariableDeclaration(VariableDeclarationSyntax node)
        {
            if (node.Variables.Count != 1)
            {
                throw new ShaderGenerationException("Cannot declare multiple variables together.");
            }

            VariableDeclaratorSyntax vds = node.Variables[0];

            string resourceName = vds.Identifier.Text;
            TypeInfo typeInfo = GetModel(node).GetTypeInfo(node.Type);
            string fullTypeName = GetModel(node).GetFullTypeName(node.Type);
            TypeReference valueType = new TypeReference(fullTypeName, typeInfo.Type);
            ShaderResourceKind kind = ClassifyResourceKind(fullTypeName);

            var structure = _backends.Select(b => b.GetContext(_shaderSet.Name).Structures
                .FirstOrDefault(s => SymbolEqualityComparer.Default.Equals(s.Type, typeInfo.Type)))
                .FirstOrDefault();
            if (structure != null && structure.Fields.Any(f => f.IsBuiltIn))
            {
                kind = ShaderResourceKind.BuiltIn;
            }

            ShaderBuiltin builtin = default;
            if (kind == ShaderResourceKind.Uniform)
            {
                if (node.Parent is FieldDeclarationSyntax field)
                {
                    if (field.Modifiers.Any(f => f.IsKind(SyntaxKind.PrivateKeyword)))
                    {
                        kind = ShaderResourceKind.Local;
                    }
                    else if (field.AttributeLists.Any(l => l.Attributes.Any(a => a.Name.ToString().Contains("Semantic"))))
                    {
                        kind = ShaderResourceKind.BuiltIn;
                        SemanticType semanticType = GetSemanticType(vds);
                        if (semanticType == SemanticType.None)
                        {
                            var geometrySemantic = GetGeometrySemantic(vds);
                            if (geometrySemantic != GeometrySemantic.None)
                            {
                                builtin = new ShaderBuiltin(geometrySemantic);
                            }
                        }
                        else
                        {
                            builtin = new ShaderBuiltin(semanticType);
                        }
                    }
                }
                else if (typeInfo.Type.GetAttributes().Any(a => a.AttributeClass.Name.Contains("Semantic")))
                {
                    kind = ShaderResourceKind.BuiltIn;
                }
            }

            if (kind == ShaderResourceKind.StructuredBuffer
                || kind == ShaderResourceKind.RWStructuredBuffer
                || kind == ShaderResourceKind.RWTexture2D
                || valueType.Name.Contains(nameof(UniformBuffer<int>)))
            {
                valueType = ParseElementType(vds);
            }

            if (node.Parent is FieldDeclarationSyntax)
            {
                var arraySize = node.Parent.DescendantNodes().OfType<AttributeSyntax>().FirstOrDefault(
                    attrSyntax => attrSyntax.Name.ToString().EndsWith("ArraySize"));
                if (arraySize != null)
                {
                    var fixedSize = (int)GetModel(node).GetConstantValue(arraySize.ArgumentList.Arguments.First().Expression).Value;
                    valueType = new TypeReference(valueType.Name, valueType.TypeInfo, fixedSize);
                }

                var emitAttribute = node.Parent.DescendantNodes().OfType<AttributeSyntax>().FirstOrDefault(
                    attrSyntax => attrSyntax.Name.ToString().EndsWith("EmitVertex"));
                if (emitAttribute != null)
                {
                    kind = ShaderResourceKind.Emit;
                }
            }

            ResourceDefinition rd;
            if (kind == ShaderResourceKind.Local)
            {
                rd = new ResourceDefinition(resourceName, 0, 0, valueType, kind);
            }
            else
            {
                int set = 0; // Default value if not otherwise specified.
                if (GetResourceDecl(node, out AttributeSyntax resourceSetDecl))
                {
                    set = GetAttributeArgumentIntValue(resourceSetDecl, 0, GetModel(node));
                }

                int resourceBinding = GetAndIncrementBinding(set);

                if (kind == ShaderResourceKind.Uniform)
                {
                    ValidateUniformType(typeInfo);
                }
                rd = new ResourceDefinition(resourceName, set, resourceBinding, valueType, kind);
            }

            rd.Semantic = builtin.Semantic;

            foreach (LanguageBackend b in _backends) { b.AddResource(_shaderSet.Name, rd); }
        }

        private TypeReference ParseElementType(VariableDeclaratorSyntax vds)
        {
            FieldDeclarationSyntax fieldDecl = (FieldDeclarationSyntax)vds.Parent.Parent;
            TypeSyntax fieldType = fieldDecl.Declaration.Type;
            while (fieldType is QualifiedNameSyntax qns)
            {
                fieldType = qns.Right;
            }

            GenericNameSyntax gns = (GenericNameSyntax)fieldType;
            TypeSyntax type = gns.TypeArgumentList.Arguments[0];
            string fullName = GetModel(vds).GetFullTypeName(type);
            return new TypeReference(fullName, GetModel(vds).GetTypeInfo(type).Type);
        }

        private int GetAndIncrementBinding(int set)
        {
            if (!_setCounts.TryGetValue(set, out int ret))
            {
                ret = 0;
                _setCounts.Add(set, ret);
            }
            else
            {
                ret += 1;
                _setCounts[set] = ret;
            }

            return ret;
        }

        private void ValidateUniformType(TypeInfo typeInfo)
        {
            string name = typeInfo.Type.ToDisplayString();
            if (name != nameof(ShaderGen) + "." + nameof(Texture1DResource)
                && name != nameof(ShaderGen) + "." + nameof(Texture2DResource)
                && name != nameof(ShaderGen) + "." + nameof(Texture3DResource)
                && name != nameof(ShaderGen) + "." + nameof(Texture2DArrayResource)
                && name != nameof(ShaderGen) + "." + nameof(Texture2DRectResource)
                && name != nameof(ShaderGen) + "." + nameof(Texture2DMSResource)
                && name != nameof(ShaderGen) + "." + nameof(TextureCubeResource)
                && name != nameof(ShaderGen) + "." + nameof(TextureBufferResource)
                && name != nameof(ShaderGen) + "." + nameof(SamplerResource)
                && name != nameof(ShaderGen) + "." + nameof(SamplerComparisonResource)
                && !name.StartsWith(nameof(ShaderGen) + "." + nameof(UniformBuffer<int>)))
            {
                var elementType = typeInfo.Type;
                if (typeInfo.Type is IArrayTypeSymbol arrayType)
                {
                    elementType = arrayType.ElementType;
                }
                if (elementType.IsReferenceType && !ShaderPrimitiveTypes.IsPrimitiveType(name))
                {
                    throw new ShaderGenerationException("Shader resource fields must be simple blittable structures.");
                }
            }
        }

        internal static ShaderResourceKind ClassifyResourceKind(string fullTypeName)
        {
            if (fullTypeName == "ShaderGen.Texture1DResource")
            {
                return ShaderResourceKind.Texture1D;
            }
            else if (fullTypeName == "ShaderGen.Texture2DResource")
            {
                return ShaderResourceKind.Texture2D;
            }
            else if (fullTypeName == "ShaderGen.Texture3DResource")
            {
                return ShaderResourceKind.Texture3D;
            }
            else if (fullTypeName == "ShaderGen.Texture2DArrayResource")
            {
                return ShaderResourceKind.Texture2DArray;
            }
            else if (fullTypeName == "ShaderGen.Texture2DRectResource")
            {
                return ShaderResourceKind.Texture2DRect;
            }
            else if (fullTypeName == "ShaderGen.TextureCubeResource")
            {
                return ShaderResourceKind.TextureCube;
            }
            else if (fullTypeName == "ShaderGen.Texture2DMSResource")
            {
                return ShaderResourceKind.Texture2DMS;
            }
            else if (fullTypeName == "ShaderGen.TextureBufferResource")
            {
                return ShaderResourceKind.TextureBuffer;
            }
            else if (fullTypeName == "ShaderGen.SamplerResource")
            {
                return ShaderResourceKind.Sampler;
            }
            else if (fullTypeName == "ShaderGen.SamplerComparisonResource")
            {
                return ShaderResourceKind.SamplerComparison;
            }
            else if (fullTypeName.Contains("ShaderGen.RWStructuredBuffer"))
            {
                return ShaderResourceKind.RWStructuredBuffer;
            }
            else if (fullTypeName.Contains("ShaderGen.StructuredBuffer"))
            {
                return ShaderResourceKind.StructuredBuffer;
            }
            else if (fullTypeName.Contains("ShaderGen.RWTexture2DResource"))
            {
                return ShaderResourceKind.RWTexture2D;
            }
            else if (fullTypeName.Contains("ShaderGen.DepthTexture2DResource"))
            {
                return ShaderResourceKind.DepthTexture2D;
            }
            else if (fullTypeName.Contains("ShaderGen.DepthTexture2DArrayResource"))
            {
                return ShaderResourceKind.DepthTexture2DArray;
            }
            else if (fullTypeName.Contains("ShaderGen.AtomicBuffer"))
            {
                return ShaderResourceKind.AtomicBuffer;
            }
            else
            {
                return ShaderResourceKind.Uniform;
            }
        }

        private bool GetResourceDecl(VariableDeclarationSyntax node, out AttributeSyntax attr)
        {
            attr = (node.Parent.DescendantNodes().OfType<AttributeSyntax>().FirstOrDefault(
                attrSyntax => attrSyntax.ToString().Contains("Resource")));
            return attr != null;
        }
    }
}
