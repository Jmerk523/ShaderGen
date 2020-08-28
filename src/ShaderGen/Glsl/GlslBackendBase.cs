using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;

namespace ShaderGen.Glsl
{
    public abstract class GlslBackendBase : LanguageBackend
    {
        protected readonly HashSet<string> _uniformNames = new HashSet<string>();
        protected readonly HashSet<string> _ssboNames = new HashSet<string>();

        public GlslBackendBase(Compilation compilation) : base(compilation)
        {
        }

        protected void WriteStructure(ShaderFunction function, StringBuilder sb, StructureDefinition sd, ResourceDefinition resource)
        {
            if (resource != null)
            {
                if (resource.ResourceKind == ShaderResourceKind.Emit)
                {
                    sb.Append("out ");
                }
                else
                {
                    sb.Append("in ");
                }
                if (resource.ResourceKind == ShaderResourceKind.BuiltIn)
                {
                    sb.AppendLine(SemanticIdentifier(new ShaderBuiltin(function.Type.Stage(), resource.Semantic)));
                }
                else
                {
                    sb.AppendLine(CSharpToShaderType(sd.Name));
                }
            }
            else
            {
                sb.AppendLine($"struct {CSharpToShaderType(sd.Name)}");
            }
            sb.AppendLine("{");
            StringBuilder fb = new StringBuilder();
            foreach (FieldDefinition field in sd.Fields)
            {
                string fieldTypeStr = GetStructureFieldType(field);
                fb.Append(fieldTypeStr);
                fb.Append(' ');
                fb.Append(CorrectIdentifier(field.Name.Trim()));
                int arrayCount = field.ArrayElementCount;
                if (arrayCount > 0)
                {
                    fb.Append('['); fb.Append(arrayCount); fb.Append(']');
                }
                fb.Append(';');
                sb.Append("    ");
                sb.AppendLine(fb.ToString());
                fb.Clear();
            }
            sb.Append("}");
            if (resource != null)
            {
                sb.Append(" ");
                sb.Append(CorrectIdentifierName(resource.Name.Trim()));
                if (resource.ValueType.FixedSize > 0)
                {
                    sb.Append('[');
                    sb.Append(resource.ValueType.FixedSize);
                    sb.Append(']');
                }
            }
            sb.AppendLine(";");
            sb.AppendLine();
        }

        protected virtual string GetStructureFieldType(FieldDefinition field)
        {
            return CSharpToShaderType(field.Type);
        }

        protected virtual string InputPrimitiveIdentifier(PrimitiveType primitive)
        {
            switch (primitive)
            {
                case PrimitiveType.Points:
                    return "points";
                case PrimitiveType.Lines:
                case PrimitiveType.LineStrip:
                case PrimitiveType.LineList:
                    return "lines";
                case PrimitiveType.LinesAdjacency:
                case PrimitiveType.LineStripAdjacency:
                    return "lines_adjacency";
                case PrimitiveType.Triangles:
                case PrimitiveType.TriangleStrip:
                case PrimitiveType.TriangleFan:
                    return "triangles";
                case PrimitiveType.TrianglesAdjacency:
                case PrimitiveType.TriangleStripAdjacency:
                    return "triangles_adjacency";
                default:
                    throw new ArgumentException(primitive.ToString());
            }
        }

        protected virtual string OutputPrimitiveIdentifier(PrimitiveType primitive)
        {
            switch (primitive)
            {
                case PrimitiveType.Points:
                    return "points";
                case PrimitiveType.LineStrip:
                    return "line_strip";
                case PrimitiveType.TriangleStrip:
                    return "triangle_strip";
                default:
                    throw new ArgumentException(primitive.ToString());
            }
        }

        protected override MethodProcessResult GenerateFullTextCore(string setName, ShaderFunction function)
        {
            BackendContext context = GetContext(setName);
            StringBuilder sb = new StringBuilder();
            var writtenResources = new HashSet<ResourceDefinition>();
            int outVarIndex = 0;

            ShaderFunctionAndMethodDeclarationSyntax entryPoint = context.Functions.SingleOrDefault(
                sfabs => sfabs.Function.Name == function.Name);
            if (entryPoint == null)
            {
                throw new ShaderGenerationException("Couldn't find given function: " + function.Name);
            }

            ValidateRequiredSemantics(setName, entryPoint.Function, function.Type);

            if (entryPoint.Function.InputPrimitive != PrimitiveType.None)
            {
                sb.AppendLine($"layout({InputPrimitiveIdentifier(entryPoint.Function.InputPrimitive)}) in;");
            }
            if (entryPoint.Function.OutputPrimitive != PrimitiveType.None)
            {
                sb.AppendLine($"layout({OutputPrimitiveIdentifier(entryPoint.Function.OutputPrimitive)}, max_vertices = {entryPoint.Function.MaxVertices}) out;");
            }

            HashSet<ResourceDefinition> resourcesUsed
                = ProcessFunctions(setName, entryPoint, out string funcStr, out string entryStr);

            ValidateResourcesUsed(setName, resourcesUsed);

            StructureDefinition[] orderedStructures
                = StructureDependencyGraph.GetOrderedStructureList(Compilation, context.Structures, resourcesUsed);

            foreach (StructureDefinition osd in orderedStructures)
            {
                if (osd.Fields.All(f => !f.IsBuiltIn))
                {
                    var resources = resourcesUsed.Concat(context.Statics)
                        .Where(r => r.Matches(osd))
                        .ToArray();
                    if (resources.Length == 1)
                    {
                        if (resources[0].ResourceKind == ShaderResourceKind.BuiltIn)
                        {
                            WriteStructure(function, sb, osd, resources[0]);
                            //foreach (var rs in resources.Where(r => r.ResourceKind == ShaderResourceKind.BuiltIn))
                            //{
                            //    WriteStructure(function, sb, osd, rs);
                            //}
                            writtenResources.Add(resources[0]);
                            continue;
                        }
                        else if (resources[0].ResourceKind == ShaderResourceKind.Emit)
                        {
                            outVarIndex = 0;
                            foreach (var field in osd.Fields)
                            {
                                WriteInOutVariable(
                                    sb,
                                    false,
                                    function.Type,
                                    CSharpToShaderType(field.Type.Name),
                                    "out_" + CorrectIdentifier(field.Name),
                                    outVarIndex);
                                outVarIndex += 1;
                            }
                            writtenResources.Add(resources[0]);
                            continue;
                        }
                    }
                    else if (resources.Length == 0)
                    {
                        //continue;
                    }
                }
                WriteStructure(function, sb, osd, null);
            }

            int structuredBufferIndex = 0;
            int rwTextureIndex = 0;
            StructureDefinition sd;
            List<FieldDefinition> outputFields = new List<FieldDefinition>();
            foreach (ResourceDefinition rd in context.Resources)
            {
                if (!resourcesUsed.Contains(rd) || writtenResources.Contains(rd))
                {
                    continue;
                }

                switch (rd.ResourceKind)
                {
                    case ShaderResourceKind.Uniform:
                        if (rd.ValueType.TypeInfo is IArrayTypeSymbol)
                        {
                            //sb.AppendLine($"#define {rd.Name}_MAX_SIZE {rd.ValueType.FixedSize}");
                        }
                        WriteUniform(sb, rd);
                        break;
                    case ShaderResourceKind.Texture1D:
                        WriteTexture1D(sb, rd);
                        break;
                    case ShaderResourceKind.Texture2D:
                        WriteTexture2D(sb, rd);
                        break;
                    case ShaderResourceKind.Texture3D:
                        WriteTexture3D(sb, rd);
                        break;
                    case ShaderResourceKind.Texture2DArray:
                        WriteTexture2DArray(sb, rd);
                        break;
                    case ShaderResourceKind.Texture2DRect:
                        WriteTexture2DRect(sb, rd);
                        break;
                    case ShaderResourceKind.TextureCube:
                        WriteTextureCube(sb, rd);
                        break;
                    case ShaderResourceKind.Texture2DMS:
                        WriteTexture2DMS(sb, rd);
                        function.UsesTexture2DMS = true;
                        break;
                    case ShaderResourceKind.TextureBuffer:
                        WriteTextureBuffer(sb, rd);
                        break;
                    case ShaderResourceKind.Sampler:
                        WriteSampler(sb, rd);
                        break;
                    case ShaderResourceKind.SamplerComparison:
                        WriteSamplerComparison(sb, rd);
                        break;
                    case ShaderResourceKind.StructuredBuffer:
                    case ShaderResourceKind.RWStructuredBuffer:
                    case ShaderResourceKind.AtomicBuffer:
                        WriteStructuredBuffer(sb, rd, rd.ResourceKind == ShaderResourceKind.StructuredBuffer, structuredBufferIndex);
                        structuredBufferIndex++;
                        break;
                    case ShaderResourceKind.RWTexture2D:
                        WriteRWTexture2D(sb, rd, rwTextureIndex);
                        rwTextureIndex++;
                        break;
                    case ShaderResourceKind.DepthTexture2D:
                        WriteDepthTexture2D(sb, rd);
                        break;
                    case ShaderResourceKind.DepthTexture2DArray:
                        WriteDepthTexture2DArray(sb, rd);
                        break;
                    case ShaderResourceKind.Emit:
                        sd = context.Structures.FirstOrDefault(s => rd.Matches(s));
                        if (sd == null)
                        {
                            WriteInOutVariable(
                                sb,
                                false,
                                entryPoint.Function.Type,
                                CSharpToShaderType(rd.ValueType.Name),
                                "out_" + CorrectIdentifier(rd.Name),
                                outVarIndex);
                            outVarIndex += 1;
                        }
                        else
                        {
                            //WriteStructure(function, sb, sd, rd);
                            outputFields.AddRange(sd.Fields);
                        }
                        break;
                    case ShaderResourceKind.BuiltIn:
                        sd = context.Structures.FirstOrDefault(s => rd.Matches(s));
                        if (sd != null)
                        {
                            WriteStructure(function, sb, sd, rd);
                        }
                        break;
                    default: throw new ShaderGenerationException("Illegal resource kind: " + rd.ResourceKind);
                }

                writtenResources.Add(rd);
            }

            entryPoint.Function.OutputFields = outputFields.ToArray();

            foreach (var variable in context.Statics)
            {
                WriteLocal(sb, variable);
            }

            sb.AppendLine(funcStr);
            sb.AppendLine(entryStr);

            WriteMainFunction(setName, sb, entryPoint.Function);

            // Append version last because it relies on information from parsing the shader.
            StringBuilder versionSB = new StringBuilder();
            WriteVersionHeader(function, entryPoint.OrderedFunctionList, versionSB);

            sb.Insert(0, versionSB.ToString());
            resourcesUsed.RemoveWhere(r => r.ResourceKind == ShaderResourceKind.Local);
            return new MethodProcessResult(sb.ToString(), resourcesUsed);
        }

        private void WriteMainFunction(string setName, StringBuilder sb, ShaderFunction entryFunction)
        {
            ParameterDefinition input = entryFunction.Parameters.Length > 0
                ? entryFunction.Parameters[0]
                : null;
            StructureDefinition inputType = input != null
                ? GetRequiredStructureType(setName, input.Type)
                : null;
            StructureDefinition outputType =
                entryFunction.ReturnType.Name != "System.Numerics.Vector4"
                && entryFunction.ReturnType.Name != "System.Void"
                    ? GetRequiredStructureType(setName, entryFunction.ReturnType)
                    : null;

            var builtins = new Dictionary<string, ShaderBuiltin>();

            if (inputType != null)
            {
                int inVarIndex = 0;
                // Declare "in" variables
                foreach (FieldDefinition field in inputType.Fields)
                {
                    if (field.SemanticType != SemanticType.None)
                    {
                        builtins.Add(field.Name, new ShaderBuiltin(field.SemanticType));
                    }
                    else if (field.GeometrySemantic != GeometrySemantic.None)
                    {
                        builtins.Add(field.Name, new ShaderBuiltin(field.GeometrySemantic));
                    }
                    else
                    {
                        WriteInOutVariable(
                            sb,
                            true,
                            entryFunction.Type,
                            CSharpToShaderType(field.Type.Name),
                            CorrectIdentifier(field.Name),
                            inVarIndex,
                            input.Type.FixedSize);
                        inVarIndex += 1;
                    }
                }
            }

            string mappedReturnType = CSharpToShaderType(entryFunction.ReturnType.Name);

            // Declare "out" variables
            if (entryFunction.Type == ShaderFunctionType.VertexEntryPoint)
            {
                int outVarIndex = 0;
                foreach (FieldDefinition field in outputType.Fields)
                {
                    if (field.SemanticType == SemanticType.SystemPosition)
                    {
                        continue;
                    }
                    else
                    {
                        WriteInOutVariable(
                            sb,
                            false,
                            entryFunction.Type,
                            CSharpToShaderType(field.Type.Name),
                            "out_" + CorrectIdentifier(field.Name),
                            outVarIndex);
                        outVarIndex += 1;
                    }
                }
            }
            else if (entryFunction.Type == ShaderFunctionType.GeometryEntryPoint)
            {
                int outVarIndex = 0;
                foreach (FieldDefinition field in entryFunction.OutputFields)
                {
                    WriteInOutVariable(
                        sb,
                        false,
                        entryFunction.Type,
                        CSharpToShaderType(field.Type.Name),
                        "out_" + CorrectIdentifier(field.Name),
                        outVarIndex);
                    outVarIndex += 1;
                }
            }
            else if (entryFunction.Type != ShaderFunctionType.GeometryEntryPoint)
            {
                Debug.Assert(entryFunction.Type == ShaderFunctionType.FragmentEntryPoint
                    || entryFunction.Type == ShaderFunctionType.ComputeEntryPoint);

                if (mappedReturnType == "vec4")
                {
                    WriteInOutVariable(sb, false, entryFunction.Type, "vec4", "_outputColor_", 0);
                }
                else if (mappedReturnType != "void")
                {
                    // Composite struct -- declare an out variable for each.
                    int colorTargetIndex = 0;
                    foreach (FieldDefinition field in outputType.Fields)
                    {
                        Debug.Assert(field.SemanticType == SemanticType.ColorTarget);
                        Debug.Assert(field.Type.Name == "System.Numerics.Vector4");
                        int index = colorTargetIndex++;
                        sb.AppendLine($"    layout(location = {index}) out vec4 _outputColor_{index};");
                    }
                }
            }

            if (entryFunction.Name == "main" && input == null && outputType == null)
            {
                return;
            }

            sb.AppendLine();

            sb.AppendLine($"void main()");
            sb.AppendLine("{");
            if (inputType != null)
            {
                string inTypeName = CSharpToShaderType(inputType.Name);
                sb.Append($"    {inTypeName} {CorrectIdentifier("input")}");
                if (input.Type.FixedSize > 0)
                {
                    sb.Append('[');
                    sb.Append(input.Type.FixedSize);
                    sb.Append(']');
                }
                sb.AppendLine(";");

                void assignInVars(int? index)
                {
                    // Assign synthetic "in" variables (with real field name) to structure passed to actual function.
                    int inoutIndex = 0;
                    bool foundSystemPosition = false;
                    foreach (FieldDefinition field in inputType.Fields)
                    {
                        string indexer = String.Empty;
                        if (index.HasValue)
                        {
                            indexer = $"[{index.Value}]";
                        }
                        if (builtins.TryGetValue(field.Name, out var builtin))
                        {
                            sb.AppendLine($"    {CorrectIdentifier("input")}{indexer}.{CorrectIdentifier(field.Name)} = {SemanticIdentifier(builtin)}{indexer};");
                        }
                        else if (entryFunction.Type == ShaderFunctionType.VertexEntryPoint)
                        {
                            sb.AppendLine($"    {CorrectIdentifier("input")}{indexer}.{CorrectIdentifier(field.Name)} = {CorrectIdentifier(field.Name)}{indexer};");
                        }
                        else
                        {
                            if (field.SemanticType == SemanticType.SystemPosition && !foundSystemPosition)
                            {
                                Debug.Fail("System position should be handled by a builtin");
                                foundSystemPosition = true;
                                sb.AppendLine($"    {CorrectIdentifier("input")}{indexer}.{CorrectIdentifier(field.Name)} = gl_FragCoord{indexer};");
                            }
                            else
                            {
                                sb.AppendLine($"    {CorrectIdentifier("input")}{indexer}.{CorrectIdentifier(field.Name)} = fsin_{inoutIndex++}{indexer};");
                            }
                        }
                    }
                }

                if (input.Type.FixedSize == 0)
                {
                    assignInVars(null);
                }
                else
                {
                    for (int i = 0; i < input.Type.FixedSize; ++i)
                    {
                        assignInVars(i);
                    }
                }
            }

            // Call actual function.
            string invocationStr = inputType != null
                ? $"{CorrectIdentifier(entryFunction.Name)}({CorrectIdentifier("input")})"
                : $"{CorrectIdentifier(entryFunction.Name)}()";
            if (mappedReturnType != "void")
            {
                sb.AppendLine($"    {mappedReturnType} {CorrectIdentifier("output")} = {invocationStr};");
            }
            else
            {
                sb.AppendLine($"    {invocationStr};");
            }

            // Assign output fields to synthetic "out" variables with normalized "fsin_#" names.
            if (entryFunction.Type == ShaderFunctionType.VertexEntryPoint)
            {
                int inoutIndex = 0;
                FieldDefinition systemPositionField = null;
                foreach (FieldDefinition field in outputType.Fields)
                {
                    if (systemPositionField == null && field.SemanticType == SemanticType.SystemPosition)
                    {
                        systemPositionField = field;
                    }
                    else
                    {
                        sb.AppendLine($"    fsin_{inoutIndex++} = {CorrectIdentifier("output")}.{CorrectIdentifier(field.Name)};");
                    }
                }

                if (systemPositionField != null)
                {
                    // TODO: Should be caught earlier.
                    //throw new ShaderGenerationException("Vertex functions must output a SystemPosition semantic.");
                    sb.AppendLine($"    gl_Position = {CorrectIdentifier("output")}.{CorrectIdentifier(systemPositionField.Name)};");
                }

                EmitGlPositionCorrection(sb);
            }
            else if (entryFunction.Type == ShaderFunctionType.GeometryEntryPoint)
            {
            }
            else if (entryFunction.Type == ShaderFunctionType.FragmentEntryPoint)
            {
                if (mappedReturnType == "vec4")
                {
                    sb.AppendLine($"    _outputColor_ = {CorrectIdentifier("output")};");
                }
                else if (mappedReturnType != "void")
                {
                    // Composite struct -- assign each field to output
                    int colorTargetIndex = 0;
                    foreach (FieldDefinition field in outputType.Fields)
                    {
                        Debug.Assert(field.SemanticType == SemanticType.ColorTarget);
                        sb.AppendLine($"    _outputColor_{colorTargetIndex++} = {CorrectIdentifier("output")}.{CorrectIdentifier(field.Name)};");
                    }
                }
            }
            sb.AppendLine("}");
        }

        protected override string CSharpToIdentifierNameCore(string typeName, string identifier)
        {
            return GlslKnownIdentifiers.GetMappedIdentifier(typeName, identifier);
        }

        internal override string CorrectIdentifier(string identifier)
        {
            if (s_glslKeywords.Contains(identifier))
            {
                return identifier + "_";
            }

            return identifier;
        }

        internal override void AddResource(string setName, ResourceDefinition rd)
        {
            if (rd.ResourceKind == ShaderResourceKind.Uniform)
            {
                _uniformNames.Add(rd.Name);
            }
            if (rd.ResourceKind == ShaderResourceKind.StructuredBuffer
                || rd.ResourceKind == ShaderResourceKind.RWStructuredBuffer
                || rd.ResourceKind == ShaderResourceKind.AtomicBuffer)
            {
                _ssboNames.Add(rd.Name);
            }

            base.AddResource(setName, rd);
        }

        protected string CorrectIdentifierName(string identifier)
        {
            return CorrectIdentifierName(identifier, identifier);
        }

        protected virtual string CorrectIdentifierName(string originalName, string identifier)
        {
            if (_uniformNames.Contains(originalName) || _ssboNames.Contains(originalName))
            {
                return "field_" + identifier;
            }
            else
            {
                return identifier;
            }
        }

        internal override string CorrectFieldAccess(SymbolInfo symbolInfo)
        {
            string originalName = symbolInfo.Symbol.Name;
            string identifier = base.CorrectFieldAccess(symbolInfo);
            return CorrectIdentifierName(originalName, identifier);
        }

        internal override string GetComputeGroupCountsDeclaration(UInt3 groupCounts)
        {
            return $"layout(local_size_x = {groupCounts.X}, local_size_y = {groupCounts.Y}, local_size_z = {groupCounts.Z}) in;";
        }

        internal override string ParameterDirection(ParameterDirection direction)
        {
            switch (direction)
            {
                case ShaderGen.ParameterDirection.Out:
                    return "out";
                case ShaderGen.ParameterDirection.InOut:
                    return "inout";
                default:
                    return string.Empty;
            }
        }

        private static readonly HashSet<string> s_glslKeywords = new HashSet<string>()
        {
            "input", "output", "main"
        };

        protected override string SemanticIdentifier(ShaderBuiltin semantic)
        {
            switch (semantic.Semantic)
            {
                case BuiltinSemantic.ClipDistance:
                    return "gl_ClipDistance";
                case BuiltinSemantic.Instance:
                    return "gl_InvocationID";
                case BuiltinSemantic.Layer:
                    return "gl_Layer";
                case BuiltinSemantic.PointSize:
                    return "gl_PointSize";
                case BuiltinSemantic.Primitive:
                    return "gl_PrimitiveID";
                case BuiltinSemantic.Viewport:
                    return "gl_Viewport";
                case BuiltinSemantic.Color:
                    return "gl_Color";
                case BuiltinSemantic.Normal:
                    return "gl_Normal";
                case BuiltinSemantic.Position:
                    switch (semantic.Stage)
                    {
                        case ShaderStage.Vertex:
                        case ShaderStage.TesselationControl:
                        case ShaderStage.TesselationEvaluation:
                        case ShaderStage.Geometry:
                            return "gl_Position";
                        case ShaderStage.Fragment:
                            return "gl_FragCoord";
                    }
                    break;
                case BuiltinSemantic.TextureCoordinate:
                    return "gl_TexCoord";
                case BuiltinSemantic.Point:
                    return "gl_PointCoord";
                case BuiltinSemantic.FrontFacing:
                    return "gl_FrontFacing";
                case BuiltinSemantic.Sample:
                    return "gl_SampleID";
                case BuiltinSemantic.SamplePosition:
                    return "gl_SamplePosition";
                case BuiltinSemantic.SampleMask:
                    switch (semantic.Stage)
                    {
                        case ShaderStage.Fragment:
                            return "gl_SampleMaskIn";
                        case ShaderStage.PerSample:
                            return "gl_SampleMask";
                    }
                    break;
                case BuiltinSemantic.Depth:
                    return "gl_FragDepth";
                case BuiltinSemantic.BaseInstance:
                    return "gl_BaseInstance";
                case BuiltinSemantic.BaseVertex:
                    return "gl_BaseVertex";
                case BuiltinSemantic.Draw:
                    return "gl_DrawID";
                case BuiltinSemantic.PatchVertices:
                    return "gl_PatchVertices";
                case BuiltinSemantic.TesselatedCoordinate:
                    return "gl_TessCoord";
                case BuiltinSemantic.TesselationLevelInner:
                    return "gl_TessLevelInner";
                case BuiltinSemantic.TesselationLevelOuter:
                    return "gl_TessLevelOuter";
                case BuiltinSemantic.Vertex:
                    return "gl_VertexID";
                case BuiltinSemantic.PerVertex:
                    return "gl_PerVertex";
            }
            throw new NotSupportedException(semantic.ToString());
        }

        protected virtual void WriteLocal(StringBuilder sb, ResourceDefinition rd)
        {
            sb.AppendLine($"{CSharpToShaderType(rd.ValueType.Name)} {rd.Name};");
        }

        protected abstract void WriteVersionHeader(
            ShaderFunction function,
            ShaderFunctionAndMethodDeclarationSyntax[] orderedFunctions,
            StringBuilder sb);
        protected abstract void WriteUniform(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteSampler(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteSamplerComparison(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTexture1D(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTexture2D(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTexture3D(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTexture2DArray(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTexture2DRect(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTextureCube(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTexture2DMS(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteTextureBuffer(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteStructuredBuffer(StringBuilder sb, ResourceDefinition rd, bool isReadOnly, int index);
        protected abstract void WriteRWTexture2D(StringBuilder sb, ResourceDefinition rd, int index);
        protected abstract void WriteDepthTexture2D(StringBuilder sb, ResourceDefinition rd);
        protected abstract void WriteDepthTexture2DArray(StringBuilder sb, ResourceDefinition rd);
        protected void WriteInOutVariable(
            StringBuilder sb,
            bool isInVar,
            ShaderFunctionType stage,
            string normalizedType,
            string normalizedIdentifier,
            int index)
            => WriteInOutVariable(sb, isInVar, stage, normalizedType, normalizedIdentifier, index, 0);
        protected abstract void WriteInOutVariable(
            StringBuilder sb,
            bool isInVar,
            ShaderFunctionType stage,
            string normalizedType,
            string normalizedIdentifier,
            int index,
            int arraySize);
        protected abstract void EmitGlPositionCorrection(StringBuilder sb);

        internal override string CorrectCastExpression(string type, string expression)
        {
            return $"{type}({expression})";
        }
    }
}
