using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Text;
using System;
using System.Linq;
using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;

namespace ShaderGen
{
    public partial class ShaderMethodVisitor : CSharpSyntaxVisitor<string>
    {
        protected readonly Compilation _compilation;
        protected readonly string _setName;
        protected readonly LanguageBackend _backend;
        protected readonly ShaderFunction _shaderFunction;
        private string _containingTypeName;
        private HashSet<ResourceDefinition> _resourcesUsed = new HashSet<ResourceDefinition>();
        private Dictionary<int, HashSet<ISymbol>> _emittedData = new Dictionary<int, HashSet<ISymbol>>();
        private HashSet<ISymbol> _writeFields = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        private SyntaxNode _lastVertexEmit;
        private int _indentation;

        public ShaderMethodVisitor(
            Compilation compilation,
            string setName,
            ShaderFunction shaderFunction,
            LanguageBackend backend)
        {
            _compilation = compilation;
            _setName = setName;
            _shaderFunction = shaderFunction;
            _backend = backend;
        }

        private SemanticModel GetModel(SyntaxNode node) => _compilation.GetSemanticModel(node.SyntaxTree);

        private void MarkWritten(ExpressionSyntax expr)
        {
            var model = GetModel(expr);
            var symbol = model.GetSymbolInfo(expr).Symbol;
            if (symbol != null)
            {
                _writeFields.Add(symbol);
            }
            if (symbol is IFieldSymbol && expr is MemberAccessExpressionSyntax memberAccess)
            {
                symbol = model.GetSymbolInfo(memberAccess.Expression).Symbol;
            }
            if (symbol != null)
            {
                var resource = _resourcesUsed.FirstOrDefault(r => r.Name == symbol.Name);
                if (resource != null)
                {
                    resource.MarkWritten();
                }
            }
        }

        public MethodProcessResult VisitFunction(BaseMethodDeclarationSyntax node)
        {
            _containingTypeName = Utilities.GetFullNestedTypePrefix((SyntaxNode)node.Body ?? node.ExpressionBody, out bool _);

            StringBuilder sb = new StringBuilder();
            string blockResult;
            // Visit block first in order to discover builtin variables.
            if (node.Body != null)
            {
                _lastVertexEmit = node.Body.Statements.FirstOrDefault();
                blockResult = VisitBlock(node.Body);
            }
            else if (node.ExpressionBody != null)
            {
                _lastVertexEmit = node.ExpressionBody;
                blockResult = VisitArrowExpressionClause(node.ExpressionBody);
            }
            else
            {
                throw new NotSupportedException("Methods without bodies cannot be shader functions.");
            }

            string functionDeclStr = GetFunctionDeclStr();

            if (_shaderFunction.Type == ShaderFunctionType.ComputeEntryPoint)
            {
                sb.AppendLine(_backend.GetComputeGroupCountsDeclaration(_shaderFunction.ComputeGroupCounts));
            }

            sb.AppendLine(functionDeclStr);
            sb.AppendLine(blockResult);

            foreach (var stream in _emittedData)
            {
                foreach (var emission in stream.Value)
                {
                    ITypeSymbol type;
                    if (emission is ILocalSymbol local)
                    {
                        type = local.Type;
                    }
                    else if (emission is IFieldSymbol field)
                    {
                        type = field.Type;
                    }
                    else
                    {
                        continue;
                    }
                    //_resourcesUsed.RemoveWhere(r => r.Name == emission.Name);
                    _resourcesUsed.Add(new ResourceDefinition(emission.Name, 0, stream.Key,
                        new TypeReference(type.GetFullMetadataName(), type),
                        ShaderResourceKind.Emit));
                }
            }
            return new MethodProcessResult(sb.ToString(), _resourcesUsed);
        }

        public override string DefaultVisit(SyntaxNode node)
        {
            throw new NotImplementedException($"{node.GetType()} are not implemented.");
        }

        public override string VisitBlock(BlockSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(Indent("{"));

            using (IncreaseIndent())
            {
                // Only declare discarded variables - i.e. MyFunc(out _) - for the top-level block in a function.
                if (node.Parent.IsKind(SyntaxKind.MethodDeclaration))
                {
                    sb.Append(DeclareDiscardedVariables(node));
                }

                foreach (StatementSyntax ss in node.Statements)
                {
                    sb.Append(DeclareInlineOutVariables(ss));

                    string statementResult = Visit(ss);
                    if (string.IsNullOrEmpty(statementResult))
                    {
                        throw new NotImplementedException($"{ss.GetType()} statements are not implemented.");
                    }
                    else
                    {
                        sb.AppendLine("    " + statementResult);
                    }
                }
            }

            sb.Append(Indent("}"));
            return sb.ToString();
        }

        /// <summary>
        /// Declares any "discard"ed variables - i.e. MyFunc(out _) - for this block and all nested blocks.
        /// </summary>
        private string DeclareDiscardedVariables(BlockSyntax block)
        {
            StringBuilder sb = new StringBuilder();

            SemanticModel semanticModel = GetModel(block);

            IEnumerable<ISymbol> discardedVariables = block
                .DescendantNodes()
                .Where(x => x.IsKind(SyntaxKind.IdentifierName))
                .Select(x => semanticModel.GetSymbolInfo(x).Symbol)
                .Where(x => x.Kind == SymbolKind.Discard)
                .Cast<IDiscardSymbol>();

            List<ISymbol> alreadyWrittenTypes = new List<ISymbol>();

            foreach (IDiscardSymbol discardedVariable in discardedVariables)
            {
                if (alreadyWrittenTypes.Contains(discardedVariable.Type))
                {
                    continue;
                }

                sb.Append("    ");
                sb.Append(GetDiscardedVariableType(discardedVariable));
                sb.Append(' ');
                sb.Append(GetDiscardedVariableName(discardedVariable));
                sb.AppendLine(";");

                alreadyWrittenTypes.Add(discardedVariable.Type);
            }

            return sb.ToString();
        }

        /// <summary>
        /// Check for any inline "out" variable declarations in this statement - i.e. MyFunc(out var result) - 
        /// and declare those variables now.
        /// </summary>
        private string DeclareInlineOutVariables(StatementSyntax statement)
        {
            StringBuilder sb = new StringBuilder();

            IEnumerable<SyntaxNode> declarationExpressionNodes = statement
                .DescendantNodes(x => !x.IsKind(SyntaxKind.Block)) // Don't descend into child blocks
                .Where(x => x.IsKind(SyntaxKind.DeclarationExpression));

            foreach (DeclarationExpressionSyntax declarationExpressionNode in declarationExpressionNodes)
            {
                string varType = _compilation.GetSemanticModel(declarationExpressionNode.Type.SyntaxTree).GetFullTypeName(declarationExpressionNode.Type);
                string mappedType = _backend.CSharpToShaderType(varType);

                sb.Append("    ");
                sb.Append(mappedType);
                sb.Append(' ');

                switch (declarationExpressionNode.Designation)
                {
                    case SingleVariableDesignationSyntax svds:
                        string identifier = _backend.CorrectIdentifier(svds.Identifier.Text);
                        sb.Append(identifier);
                        sb.Append(';');
                        sb.AppendLine();
                        break;

                    default:
                        throw new NotImplementedException($"{declarationExpressionNode.Designation.GetType()} designations are not implemented.");
                }
            }

            return sb.ToString();
        }

        public override string VisitArrowExpressionClause(ArrowExpressionClauseSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(Indent("{"));

            using (IncreaseIndent())
            {
                string expressionResult = Visit(node.Expression);

                if (_shaderFunction.ReturnType.Name == "System.Void")
                {
                    sb.AppendLine(Indent(expressionResult + ";"));
                }
                else
                {
                    sb.AppendLine(Indent("return " + expressionResult + ";"));
                }
            }

            sb.AppendLine(Indent("}"));
            return sb.ToString();
        }

        protected virtual string GetFunctionDeclStr()
        {
            string returnType = _backend.CSharpToShaderType(_shaderFunction.ReturnType.Name);
            string fullDeclType = _backend.CSharpToShaderType(_shaderFunction.DeclaringType);
            string shaderFunctionName = _backend.CorrectIdentifier(_shaderFunction.Name).Replace(".", "0_");
            string funcName = _shaderFunction.IsEntryPoint
                ? shaderFunctionName
                : fullDeclType + "_" + shaderFunctionName;
            return $"{returnType} {funcName}({GetParameterDeclList()})";
        }

        public override string VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            if (node.Modifiers.Any(x => x.IsKind(SyntaxKind.ConstKeyword)))
            {
                return " "; // TODO: Can't return empty string here because of validation check in VisitBlock
            }

            return Visit(node.Declaration);
        }

        public override string VisitEqualsValueClause(EqualsValueClauseSyntax node)
        {
            return node.EqualsToken.ToFullString() + Visit(node.Value);
        }

        public override string VisitAssignmentExpression(AssignmentExpressionSyntax node)
        {
            string token = node.OperatorToken.ToFullString().Trim();
            if (token == "%=")
            {
                throw new ShaderGenerationException(
                    "Modulus operator not supported in shader functions. Use ShaderBuiltins.Mod instead.");
            }

            string leftExpr = base.Visit(node.Left);
            string leftExprType = Utilities.GetFullTypeName(GetModel(node), node.Left);
            string rightExpr = base.Visit(node.Right);
            string rightExprType = Utilities.GetFullTypeName(GetModel(node), node.Right);

            MarkWritten(node.Left);

            string assignedValue = _backend.CorrectAssignedValue(leftExprType, rightExpr, rightExprType);
            return $"{leftExpr} {token} {assignedValue}";
        }

        public override string VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            SymbolInfo exprSymbol = GetModel(node).GetSymbolInfo(node.Expression);
            if (exprSymbol.Symbol?.Kind == SymbolKind.NamedType)
            {
                SymbolInfo symbolInfo = GetModel(node).GetSymbolInfo(node);
                ISymbol symbol = symbolInfo.Symbol;

                // Enum field
                INamedTypeSymbol namedTypeSymbol = (INamedTypeSymbol)exprSymbol.Symbol;
                if (namedTypeSymbol.TypeKind == TypeKind.Enum)
                {
                    IFieldSymbol enumFieldSymbol = (IFieldSymbol)symbol;
                    string constantValueString = enumFieldSymbol.ConstantValue.ToString();
                    if (namedTypeSymbol.EnumUnderlyingType.SpecialType == SpecialType.System_UInt32)
                    {
                        // TODO: We need to do this for literal values too, if they don't already have this suffix, 
                        // so this should be refactored.
                        constantValueString += "u";
                    }
                    return constantValueString;
                }

                // Static member access
                if (symbol.Kind == SymbolKind.Property || symbol.Kind == SymbolKind.Field)
                {
                    return Visit(node.Name);
                }

                string typeName = Utilities.GetFullMetadataName(exprSymbol.Symbol);
                string targetName = Visit(node.Name);
                return _backend.FormatInvocation(_setName, typeName, targetName, Array.Empty<InvocationParameterInfo>());
            }
            else if (exprSymbol.Symbol is IFieldSymbol field)
            {
                var context = _backend.GetContext(_setName);
                if (context.Resources.Any(r => r.Name == field.Name && r.ResourceKind == ShaderResourceKind.Emit))
                {
                    return "out_" + Visit(node.Name);
                }
            }

            // Other accesses
            bool isIndexerAccess = (node.Expression is ElementAccessExpressionSyntax)
                || _backend.IsIndexerAccess(GetModel(node).GetSymbolInfo(node.Name));

            string expr = Visit(node.Expression);
            string name = Visit(node.Name);

            return expr + node.OperatorToken.ToFullString() + name;
        }

        private IndentationScope IncreaseIndent()
        {
            return new IndentationScope(this);
        }

        private string Indent(string str)
        {
            return new string(' ', _indentation * 4) + str;
        }

        private static readonly char[] newlineChars = new char[] { '\r', '\n' };
        public override string Visit(SyntaxNode node)
        {
            var result = base.Visit(node);
            if (node is StatementSyntax && result.IndexOfAny(newlineChars) < 0) 
            {
                return Indent(result);
            }
            return result;
        }

        public override string VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            return Visit(node.Expression) + ";";
        }

        public override string VisitReturnStatement(ReturnStatementSyntax node)
        {
            return "return "
                + Visit(node.Expression)
                + ";";
        }

        public override string VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            if (node.Expression is IdentifierNameSyntax ins)
            {
                InvocationParameterInfo[] parameterInfos = GetParameterInfos(node.ArgumentList);
                SymbolInfo symbolInfo = GetModel(node).GetSymbolInfo(ins);
                string type = symbolInfo.Symbol.ContainingType.ToDisplayString();
                string method = symbolInfo.Symbol.Name;

                ProcessBuiltInMethodInvocation(method, node);

                return _backend.FormatInvocation(_setName, type, method, parameterInfos);
            }
            else if (node.Expression is MemberAccessExpressionSyntax maes)
            {
                SymbolInfo methodSymbol = GetModel(maes).GetSymbolInfo(maes);
                if (methodSymbol.Symbol is IMethodSymbol ims)
                {
                    string containingType = Utilities.GetFullMetadataName(ims.ContainingType);
                    string methodName = ims.MetadataName;
                    List<InvocationParameterInfo> pis = new List<InvocationParameterInfo>();
                    if (ims.IsExtensionMethod)
                    {
                        string identifier = Visit(maes.Expression);
                        string identifierType = Utilities.GetFullTypeName(GetModel(maes.Expression), maes.Expression);
                        Debug.Assert(identifier != null);
                        // Might need FullTypeName here too.
                        pis.Add(new InvocationParameterInfo()
                        {
                            Identifier = identifier,
                            FullTypeName = identifierType,
                        });
                    }

                    else if (!ims.IsStatic) // Add implicit "this" parameter.
                    {
                        string identifier = null;
                        if (maes.Expression is MemberAccessExpressionSyntax subExpression)
                        {
                            identifier = Visit(subExpression);
                        }
                        else if (maes.Expression is IdentifierNameSyntax identNameSyntax)
                        {
                            identifier = Visit(identNameSyntax);
                        }

                        Debug.Assert(identifier != null);
                        pis.Add(new InvocationParameterInfo
                        {
                            FullTypeName = containingType,
                            Identifier = identifier
                        });
                    }

                    ProcessBuiltInMethodInvocation(methodName, node);

                    pis.AddRange(GetParameterInfos(node.ArgumentList));
                    return _backend.FormatInvocation(_setName, containingType, methodName, pis.ToArray());
                }

                throw new NotImplementedException();
            }
            else
            {
                string message = "Function calls must be made through an IdentifierNameSyntax or a MemberAccessExpressionSyntax.";
                message += Environment.NewLine + "This node used a " + node.Expression.GetType().Name;
                message += Environment.NewLine + node.ToFullString();
                throw new NotImplementedException(message);
            }
        }

        private void ProcessBuiltInMethodInvocation(string name, InvocationExpressionSyntax node)
        {
            switch (name)
            {
                case nameof(ShaderBuiltins.Ddx):
                case nameof(ShaderBuiltins.Ddy):
                case nameof(ShaderBuiltins.SampleComparisonLevelZero):
                    if (_shaderFunction.Type != ShaderFunctionType.FragmentEntryPoint)
                    {
                        throw new ShaderGenerationException($"{name} can only be used within Fragment shaders.");
                    }
                    break;

                case nameof(ShaderBuiltins.InterlockedAdd):
                    _shaderFunction.UsesInterlockedAdd = true;
                    break;
                case nameof(GeometryExtensions.EmitVertex):
                case nameof(GeometryExtensions.EmitStreamVertex):
                    if (_shaderFunction.Type != ShaderFunctionType.GeometryEntryPoint)
                    {
                        throw new ShaderGenerationException($"{name} can only be used within Geometry shaders.");
                    }
                    ProcessEmitVertex(node);
                    break;
                case nameof(GeometryExtensions.EndPrimitive):
                case nameof(GeometryExtensions.EndStreamPrimitive):
                    if (_shaderFunction.Type != ShaderFunctionType.GeometryEntryPoint)
                    {
                        throw new ShaderGenerationException($"{name} can only be used within Geometry shaders.");
                    }
                    break;
            }
        }

        private void ProcessEmitVertex(InvocationExpressionSyntax node)
        {
            var model = GetModel(node);
            DataFlowAnalysis analysis = null;
            if (_lastVertexEmit is ExpressionSyntax expression)
            {
                analysis = model.AnalyzeDataFlow(expression);
            }
            else if (_lastVertexEmit is StatementSyntax lastStatement)
            {
                var emitStatement = node.Ancestors().OfType<StatementSyntax>().FirstOrDefault();
                if (lastStatement.Parent != emitStatement.Parent)
                {
                    lastStatement = emitStatement.Parent.ChildNodes().Cast<StatementSyntax>().First();
                }
                analysis = model.AnalyzeDataFlow(lastStatement, emitStatement);
                _lastVertexEmit = emitStatement;
            }

            var emit = model.GetSymbolInfo(node.ArgumentList.Arguments.Last().Expression);
            if (!(emit.Symbol is IFieldSymbol emitParam))
            {
                throw new ShaderGenerationException("EmitVertex argument must be a field");
            }
            var emitAttribute = emit.Symbol.GetAttributes().FirstOrDefault(a => a.AttributeClass.Name.Contains("EmitVertex"));
            if (emitAttribute == null)
            {
                throw new ShaderGenerationException("EmitVertex arguments must be marked with EmitVertexAttribute");
            }

            int streamId = 0;
            if (node.ArgumentList.Arguments.Count > 1)
            {
                streamId = (int)model.GetConstantValue(node.ArgumentList.Arguments.First()).Value;
            }

            var assignments = analysis.AlwaysAssigned.Concat(_writeFields);
            if (!_emittedData.TryGetValue(streamId, out var emissions))
            {
                emissions = new HashSet<ISymbol>(assignments.OfType<IFieldSymbol>(), SymbolEqualityComparer.Default);
                _emittedData.Add(streamId, emissions);
                if (assignments.Contains(emitParam))
                {
                    emissions.Add(emitParam);
                }
            }

            if (analysis != null)
            {
                foreach (var emission in emissions)
                {
                    if (!assignments.Contains(emission) && !assignments.Contains(emission.ContainingType))
                    {
                        var type = (emission as IFieldSymbol)?.Type ?? (emission as IParameterSymbol)?.Type;
                        if (type == null || !type.GetMembers().OfType<IFieldSymbol>().All(assignments.Contains))
                        {
                            throw new ShaderGenerationException($"{emission.Name} must be fully assigned before every call to EmitVertex");
                        }
                    }
                }
                foreach (var assignment in assignments.OfType<IFieldSymbol>().Append<ISymbol>(emitParam).Where(a => !emissions.Contains(a)))
                {
                    var type = (assignment as IFieldSymbol)?.Type ?? (assignment as IParameterSymbol)?.Type;
                    if (type == null || !type.GetMembers().OfType<IFieldSymbol>().All(emissions.Contains))
                    {
                        throw new ShaderGenerationException($"{assignment.Name} must be fully assigned before every call to EmitVertex");
                    }
                }
            }
            _writeFields.Clear();
        }

        public override string VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            string token = node.OperatorToken.ToFullString().Trim();
            if (token == "%")
            {
                throw new ShaderGenerationException(
                    "Modulus operator not supported in shader functions. Use ShaderBuiltins.Mod instead.");
            }

            string leftExpr = Visit(node.Left);
            string leftExprType = Utilities.GetFullTypeName(GetModel(node), node.Left);
            string operatorToken = node.OperatorToken.ToString();
            string rightExpr = Visit(node.Right);
            string rightExprType = Utilities.GetFullTypeName(GetModel(node), node.Right);

            if (SyntaxFacts.IsAssignmentExpression(node.Kind()))
            {
                MarkWritten(node.Left);
            }

            return _backend.CorrectBinaryExpression(leftExpr, leftExprType, operatorToken, rightExpr, rightExprType);
        }

        public override string VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
        {
            return node.OpenParenToken
                + Visit(node.Expression)
                + node.CloseParenToken;
        }

        public override string VisitArgumentList(ArgumentListSyntax node)
        {
            return string.Join(", ", node.Arguments.Select(argSyntax => Visit(argSyntax)));
        }

        public override string VisitArgument(ArgumentSyntax node)
        {
            string result = Visit(node.Expression);
            if (string.IsNullOrEmpty(result))
            {
                throw new NotImplementedException($"{node.Expression.GetType()} arguments are not implemented.");
            }
            else
            {
                return result;
            }
        }

        public override string VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
        {
            SymbolInfo symbolInfo = GetModel(node).GetSymbolInfo(node.Type);
            string fullName = Utilities.GetFullName(symbolInfo);

            InvocationParameterInfo[] parameters = GetParameterInfos(node.ArgumentList);
            return _backend.FormatInvocation(_setName, fullName, ".ctor", parameters);
        }

        private string GetDiscardedVariableType(ISymbol symbol)
        {
            Debug.Assert(symbol.Kind == SymbolKind.Discard);

            string varType = Utilities.GetFullTypeName(((IDiscardSymbol)symbol).Type, out _);
            return _backend.CSharpToShaderType(varType);
        }

        private string GetDiscardedVariableName(ISymbol symbol)
        {
            string mappedType = GetDiscardedVariableType(symbol);
            return _backend.CorrectIdentifier($"_shadergen_discard_{mappedType}");
        }

        private ExpressionSyntax GetExpression(SyntaxNode node)
        {
            while (!(node is ExpressionSyntax))
            {
                node = node.Parent;
            }
            while (node.Parent is ExpressionSyntax)
            {
                node = node.Parent;
            }
            return node as ExpressionSyntax;
        }

        public override string VisitIdentifierName(IdentifierNameSyntax node)
        {
            SymbolInfo symbolInfo = GetModel(node).GetSymbolInfo(node);
            ISymbol symbol = symbolInfo.Symbol;
            if (symbol.Kind == SymbolKind.Discard)
            {
                return GetDiscardedVariableName(symbol);
            }
            string containingTypeName = Utilities.GetFullName(symbol.ContainingType);
            if (containingTypeName == "ShaderGen.ShaderBuiltins")
            {
                TryRecognizeBuiltInVariable(symbolInfo);
            }
            if (symbol is IFieldSymbol fs && fs.HasConstantValue)
            {
                // TODO: Share code to format constant values.
                return string.Format(CultureInfo.InvariantCulture, "{0}", fs.ConstantValue);
            }
            //else if (symbol is IFieldSymbol && _backend.GetContext(_setName).BuiltIns.Any(b => b.Name == symbol.Name))
            //{
            //    return _backend.CorrectFieldAccess(symbolInfo);
            //}
            else if (symbol.Kind == SymbolKind.Field && containingTypeName == _containingTypeName)
            {
                string symbolName = symbol.Name;
                ResourceDefinition referencedResource = _backend.GetContext(_setName).Resources.SingleOrDefault(rd => rd.Name == symbolName);
                if (referencedResource != null)
                {
                    _resourcesUsed.Add(referencedResource);
                    _shaderFunction.UsesTexture2DMS |= referencedResource.ResourceKind == ShaderResourceKind.Texture2DMS;
                    bool usesStructuredBuffer = referencedResource.ResourceKind == ShaderResourceKind.StructuredBuffer
                        || referencedResource.ResourceKind == ShaderResourceKind.RWStructuredBuffer
                        || referencedResource.ResourceKind == ShaderResourceKind.AtomicBuffer;
                    _shaderFunction.UsesStructuredBuffer |= usesStructuredBuffer;
                    _shaderFunction.UsesRWTexture2D |= referencedResource.ResourceKind == ShaderResourceKind.RWTexture2D;

                    if (referencedResource.ResourceKind == ShaderResourceKind.BuiltIn)
                    {
                        return _backend.CSharpToShaderIdentifierName(symbolInfo);
                    }
                }

                return _backend.CorrectFieldAccess(symbolInfo);
            }
            else if (symbol.Kind == SymbolKind.Property)
            {
                return _backend.FormatInvocation(_setName, containingTypeName, symbol.Name, Array.Empty<InvocationParameterInfo>());
            }
            else if (symbol is ILocalSymbol ls && ls.HasConstantValue)
            {
                // TODO: Share code to format constant values.
                return string.Format(CultureInfo.InvariantCulture, "{0}", ls.ConstantValue);
            }

            string mapped = _backend.CSharpToShaderIdentifierName(symbolInfo);
            return _backend.CorrectIdentifier(mapped);
        }

        private void TryRecognizeBuiltInVariable(SymbolInfo symbolInfo)
        {
            string name = symbolInfo.Symbol.Name;
            if (name == nameof(ShaderBuiltins.VertexID))
            {
                if (_shaderFunction.Type != ShaderFunctionType.VertexEntryPoint)
                {
                    throw new ShaderGenerationException("VertexID can only be used within Vertex shaders.");
                }
                _shaderFunction.UsesVertexID = true;
            }
            else if (name == nameof(ShaderBuiltins.InstanceID))
            {
                _shaderFunction.UsesInstanceID = true;
            }
            else if (name == nameof(ShaderBuiltins.DispatchThreadID))
            {
                if (_shaderFunction.Type != ShaderFunctionType.ComputeEntryPoint)
                {
                    throw new ShaderGenerationException("DispatchThreadID can only be used within Compute shaders.");
                }
                _shaderFunction.UsesDispatchThreadID = true;
            }
            else if (name == nameof(ShaderBuiltins.GroupThreadID))
            {
                if (_shaderFunction.Type != ShaderFunctionType.ComputeEntryPoint)
                {
                    throw new ShaderGenerationException("GroupThreadID can only be used within Compute shaders.");
                }
                _shaderFunction.UsesGroupThreadID = true;
            }
            else if (name == nameof(ShaderBuiltins.IsFrontFace))
            {
                if (_shaderFunction.Type != ShaderFunctionType.FragmentEntryPoint)
                {
                    throw new ShaderGenerationException("IsFrontFace can only be used within Fragment shaders.");
                }
                _shaderFunction.UsesFrontFace = true;
            }
        }

        public override string VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            string literal = node.ToFullString().Trim();
            return _backend.CorrectLiteral(literal);
        }

        public override string VisitIfStatement(IfStatementSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(Indent("if (" + Visit(node.Condition) + ")"));
            using (IncreaseIndent())
            {
                sb.AppendLine(Visit(node.Statement));
                if (node.Else != null)
                {
                    sb.AppendLine(Visit(node.Else));
                }
            }
            return sb.ToString();
        }

        public override string VisitElseClause(ElseClauseSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(Indent("else"));
            sb.Append(Visit(node.Statement));
            return sb.ToString();
        }

        public override string VisitForStatement(ForStatementSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            string declaration = Visit(node.Declaration);
            string incrementers = string.Join(", ", node.Incrementors.Select(es => Visit(es)));
            string condition = Visit(node.Condition);
            sb.AppendLine($"for ({declaration} {condition}; {incrementers})");
            sb.AppendLine(Visit(node.Statement));
            return sb.ToString();
        }

        public override string VisitSwitchStatement(SwitchStatementSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(Indent("switch (" + Visit(node.Expression) + ")"));
            sb.AppendLine(Indent("{"));
            using (IncreaseIndent())
            {
                foreach (SwitchSectionSyntax section in node.Sections)
                {
                    foreach (SwitchLabelSyntax label in section.Labels)
                    {
                        sb.AppendLine(Indent(Visit(label)));
                    }

                    using (IncreaseIndent())
                    {
                        foreach (StatementSyntax statement in section.Statements)
                        {
                            sb.AppendLine(Visit(statement));
                        }
                    }
                }
            }
            sb.AppendLine(Indent("}"));
            return sb.ToString();
        }
        public override string VisitCaseSwitchLabel(CaseSwitchLabelSyntax node)
        {
            return "case " + Visit(node.Value) + ":";
        }

        public override string VisitDefaultSwitchLabel(DefaultSwitchLabelSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine("default:");
            return sb.ToString();
        }

        public override string VisitBreakStatement(BreakStatementSyntax node)
        {
            return "break;";
        }

        public override string VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
        {
            return node.OperatorToken.ToFullString() + Visit(node.Operand);
        }

        public override string VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
        {
            return Visit(node.Operand) + node.OperatorToken.ToFullString();
        }

        public override string VisitVariableDeclaration(VariableDeclarationSyntax node)
        {
            if (node.Variables.Count != 1)
            {
                throw new NotImplementedException();
            }

            StringBuilder sb = new StringBuilder();

            SemanticModel semanticModel = _compilation.GetSemanticModel(node.Type.SyntaxTree);
            string varType = semanticModel.GetFullTypeName(node.Type);
            var typeRef = new TypeReference(varType, semanticModel.GetTypeInfo(node.Type).Type);
            _backend.ForceTypeDiscovery(_setName, typeRef, out var sd);
            if (sd != null && _backend.GetContext(_setName).Structures.Any(s => s.Name == sd.Name))
            {
                _resourcesUsed.Add(new ResourceDefinition(
                    node.Variables.First().Identifier.Text, 0, 0, typeRef, ShaderResourceKind.Local));
            }
            string mappedType = _backend.CSharpToShaderType(typeRef);
            bool first = true;
            foreach (var varDeclarator in node.Variables)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    sb.AppendLine();
                }

                sb.Append(mappedType);
                sb.Append(' ');

                string identifier = _backend.CorrectIdentifier(varDeclarator.Identifier.ToString());
                sb.Append(identifier);

                if (varDeclarator.Initializer != null && !(varDeclarator.Initializer.Value is DefaultExpressionSyntax))
                {
                    sb.Append(' ');
                    sb.Append(varDeclarator.Initializer.EqualsToken.ToString());
                    sb.Append(' ');

                    string rightExpr = base.Visit(varDeclarator.Initializer.Value);
                    string rightExprType = Utilities.GetFullTypeName(GetModel(node), varDeclarator.Initializer.Value);

                    sb.Append(_backend.CorrectAssignedValue(varType, rightExpr, rightExprType));
                }

                sb.Append(";");
            }

            return sb.ToString();
        }

        public override string VisitElementAccessExpression(ElementAccessExpressionSyntax node)
        {
            return Visit(node.Expression) + Visit(node.ArgumentList);
        }

        public override string VisitBracketedArgumentList(BracketedArgumentListSyntax node)
        {
            return node.OpenBracketToken.ToFullString()
                + string.Join(", ", node.Arguments.Select(argSyntax => Visit(argSyntax)))
                + node.CloseBracketToken.ToFullString();
        }

        public override string VisitCastExpression(CastExpressionSyntax node)
        {
            var semanticModel = _compilation.GetSemanticModel(node.SyntaxTree);
            var conversion = semanticModel.ClassifyConversion(node.Expression, semanticModel.GetTypeInfo(node).ConvertedType);

            if (conversion.IsImplicit && conversion.IsNumeric)
            {
                return Visit(node.Expression);
            }
            else
            {
                string varType = semanticModel.GetFullTypeName(node.Type);
                string mappedType = _backend.CSharpToShaderType(varType);

                return _backend.CorrectCastExpression(mappedType, Visit(node.Expression));
            }
        }

        public override string VisitDeclarationExpression(DeclarationExpressionSyntax node)
        {
            return Visit(node.Designation);
        }

        public override string VisitSingleVariableDesignation(SingleVariableDesignationSyntax node)
        {
            return _backend.CorrectIdentifier(node.Identifier.Text);
        }

        public override string VisitConditionalExpression(ConditionalExpressionSyntax node)
        {
            return Visit(node.Condition)
                + node.QuestionToken.ToFullString()
                + Visit(node.WhenTrue)
                + node.ColonToken.ToFullString()
                + Visit(node.WhenFalse);
        }

        public override string VisitDoStatement(DoStatementSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append(node.DoKeyword);
            sb.Append(" {");
            sb.AppendLine();
            sb.Append(Visit(node.Statement));
            sb.AppendLine();
            sb.Append(" } while (");
            sb.Append(Visit(node.Condition));
            sb.Append(");");
            return sb.ToString();

        }

        public override string VisitWhileStatement(WhileStatementSyntax node)
        {
            StringBuilder sb = new StringBuilder();
            sb.Append("while (");
            sb.Append(Visit(node.Condition));
            sb.AppendLine(")");
            sb.Append(Visit(node.Statement));
            return sb.ToString();
        }

        protected string GetParameterDeclList()
        {
            return string.Join(", ", _shaderFunction.Parameters.Select(FormatParameter));
        }

        protected virtual string FormatParameter(ParameterDefinition pd)
        {
            string indexer = pd.Type.FixedSize > 0 ? $"[{pd.Type.FixedSize}]" : String.Empty;
            return $"{_backend.ParameterDirection(pd.Direction)} {_backend.CSharpToShaderType(pd.Type)} {_backend.CorrectIdentifier(pd.Name)}{indexer}";
        }

        private InvocationParameterInfo[] GetParameterInfos(ArgumentListSyntax argumentList)
        {
            return argumentList.Arguments.Select(argSyntax =>
            {
                return GetInvocationParameterInfo(argSyntax);
            }).ToArray();
        }

        private InvocationParameterInfo GetInvocationParameterInfo(ArgumentSyntax argSyntax)
        {
            TypeInfo typeInfo = GetModel(argSyntax).GetTypeInfo(argSyntax.Expression);
            return new InvocationParameterInfo
            {
                FullTypeName = typeInfo.Type.ToDisplayString(),
                Identifier = Visit(argSyntax.Expression)
            };
        }

        private struct IndentationScope : IDisposable
        {
            private readonly ShaderMethodVisitor visitor;

            public IndentationScope(ShaderMethodVisitor visitor)
            {
                this.visitor = visitor;
                ++visitor._indentation;
            }

            public void Dispose()
            {
                --visitor._indentation;
            }
        }
    }
}
