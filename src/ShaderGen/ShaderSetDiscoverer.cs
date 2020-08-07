using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Generic;

namespace ShaderGen
{
    internal class ShaderSetDiscoverer : CSharpSyntaxWalker
    {
        private readonly HashSet<string> _discoveredNames = new HashSet<string>();
        private readonly List<ShaderSetInfo> _shaderSets = new List<ShaderSetInfo>();
        public override void VisitAttribute(AttributeSyntax node)
        {
            // TODO: Only look at assembly-level attributes.
            if (node.Name.ToFullString().Contains("ComputeShaderSet"))
            {
                string name = GetStringParam(node, 0);
                string cs = GetStringParam(node, 1);
                if (!TypeAndMethodName.Get(cs, out TypeAndMethodName csName))
                {
                    throw new ShaderGenerationException("ComputeShaderSetAttribute has an incomplete or invalid compute shader name.");
                }

                _shaderSets.Add(new ShaderSetInfo(name, csName));
            }
            else if (node.Name.ToFullString().Contains("ShaderSet"))
            {
                int paramIndex = 0;
                string name = GetStringParam(node, paramIndex++);

                TypeAndMethodName vsName = null;
                string vs = GetStringParam(node, paramIndex++);
                if (vs != null && !TypeAndMethodName.Get(vs, out vsName))
                {
                    throw new ShaderGenerationException("ShaderSetAttribute has an incomplete or invalid vertex shader name.");
                }

                TypeAndMethodName gsName = null;
                if (node.ArgumentList.Arguments.Count > 3)
                {
                    string gs = GetStringParam(node, paramIndex++);
                    if (gs != null && !TypeAndMethodName.Get(gs, out gsName))
                    {
                        throw new ShaderGenerationException("ShaderSetAttribute has an incomplete or invalid geometry shader name.");
                    }
                }

                TypeAndMethodName fsName = null;
                string fs = GetStringParam(node, paramIndex++);
                if (fs != null && !TypeAndMethodName.Get(fs, out fsName))
                {
                    throw new ShaderGenerationException("ShaderSetAttribute has an incomplete or invalid fragment shader name.");
                }

                if (vsName == null && gsName == null && fsName == null)
                {
                    throw new ShaderGenerationException("ShaderSetAttribute must specify at least one shader name.");
                }

                if (!_discoveredNames.Add(name))
                {
                    throw new ShaderGenerationException("Multiple shader sets with the same name were defined: " + name);
                }

                _shaderSets.Add(new ShaderSetInfo(
                    name,
                    vsName,
                    gsName,
                    fsName));
            }
        }

        private string GetStringParam(AttributeSyntax node, int index)
        {
            string text = node.ArgumentList.Arguments[index].ToFullString();
            if (text == "null")
            {
                return null;
            }
            else
            {
                return text.Trim().TrimStart('"').TrimEnd('"');
            }
        }

        public ShaderSetInfo[] GetShaderSets() => _shaderSets.ToArray();
    }
}
