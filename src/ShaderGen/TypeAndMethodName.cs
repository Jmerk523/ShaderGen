using System;
using System.Linq;

namespace ShaderGen
{
    internal class TypeAndMethodName : IEquatable<TypeAndMethodName>
    {
        public string TypeName;
        public string MethodName;

        public string FullName => TypeName + "." + MethodName;

        public static bool Get(string fullName, out TypeAndMethodName typeAndMethodName)
        {
            string[] parts = fullName.Split(new[] { '.' });
            if (parts.Length < 2)
            {
                typeAndMethodName = default(TypeAndMethodName);
                return false;
            }
            string typeName = String.Join(".", parts.Take(parts.Length - 1));

            typeAndMethodName = new TypeAndMethodName { TypeName = typeName, MethodName = parts.Last() };
            return true;
        }

        public bool Equals(TypeAndMethodName other)
        {
            return TypeName == other.TypeName && MethodName == other.MethodName;
        }

        public override int GetHashCode() => FullName.GetHashCode();

        public override string ToString() => FullName;
    }
}
