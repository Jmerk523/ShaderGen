namespace ShaderGen
{
    public class UniformBuffer<T> where T : unmanaged
    {
        public T this[int index] => throw new ShaderBuiltinException();
        public T this[uint index] => throw new ShaderBuiltinException();

        public int Length => throw new ShaderBuiltinException();
    }
}
