using NUnit.Framework;
using Sylvester;

namespace SylvesterTests
{
    [TestFixture]
    public class MatrixTests
    {
        [Test]
        public void MatrixInitializesToZerosByDefault()
        {
            var m = new Matrix(2, 3);
            Assert.That(m.GetElement(0, 0), Is.EqualTo(0));
        }

        [Test]
        public void CanSetSpecificElements()
        {
            var m = new Matrix(2, 2);
            m.SetElement(0, 0, 1);
            Assert.That(m.GetElement(0, 0), Is.EqualTo(1));
        }
    }
}
