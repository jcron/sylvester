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
            var m = new Matrix(2, 2);
            Assert.That(m.GetElement(0, 0), Is.EqualTo(0));
            Assert.That(m.GetElement(0, 1), Is.EqualTo(0));
            Assert.That(m.GetElement(1, 0), Is.EqualTo(0));
            Assert.That(m.GetElement(1, 1), Is.EqualTo(0));
        }

        [Test]
        public void CanSetSpecificElements()
        {
            var m = new Matrix(2, 2);
            m.SetElement(0, 0, 1);
            Assert.That(m.GetElement(0, 0), Is.EqualTo(1));
            Assert.That(m.GetElement(0, 1), Is.EqualTo(0));
            Assert.That(m.GetElement(1, 0), Is.EqualTo(0));
            Assert.That(m.GetElement(1, 1), Is.EqualTo(0));
        }

        [Test]
        public void CanSetRowByRowIndex()
        {
            var m = new Matrix(3, 3);
            m.SetElements(1, new double[3] {1,2,3});
            Assert.That(m.GetElement(1, 0), Is.EqualTo(1));
            Assert.That(m.GetElement(1, 1), Is.EqualTo(2));
            Assert.That(m.GetElement(1, 2), Is.EqualTo(3));
        }
    }
}
