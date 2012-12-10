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
            Assert.That(m.GetRow(0), Is.EqualTo(new double[] { 0, 0 }));
            Assert.That(m.GetRow(1), Is.EqualTo(new double[] { 0, 0 }));
        }

        [Test]
        public void CanSetSpecificElements()
        {
            var m = new Matrix(2, 2);
            m.SetElement(0, 0, 1);
            Assert.That(m.GetElement(0, 0), Is.EqualTo(1));
        }

        [Test]
        public void CanSetRowByRowIndex()
        {
            var m = new Matrix(3, 3);
            m.SetRow(1, new double[] { 1, 2, 3 });
            Assert.That(m.GetRow(1), Is.EqualTo(new double[] { 1, 2, 3 }));
        }

        [Test]
        public void CanSetColumnByColumnIndex()
        {
            var m = new Matrix(3, 3);
            m.SetColumn(1, new double[] { 1, 2, 3 });
            Assert.That(m.GetColumn(1), Is.EqualTo(new double[] { 1, 2, 3 }));
        }

        [Test]
        public void EqualIfSameSizeAndAllElementsAreSame()
        {
            var m1 = new Matrix(2, 2);
            m1.SetRow(0, new double[] { 0, 1 });
            m1.SetRow(1, new double[] { 2, 3 });

            var m2 = new Matrix(2, 2);
            m2.SetRow(0, new double[] { 0, 1 });
            m2.SetRow(1, new double[] { 2, 3 });

            Assert.True(m1 == m2);
            Assert.True(m1.Equals(m2));
        }

        [Test]
        public void NotEqualIfDifferentSize()
        {
            var m1 = new Matrix(3, 2);
            m1.SetRow(0, new double[] { 0, 1 });
            m1.SetRow(1, new double[] { 2, 3 });
            m1.SetRow(2, new double[] { 4, 5 });

            var m2 = new Matrix(2, 2);
            m2.SetRow(0, new double[] { 0, 1 });
            m2.SetRow(1, new double[] { 2, 3 });

            Assert.True(m1 != m2);
        }

        [Test]
        public void NotEqualIfDifferentValuesForElements()
        {
            var m1 = new Matrix(2, 2);
            m1.SetRow(0, new double[] { 1, 1 });
            m1.SetRow(1, new double[] { 2, 3 });

            var m2 = new Matrix(2, 2);
            m2.SetRow(0, new double[] { 0, 1 });
            m2.SetRow(1, new double[] { 2, 3 });

            Assert.True(m1 != m2);
        }

        [Test]
        public void SquareMatrix()
        {
            var m1 = new Matrix(2, 2);
            Assert.True(m1.IsSquare());

            var m2 = new Matrix(2, 3);
            Assert.False(m2.IsSquare());
        }

        [Test]
        public void ZeroMatrix()
        {
            var m1 = new Matrix(2, 2);
            m1.SetRow(0, new double[] { 0, 0 });
            m1.SetRow(1, new double[] { 0, 0 });
            Assert.True(m1.IsZero());

            var m2 = new Matrix(2, 2);
            m2.SetRow(0, new double[] { 0, 0 });
            m2.SetRow(1, new double[] { 1, 0 });
            Assert.False(m2.IsZero());   
        }
    }
}
