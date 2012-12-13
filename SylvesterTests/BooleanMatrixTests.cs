using System;
using NUnit.Framework;
using Sylvester;

namespace SylvesterTests
{
    [TestFixture]
    public class BooleanMatrixTests
    {
        [Test]
        [ExpectedException(typeof(InvalidOperationException), ExpectedMessage = "The matrices must be binary in order to perform this operation.")]
        public void CannotOrIfMatricesAreNotBinary()
        {
            var m1 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {0, 1 }
                                   });
            var m2 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {2, 1 }
                                   });
            var m3 = m1 | m2;
        }

        [Test]
        [ExpectedException(typeof(InvalidOperationException), ExpectedMessage = "The rows and columns must match in order to perform this operation.")]
        public void CannotOrIfMatricesAreNotSameSize()
        {
            var m1 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {0, 1 }
                                   });
            var m2 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0, 1 },
                                       {0, 1, 0 }
                                   });
            var m3 = m1 | m2;
        }

        [Test]
        public void BooleanJoin()
        {
            var m1 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {0, 1 }
                                   });
            var m2 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 1 },
                                       {0, 0 }
                                   });

            var resultMatrix = new BooleanMatrix(new double[,]
                                              {
                                                  { 1, 1 },
                                                  { 0, 1 }
                                              });
            Assert.That(m1 | m2, Is.EqualTo(resultMatrix));
            Assert.That(m1.Join(m2), Is.EqualTo(resultMatrix));
        }

        [Test]
        [ExpectedException(typeof(InvalidOperationException), ExpectedMessage = "The matrices must be binary in order to perform this operation.")]
        public void CannotAndIfMatricesAreNotBinary()
        {
            var m1 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {0, 1 }
                                   });
            var m2 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {2, 1 }
                                   });
            var m3 = m1 & m2;
        }

        [Test]
        [ExpectedException(typeof(InvalidOperationException), ExpectedMessage = "The rows and columns must match in order to perform this operation.")]
        public void CannotAndIfMatricesAreNotSameSize()
        {
            var m1 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {0, 1 }
                                   });
            var m2 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0, 1 },
                                       {0, 1, 0 }
                                   });
            var m3 = m1 & m2;
        }

        [Test]
        public void BooleanMeet()
        {
            var m1 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 0 },
                                       {0, 1 }
                                   });
            var m2 = new BooleanMatrix(new double[,]
                                   {
                                       {1, 1 },
                                       {0, 0 }
                                   });

            var resultMatrix = new BooleanMatrix(new double[,]
                                              {
                                                  { 1, 0 },
                                                  { 1, 0 }
                                              });
            Assert.That(m1 & m2, Is.EqualTo(resultMatrix));
            Assert.That(m1.Meet(m2), Is.EqualTo(resultMatrix));
        }

        [Test]
        [ExpectedException(typeof(InvalidOperationException), ExpectedMessage = "The columns of the lhs Matrix must match the rows of the rhs Matrix in order to perform this operation.")]
        public void CannotPerformBooleanProductIfRowsOfADoNoMatchColumnsOfB()
        {
            var m1 = new BooleanMatrix(new double[,]
                                    {
                                        {1, 1, 1},
                                        {4, 0, 2}
                                    });
            var m2 = new BooleanMatrix(new double[,]
                                    {
                                        {1, 0},
                                        {0, 1}
                                    });
            var m3 = m1.Product(m2);
        }

        [Test]
        public void BooleanProduct()
        {
            var m1 = new BooleanMatrix(new double[,]
                                    {
                                        {0, 0, 1},
                                        {1, 0, 1}
                                    });
            var m2 = new BooleanMatrix(new double[,]
                                    {
                                        {1, 0, 1},
                                        {0, 1, 1},
                                        {0, 0, 0}
                                    });
            var resultMatrix = new BooleanMatrix(new double[,]
                                              {
                                                  {0, 0, 0},
                                                  {1, 0, 1}
                                              });
            Assert.That(m1.Product(m2), Is.EqualTo(resultMatrix));
        }
    }
}
