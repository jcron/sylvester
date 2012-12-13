
using System;

namespace Sylvester
{
    public class BooleanMatrix : Matrix
    {
        public BooleanMatrix(int rows, int columns) : base(rows, columns)
        {
        }

        public BooleanMatrix(double[,] matrix) : base(matrix)
        {
        }

        public static BooleanMatrix operator |(BooleanMatrix lhs, BooleanMatrix rhs)
        {
            BooleanPrerequisites(lhs, rhs);
            var m = new BooleanMatrix(lhs._rows, lhs._columns);
            for (var i = 0; i < m._rows; i++)
                for (var j = 0; j < m._columns; j++)
                    if (lhs.GetElement(i, j) == 1 || rhs.GetElement(i, j) == 1)
                    {
                        m.SetElement(i, j, 1);
                    }
            return m;
        }

        public BooleanMatrix Join(BooleanMatrix rhs)
        {
            return this | rhs;
        }

        public static BooleanMatrix operator &(BooleanMatrix lhs, BooleanMatrix rhs)
        {
            BooleanPrerequisites(lhs, rhs);
            var m = new BooleanMatrix(lhs._rows, lhs._columns);
            for (var i = 0; i < m._rows; i++)
                for (var j = 0; j < m._columns; j++)
                    if ((lhs.GetElement(i, j) == 0 && rhs.GetElement(i, j) == 0) ||
                        (lhs.GetElement(i, j) == 1 && rhs.GetElement(i, j) == 1))
                    {
                        m.SetElement(i, j, 1);
                    }
            return m;
        }

        public BooleanMatrix Meet(BooleanMatrix rhs)
        {
            return this & rhs;
        }

        public BooleanMatrix Product(BooleanMatrix rhs)
        {
            if (CannotMultiply(this, rhs))
            {
                throw new InvalidOperationException("The columns of the lhs Matrix must match the rows of the rhs Matrix in order to perform this operation.");
            }
            var m = new BooleanMatrix(_rows, rhs._columns);
            for (var i = 0; i < _rows; i++)
            {
                for (var j = 0; j < rhs._columns; j++)
                {
                    var row = GetRow(i);
                    var column = rhs.GetColumn(j);
                    var element = 0;
                    for (var index = 0; index < row.Length; index++)
                    {
                        element |= (int)row[index] & (int)column[index];
                    }
                    m.SetElement(i, j, element);
                }
            }
            return m;
        }

        public override bool Equals(object obj)
        {
            return obj.GetType() == typeof(BooleanMatrix) && Equals((BooleanMatrix)obj);
        }

        public bool Equals(BooleanMatrix other)
        {
            return base.Equals(other);
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }
}
