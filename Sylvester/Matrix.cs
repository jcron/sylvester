
using System;

namespace Sylvester
{
    public class Matrix
    {
        private readonly double[,] _matrix;
        private readonly int _columns;
        private readonly int _rows;

        public Matrix(int rows, int columns)
        {
            _matrix = new double[rows, columns];
            _rows = rows;
            _columns = columns;
            for (var i = 0; i < rows; i++)
                for (var j = 0; j < columns; j++)
                    _matrix[i, j] = 0;
        }

        public Matrix(double[,] matrix)
        {
            _rows = matrix.GetLength(0);
            _columns = matrix.GetLength(1);
            _matrix = matrix;
        }

        public int[] GetSize()
        {
            return new[] { _rows, _columns };
        }

        public double GetElement(int row, int column)
        {
            return _matrix[row,column];
        }

        public void SetElement(int row, int column, double value)
        {
            _matrix[row, column] = value;
        }

        public double[] GetRow(int rowIndex)
        {
            var row = new double[_columns];
            for (var i = 0; i < _columns; i++)
                row[i] = _matrix[rowIndex, i];
            return row;
        }

        public void SetRow(int row, double[] rowValue)
        {
            for (var i = 0; i < _columns; i++)
                _matrix[row, i] = rowValue[i];
        }

        public double[] GetColumn(int columnIndex)
        {
            var row = new double[_rows];
            for (var i = 0; i < _rows; i++)
                row[i] = _matrix[i, columnIndex];
            return row;
        }

        public void SetColumn(int column, double[] columnValue)
        {
            for (var i = 0; i < _rows; i++)
                _matrix[i, column] = columnValue[i];
        }
        
        public bool IsSquare()
        {
            return _rows == _columns;
        }

        public bool IsZero()
        {
            for (var i = 0; i < _rows; i++)
                for (var j = 0; j < _columns; j++)
                    if (_matrix[i, j] != 0)
                        return false;
            return true;
        }

        public bool IsDiagonal()
        {
            for (var i = 0; i < _rows; i++)
                for (var j = 0; j < _columns; j++)
                    if (i != j && _matrix[i, j] != 0)
                        return false;
            return true;
        }

        public bool IsIdentity()
        {
            for (var i = 0; i < _rows; i++)
                for (var j = 0; j < _columns; j++)
                    if (i == j && _matrix[i, j] != 1)
                        return false;
            return IsDiagonal();
        }

        public static Matrix operator +(Matrix lhs, Matrix rhs)
        {
            if (!AreSameSize(lhs, rhs))
            {
                throw new InvalidOperationException("The rows and columns must match in order to add two matrices together.");
            }
            var m = new Matrix(lhs._rows, lhs._columns);
            for (var i = 0; i < lhs._rows; i++)
                for (var j = 0; j < lhs._columns; j++)
                    m.SetElement(i,j, lhs.GetElement(i,j) + rhs.GetElement(i,j));
            return m;
        }

        public static Matrix operator -(Matrix lhs, Matrix rhs)
        {
            if (!AreSameSize(lhs, rhs))
            {
                throw new InvalidOperationException("The rows and columns must match in order to subtract two matrices.");
            }
            var m = new Matrix(lhs._rows, lhs._columns);
            for (var i = 0; i < lhs._rows; i++)
                for (var j = 0; j < lhs._columns; j++)
                    m.SetElement(i, j, lhs.GetElement(i, j) - rhs.GetElement(i, j));
            return m;
        }

        public static Matrix operator *(Matrix lhs, Matrix rhs)
        {
            if (CannotMultiply(lhs, rhs))
            {
                throw new InvalidOperationException("The columns of the lhs Matrix must match the rows of the rhs Matrix in order to perform multiplication on matrices.");
            }
            var m = new Matrix(lhs._rows, rhs._columns);
            for (var i = 0; i < lhs._rows; i++)
            {
                for (var j = 0; j < rhs._columns; j++)
                {
                    var row = lhs.GetRow(i);
                    var column = rhs.GetColumn(j);
                    var element = 0.0;
                    for (var index = 0; index < row.Length; index++)
                    {
                        element += row[index] * column[index];
                    }
                    m.SetElement(i, j, element);
                }
            }
            return m;
        }

        public static Matrix operator *(double multiplier, Matrix matrix)
        {
            var m = new Matrix(matrix._rows, matrix._columns);
            for (var i = 0; i < matrix._rows; i++)
                for (var j = 0; j < matrix._columns; j++)
                    m.SetElement(i, j, matrix.GetElement(i, j) * multiplier);
            return m;
        }

        public static Matrix operator *(Matrix matrix, double multiplier)
        {
            return multiplier*matrix;
        }

        public static bool operator ==(Matrix lhs, Matrix rhs)
        {
            return lhs.Equals(rhs);
        }

        public static bool operator !=(Matrix lhs, Matrix rhs)
        {
            return !lhs.Equals(rhs);
        }

        public bool Equals(Matrix other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            if (AreSameSize(this, other))
            {
                for (var i = 0; i < _rows; i++)
                    for (var j = 0; j < _columns; j++)
                        if (other._matrix[i, j] != _matrix[i, j])
                            return false;
                return true;
            }
            return false;
        }

        public override bool Equals(object obj)
        {
            return obj.GetType() == typeof(Matrix) && Equals((Matrix)obj);
        }

        // Re-sharper generated
        public override int GetHashCode()
        {
            unchecked
            {
                var result = (_matrix != null ? _matrix.GetHashCode() : 0);
                result = (result * 397) ^ _columns;
                result = (result * 397) ^ _rows;
                return result;
            }
        }

        public override string ToString()
        {
            var m = string.Empty;
            for (var i = 0; i < _rows; i++)
            {
                for (var j = 0; j < _columns; j++)
                {
                    m += string.Format("{0} ", GetElement(i, j));
                }
                m += Environment.NewLine;
            }
            return m;
        }

        public Matrix Transpose()
        {
            var m = new Matrix(_columns, _rows);
            for (var i = 0; i < _rows; i++)
            {
                m.SetColumn(i, GetRow(i));
            }
            return m;
        }

        public bool IsSymmetric()
        {
            return IsSquare() && this == Transpose();
        }

        private static bool AreSameSize(Matrix lhs, Matrix rhs)
        {
            return (lhs._rows == rhs._rows && lhs._columns == rhs._columns);
        }

        private static bool CannotMultiply(Matrix lhs, Matrix rhs)
        {
            return (lhs._columns != rhs._rows);
        }
    }
}
