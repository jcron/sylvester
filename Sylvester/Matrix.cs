﻿
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
            {
                for (var j = 0; j < columns; j++)
                {
                    _matrix[i, j] = 0;
                }
            }
        }

        public Matrix(double[,] matrix)
        {
            _rows = matrix.GetLength(0);
            _columns = matrix.GetLength(1);
            _matrix = matrix;
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
            {
                row[i] = _matrix[rowIndex, i];
            }
            return row;
        }

        public void SetRow(int row, double[] rowValue)
        {
            for (var i = 0; i < _columns; i++)
            {
                _matrix[row, i] = rowValue[i];
            }
        }

        public double[] GetColumn(int columnIndex)
        {
            var row = new double[_rows];
            for (var i = 0; i < _rows; i++)
            {
                row[i] = _matrix[i, columnIndex];
            }
            return row;
        }

        public void SetColumn(int column, double[] columnValue)
        {
            for (var i = 0; i < _rows; i++)
            {
                _matrix[i, column] = columnValue[i];
            }
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
            if (lhs._rows != rhs._rows || lhs._columns != rhs._columns)
            {
                throw new System.Exception("Create better exception");
            }
            return new Matrix(null);
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
            if (other._columns == _columns && other._rows == _rows)
            {
                for (var i = 0; i < _rows; i++)
                {
                    for (var j = 0; j < _columns; j++)
                    {
                        if (other._matrix[i, j] != _matrix[i, j])
                            return false;
                    }
                }
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
    }
}
