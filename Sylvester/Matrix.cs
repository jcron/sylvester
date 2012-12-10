
using System;

namespace Sylvester
{
    public class Matrix
    {
        private double[,] _matrix;
        private int _columns;
        public Matrix(int rows, int columns)
        {
            _matrix = new double[rows,columns];
            _columns = columns;
            for (var i = 0; i < rows; i++)
            {
                for (var j = 0; j < columns; j++)
                {
                    _matrix[i, j] = 0;
                }
            }
        }

        public double GetElement(int row, int column)
        {
            return _matrix[row,column];
        }

        public void SetElement(int row, int column, double value)
        {
            _matrix[row, column] = value;
        }

        public void SetElements(int row, double[] rowValue)
        {
            for (var i = 0; i < _columns; i++)
            {
                _matrix[row, i] = rowValue[i];
            }
        }
    }
}
