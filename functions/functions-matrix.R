#transposeMatrix
transposeMatrix = function(mat)
{
  t(mat);	
}

#rotateMatrix90(mat)
rotateMatrix90 = function(mat)
{
  t(apply(mat,2,rev));
}

#rotateMatrix180(mat)	
rotateMatrix180 = function(mat)
{
  rotateMatrix90(rotateMatrix90(mat));
}

#rotateMatrix270(mat)
rotateMatrix270 = function(mat)
{
  rotateMatrix180(rotateMatrix90(mat));
}

# 3x3 matrix ... ## matrix multiplication
matrixmultiplication = function(mat1,mat2)
{
  (mat1 %*% mat2);
}