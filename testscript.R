
# random matrices are invertible with probability 1
# so we shouldn't have to worry about this test case
# failing due to a non invertible matrix.
testrand <- matrix(data=rnorm(9), nrow=3, ncol=3)
while(det(testrand) == 0){
  testrand <- matrix(data=rnorm(9), nrow=3, ncol=3)
}

testrand_c <- makeCacheMatrix(testrand)
cacheSolve(testrand_c)
testrand_c$get()
testrand_c$getinv()
stopifnot(
  identical(testrand, testrand_c$get()),
  identical(solve(testrand),testrand_c$getinv()))


testbig <- matrix(data=rnorm(10000), nrow=100, ncol=100)
while(det(testbig) == 0){
  testbig <- matrix(data=rnorm(10000), nrow=100, ncol=100)
}
testbig_c <- makeCacheMatrix(testbig)

stopifnot(
  identical(cacheSolve(testbig_c), solve(testbig)),
  identical(cacheSolve(testbig_c), solve(testbig)),
  identical(testbig_c$getinv(), solve(testbig)),
  identical(testbig, testbig_c$get()))

print('Passes all tests :)')

