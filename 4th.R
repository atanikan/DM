n = c(100,1000,10000);
k = 1:100;
avgCount = 4;
resVec = matrix(rep(0,length(n) * length(k)), nrow = length(n), byrow = F);
index = 0;
for( i in n){
  index = index + 1;
  for(j in k){
    X = getDataMat(i,j,avgCount)
    dist = as.matrix(dist(X, method = "euclidean"))
    d_min = min(dist[lower.tri(dist)])
    d_max = max(dist[lower.tri(dist)])
    resVec[index,j] = log10((d_max - d_min) / d_min)
  }
}

plot(resVec[3,], type = "n", main = "R values for different n", xlab = "k", ylab = "r(k)");
points(resVec[1, ], col = "blue", pch = 19 );
points(resVec[2, ], col = "red", pch = 19 );
points(resVec[3, ], col = "purple", pch = 19 );
legend(75,8, legend = c("n = 100", "n = 1000", "n = 10000"), lwd=c(2.5,2.5),col=c("blue","red", "purple"));
getDataMat = function(i,j,avgCount){
  X = matrix(rep(0, i * j), nrow = i);
  for(k in 1:avgCount){
    X = X + matrix(runif(i * j, min=0, max=1), nrow = i);
  }
  X = X / avgCount;
}
