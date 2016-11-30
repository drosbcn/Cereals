library('lars')

t = cereals[grep('(tot_sales)', colnames(cereals))]
X = cereals[grep('[^tot_sales]*', colnames(cereals))]
X = as.matrix(select_if(X, is.numeric))

G = t(X) %*% X

lasso = lars(X, t, 
     type = c("lasso"),
     trace = TRUE, 
     normalize = TRUE, 
     intercept = FALSE, G)

# plot(lasso)
lasso.coefs = tail(coef(lasso),1)
lasso.nz.coefs = lasso.coefs[,colnames(lasso.coefs)[abs(lasso.coefs) > 10**-6]]
colnames(lasso.nz.coefs)
