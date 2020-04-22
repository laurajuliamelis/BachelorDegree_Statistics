##########################################################
###  Software Estadístic, 27.09.2016                   ###
###  Lecture 4: Creation and manipulation of matrices  ###
##########################################################


# Creation of matrices "by hand"
# ------------------------------
(A <- matrix(1:12, ncol = 4))
(AA <- matrix(1:12, nrow = 3, byrow = T))

# Some useful functions
# ---------------------
dim(A)
dim(A)[2]
nrow(A)
ncol(A)
length(A)

# Another example
# ---------------
B <- matrix(-5:4, ncol = 3)
B
dim(B)
t(B)

# Creation of matrices using existing ones
# ----------------------------------------
cbind(A, AA)
rbind(A, AA)
cbind(A, B)              # Error!
cbind(A, t(B))

# Deconstruction of matrices: We can deconstruct a matrix by applying
# the c() function, which combines all column vectors into one
# -------------------------------------------------------------------
c(A)
as.vector(AA)

# Subsetting matrices
# -------------------
A[2, 3]
A[, 1]
B[1:2, ]
A[c(1, 3), 2:3]
A[1, ] <- c(2, 17, 0, 3)
A


# --------------------------------------------------------------------------
# EXERCISE 1: Construct a matrix C from the combination of first two columns
# of matrix  A and the vector c(1, 2, -1)
# --------------------------------------------------------------------------
(C <- cbind(A[, 1:2], c(1, 2, -1)))


# Mathematical operations with matrices
# -------------------------------------
2*A
sqrt(A)
log(B)
A + AA
A*B               # R cannot handle this!
A*t(B)		      # Atention: This is NOT the classical matricial product
(C <- A %*% B)    # The matricial product
diag(C)		      # To elements of the main diagonal of a square matrix
det(C)		      # Determinant of a square matrix
qr(C)$rank		  # rank of a square matrix
diag(5)		      # To construct the identity matrix with dim = 5


# The application of binary operators return matrices with logicals
# -----------------------------------------------------------------
B > 0
sum(B > 0)
A < AA
which(A < AA)
which(A < AA, arr.ind = T)
C[upper.tri(C)]   # Extract upper triangular part of a square matrix
C[lower.tri(C)]   # Extract lower triangular part of a square matrix


# Statistical functions applied to matrices
# -----------------------------------------
sum(A)
mean(A)
median(B)
sd(B)


# The following functions treat each column as a variable
var(B)		# Variance-Covariance matrix (always symmetric)
cov(B)		# Variance-Covariance matrix (always symmetric)
cor(B)		# Correlation matrix (always symmetric with 1's in the diagonal)
summary(AA)	# Summary by columns

# Matrix row/column operations
# ----------------------------
rowSums(A)
rowMeans(A)
colSums(A)
colMeans(A)


# ----------------------------------------------------------------
# EXERCISE 2: Let consider the matrix D defined as:
D <- matrix(c(1:64), nrow = 8, byrow = TRUE)
# Extract the diagonal elements allocated above the main diagonal
# and the diagonal elements allocated under the main diagonal.
# Note: Functions col and row may be helpful.
# ----------------------------------------------------------------
# Upper-diagonal elements
u_diagonal <- D[col(D) - row(D) == 1]
u_diagonal

# Lower-diagonal elements
l_diagonal <- D[row(D) - col(D) == 1]
l_diagonal

# ------------------------------------------------------
# EXERCISE 3: Use the same matrix D to extract
#             the reverse diagonal elements.
# ------------------------------------------------------
D[row(D) + col(D) == nrow(D) + 1]


# Inverse of matrices: Function solve()
# -------------------------------------
A^(-1)				      # This is NOT the inverse of matrix A
solve(A)				  # Matrix A does not have an inverse since it is not square
C^(-1)				      # This is NOT the inverse of matrix C
solve(C)				  # Inverse of matrix C
round(C %*% solve(C), 2)  # Proof


# Row and column names
# --------------------
rownames(A) # NULL
colnames(A) # NULL

rownames(A) <- paste("Fila", 1:3)
colnames(A) <- paste0("Col.", 1:ncol(A))
A

A["Fila 2", ]
A[2, ]

A["Fila 2", "Col.4"]
A[2, 4]


# All elements of a matrix are of the same type
# ----------------------------------------------
D <- matrix(c("Joan", "Rosa", "Laura", "Miguel", 32, 30, 5, 2), ncol = 2)
D
class(D[, 2]) # character

v <- as.factor(c("Joan", "Rosa", "Laura", "Miguel"))
D <- matrix(c(v, 32, 30, 5, 2), ncol = 2)
D
class(D[, 1]) # numeric

# Working with numeric and categorical variabeles, we have to use data frames
# ---------------------------------------------------------------------------
D <- data.frame(names = v, values = c(32, 30, 5, 2))

# ----------------------------------------------------------
# EXERCISE 4: Solve the following system of linear equations
# in five unknowns: X <- c(x1, x2, x3, x4, x5)
# ----------------------------------------------------------

#       2x2 + 2x3 + 2x4 + 2x5 =  0
# 2x1 +       2x3 + 2x4 + 2x5 =  1
# 2x1 + 2x2 +       2x4 + 2x5 =  0
# 2x1 + 2x2 + 2x3 +       2x5 = -1
# 2x1 + 2x2 + 2x3 + 2x4       =  1

# Solution: X = (1/8, -3/8, 1/8, 5/8, -3/8)
# ----------------------------------------------------------
