# Lab Session 01 - Introduction to R --------------------------------------

# Basic Commands ----------------------------------------------------------

# `=`, `<-`, `->` or `<--`: Variable assignment.
junk1 = sin(2^5 + 7 * sqrt(4) / log(10))
junk2 <- sin(2^5 + 7 * sqrt(4) / log(10))
sin(2^5 + 7 * sqrt(4) / log(10)) -> junk3
junk <-- sin(2^5 + 7 * sqrt(4) / log(10))

# `?` or `??""`: Invoke the Help environment.
?sin
??"sqrt"

# `rm`: Remove variables from an environment.
rm(junk1, junk2, junk3, junk)
?rm

# Basic Datatypes ---------------------------------------------------------

# `typeof`: Look at an object's datatype. 
foo <- 2
typeof(foo)

# `.is`: Check whether a variable stores a certain datatype.
# `.as`: Convert a value to a chosen datatype.
foo2 <- as.integer(foo)
typeof(foo2)

# Vectors -----------------------------------------------------------------

# A vector can be constructed using the `c` function.
# Note: It is good practice to store values of the same datatype in a vector.
age <- c(49, 26, 22, 21.5, 23)
typeof(age)

# A first way of selecting items is by indicating indices.
age[1] # Take the element in the first position.
age[c(1, 4)] # Take the elements in positions 1 and 4.

# Indices can also be constructed using the `start:end` notation.
1:3
4:-2 # Reverse order.
4 + 1:7 # Add 4 to each element in the sequence.
(4 + 1):7 # Start from 4 + 1 = 5.

# Alternatively, it is possible to select items using a Boolean mask.
idx <- (age > 29) # Check whether the items in age are > 29.
typeof(idx)
which(idx) # Indices that are True.
which(!idx) # Indices that are False.
age[idx] # Selection via Boolean mask.

# Lastly, one can select items by name, focusing on specific attributes.
# It is possible to add attributes to a collection using the `$` symbol.
names(age)
?NULL # Other particular values include NaN or NA.

age2 <- c(alice = 49, bob = 26)
age2
typeof(age2)
names(age2)
age2["alice"] # Selection by name requires using strings for indexing.

# Common operations on vectors include length and element-wise operations.
# Note: Most operations can be performed only on objects of the same size.
length(age)
age / age
age * age
sqrt(age)

# Matrices ----------------------------------------------------------------

# A matrix can be seen as a multi-dimensional generalization of vectors.
# By default, the `matrix` constructor creates 1x1 matrices of missing values.
?matrix # `matrix` is a subclass of the general `array`.

# Create a zero matrix.
matrix(0, 2, 3) # 2x3 matrix.
matrix(0, ncol = 2, nrow = 3) # 3x2 matrix.

# Create a matrix by replicating a previously defined vector.
age_matrix <- matrix(age, 5, 3)
age_matrix
t(age_matrix) # Transpose a matrix using the `t` operator.

# Selecting items from a matrix is done by specifying indices for each dimension.
# Note: One extract an entire dimension by indicating its index only.
age_matrix[1, 1]
age_matrix[1:3, 2:3]
age_matrix[, 1] # Extract Row 1.
age_matrix[2, ] # Extract Row 2

# Matrix operations include both element-wise operations and matrix operations.
age_matrix * age_matrix # Element-wise multiplication.
age_matrix %*% t(age_matrix) # Matrix multiplication, if compatible.

# Dataframes --------------------------------------------------------------

# A dataframe can be seen as a table containing different types of data.
age
surname <- c("fields", "clarks", "turing", "lovelace", "bayes")

# One can use the `cbind` or `rbind` functions to combine vectors into a dataframe.
# Note: Doing so can cause problems with type casting.
try1 <- cbind(surname, age)
try1

# Since using `cbind` or `rbind` can cause problems with type casting,
# dataframes are typically created using the `data.frame` function.
try2 <- data.frame(surname, age)
try2

# In particular, it is possible to look at a specific column using selection.
# Note: Columns in a dataframe must have unique names.
try2$age
try2[,2]
try2[, "age"]
try2[[2]] # List-based selection for dataframes.
