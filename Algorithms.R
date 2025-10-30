# SORTING ALGORITHMS AND DATA STRUCTURES
#
# =============================================================================
# HEAPSORT (From: Alg 3-Árbol binario.pdf)
# =============================================================================
# MAX-HEAPIFY - Maintains the max-heap property
# BUILD-MAX-HEAP - Builds a max-heap from an array
# MAXIMUM(S) - Returns the maximum element
# EXTRACT-MAX(S) - Extracts and returns the maximum element
# INCREASE-KEY(S, x, k) - Increases the key value of element x
# INSERT(S, x) - Inserts a new element x into the heap

# --- Helper functions for 1-based indexing ---
parent <- function(i) {
  return(floor(i / 2))
}

left <- function(i) {
  return(2 * i)
}

right <- function(i) {
  return(2 * i + 1)
}

# --- Corrected MAX-HEAPIFY and BUILD-MAX-HEAP ---

max_heapify_r <- function(A, i) {
  l <- left(i)
  r <- right(i)
  largest <- i
  
  if (l <= A$heap_size && A$vec[l] > A$vec[i]) {
    largest <- l
  }
  if (r <= A$heap_size && A$vec[r] > A$vec[largest]) {
    largest <- r
  }
  
  if (largest != i) {
    temp <- A$vec[i]
    A$vec[i] <- A$vec[largest]
    A$vec[largest] <- temp
    A <- max_heapify_r(A, largest) # Recursive call
  }
  return(A) # Return the modified list
}

build_max_heap_r <- function(A_vec) {
  A <- list(vec = A_vec, heap_size = length(A_vec))
  
  for (i in floor(A$heap_size / 2):1) {
    A <- max_heapify_r(A, i)
  }
  return(A)
}

# --- HEAPSORT ---
heapsort <- function(A_vec) {
  A <- build_max_heap_r(A_vec)
  
  for (i in length(A$vec):2) {
    # Swap A[1] and A[i]
    temp <- A$vec[1]
    A$vec[1] <- A$vec[i]
    A$vec[i] <- temp
    
    # Reduce heap size
    A$heap_size <- A$heap_size - 1
    
    # Fix the heap
    A <- max_heapify_r(A, 1)
  }
  return(A$vec)
}

# --- Priority Queue Operations ---

heap_maximum <- function(A) {
  return(A$vec[1])
}

heap_extract_max <- function(A) {
  if (A$heap_size < 1) {
    stop("Heap underflow")
  }
  max_val <- A$vec[1]
  A$vec[1] <- A$vec[A$heap_size]
  A$heap_size <- A$heap_size - 1
  A <- max_heapify_r(A, 1)
  
  # Trim the underlying vector for clarity, though not strictly necessary
  A$vec <- A$vec[1:A$heap_size] 
  
  print(paste("Extracted:", max_val))
  return(A)
}

heap_increase_key <- function(A, i, key) {
  if (key < A$vec[i]) {
    stop("New key is smaller than current key")
  }
  A$vec[i] <- key
  while (i > 1 && A$vec[parent(i)] < A$vec[i]) {
    # Swap A[i] and A[parent(i)]
    temp <- A$vec[i]
    A$vec[i] <- A$vec[parent(i)]
    A$vec[parent(i)] <- temp
    
    i <- parent(i)
  }
  return(A)
}

max_heap_insert <- function(A, key) {
  A$heap_size <- A$heap_size + 1
  # Extend the vector if needed
  if (length(A$vec) < A$heap_size) {
    A$vec[A$heap_size] <- -Inf # Add placeholder
  } else {
    # If vector was trimmed, just use the new size
    A$vec <- c(A$vec[1:(A$heap_size-1)], -Inf)
  }
  
  A <- heap_increase_key(A, A$heap_size, key)
  return(A)
}

# --- Example Usage ---
print("--- HEAPSORT ---")
vec_heap <- c(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
sorted_vec_heap <- heapsort(vec_heap)
print(paste("Heapsort:", paste(sorted_vec_heap, collapse=",")))

# Priority Queue Example
heap <- build_max_heap_r(vec_heap)
print(paste("Max:", heap_maximum(heap)))
heap <- heap_extract_max(heap)
heap <- max_heap_insert(heap, 20)
print(paste("New heap vec:", paste(heap$vec, collapse=",")))


# =============================================================================
# QUICKSORT (From: Alg 4 - Quicksort.pdf)
# =============================================================================
# QUICKSORT(A, p, r) - Sorts the subarray A[p..r]
# PARTITION(A, p, r) - Partitions the array and returns the pivot index

# --- PARTITION ---
# A is an environment containing the vector $vec
# This allows in-place modification
partition <- function(A_env, p, r) {
  x <- A_env$vec[r] # Pivot
  i <- p - 1
  
  for (j in p:(r - 1)) {
    if (A_env$vec[j] <= x) {
      i <- i + 1
      # Swap A[i] and A[j]
      temp <- A_env$vec[i]
      A_env$vec[i] <- A_env$vec[j]
      A_env$vec[j] <- temp
    }
  }
  
  # Swap A[i+1] and A[r] (the pivot)
  temp <- A_env$vec[i + 1]
  A_env$vec[i + 1] <- A_env$vec[r]
  A_env$vec[r] <- temp
  
  return(i + 1)
}

# --- QUICKSORT ---
# A_env is the environment, p and r are start/end indices
quicksort_recursive <- function(A_env, p, r) {
  if (p < r) {
    q <- partition(A_env, p, r)
    quicksort_recursive(A_env, p, q - 1)
    quicksort_recursive(A_env, q + 1, r)
  }
  # No return value needed, A_env$vec is modified
}

# --- Wrapper Function ---
quicksort <- function(A_vec) {
  A_env <- new.env()
  A_env$vec <- A_vec
  
  quicksort_recursive(A_env, 1, length(A_vec))
  
  return(A_env$vec)
}

# --- Example Usage ---
print("--- QUICKSORT ---")
vec_quick <- c(2, 8, 7, 1, 3, 5, 6, 4)
sorted_vec_quick <- quicksort(vec_quick)
print(paste("Quicksort:", paste(sorted_vec_quick, collapse=",")))


# =============================================================================
# SELECTION ALGORITHMS (From: Alg 9 Orden Mediano.pdf)
# =============================================================================
# MINIMUM(A) - Finds the minimum element
# RANDOMIZED-SELECT(A, p, r, i) - Finds the i-th smallest element
# RANDOMIZED-PARTITION(A, p, r) - Random partition for randomized-select
# SELECT - Worst-case linear time selection

# --- MINIMUM ---
minimum <- function(A) {
  min_val <- A[1]
  for (i in 2:length(A)) {
    if (min_val > A[i]) {
      min_val <- A[i]
    }
  }
  return(min_val)
}

# --- RANDOMIZED-PARTITION ---
# (Used by Randomized-Select)
randomized_partition <- function(A_env, p, r) {
  # Pick a random pivot
  i <- sample(p:r, 1)
  
  # Swap A[i] with A[r]
  temp <- A_env$vec[r]
  A_env$vec[r] <- A_env$vec[i]
  A_env$vec[i] <- temp
  
  # Use the standard partition function (defined in Quicksort section)
  return(partition(A_env, p, r))
}

# --- RANDOMIZED-SELECT (Recursive) ---
randomized_select_recursive <- function(A_env, p, r, i) {
  if (p == r) {
    return(A_env$vec[p])
  }
  
  q <- randomized_partition(A_env, p, r)
  k <- q - p + 1 # Number of elements in the low side + pivot
  
  if (i == k) {
    return(A_env$vec[q]) # Pivot is the answer
  } else if (i < k) {
    return(randomized_select_recursive(A_env, p, q - 1, i))
  } else {
    return(randomized_select_recursive(A_env, q + 1, r, i - k))
  }
}

# --- Wrapper for RANDOMIZED-SELECT ---
randomized_select <- function(A_vec, i) {
  if (i < 1 || i > length(A_vec)) {
    stop("i is out of bounds")
  }
  A_env <- new.env()
  A_env$vec <- A_vec # We can use a copy
  return(randomized_select_recursive(A_env, 1, length(A_vec), i))
}

# --- Example Usage ---
print("--- RANDOMIZED SELECT ---")
vec_select <- c(3, 2, 9, 0, 7, 5, 4, 8, 6, 1)
print(paste("Minimum:", minimum(vec_select))) # Should be 0
print(paste("3rd smallest (i=3):", randomized_select(vec_select, 3))) # Should be 2
print(paste("Median (i=5):", randomized_select(vec_select, 5))) # Should be 4


# --- SELECT (Worst-case Linear Time) ---

# Helper: Find median of a small vector (at most 5 elements)
find_median_of_5 <- function(v) {
  return(sort(v)[ceiling(length(v) / 2)])
}

# Helper: Partition, but using a specific pivot value
partition_around_pivot <- function(A_vec, pivot_val) {
  # Find index of pivot
  pivot_index <- which(A_vec == pivot_val)[1]
  
  # Move pivot to end
  A_env <- new.env()
  A_env$vec <- A_vec
  r <- length(A_vec)
  
  temp <- A_env$vec[r]
  A_env$vec[r] <- A_env$vec[pivot_index]
  A_env$vec[pivot_index] <- temp
  
  # Standard partition (from Quicksort section)
  q <- partition(A_env, 1, r)
  return(list(vec = A_env$vec, pivot_index = q))
}

select_recursive <- function(A_vec, i) {
  n <- length(A_vec)
  if (n <= 5) {
    return(sort(A_vec)[i])
  }
  
  # 1. Divide into groups of 5
  groups <- split(A_vec, ceiling(seq_along(A_vec) / 5))
  
  # 2. Find median of each group
  medians <- sapply(groups, find_median_of_5)
  
  # 3. Find median of medians (x)
  median_of_medians <- select_recursive(medians, ceiling(length(medians) / 2))
  
  # 4. Partition around median-of-medians (x)
  partition_result <- partition_around_pivot(A_vec, median_of_medians)
  A_partitioned <- partition_result$vec
  k <- partition_result$pivot_index # k is the rank (index) of the pivot
  
  # 5. Recurse
  if (i == k) {
    return(median_of_medians)
  } else if (i < k) {
    # Recurse on the "low" side
    return(select_recursive(A_partitioned[1:(k - 1)], i))
  } else {
    # Recurse on the "high" side
    return(select_recursive(A_partitioned[(k + 1):n], i - k))
  }
}

# --- Wrapper for SELECT ---
select_linear <- function(A_vec, i) {
  if (i < 1 || i > length(A_vec)) {
    stop("i is out of bounds")
  }
  return(select_recursive(A_vec, i))
}

# --- Example Usage ---
print("--- LINEAR SELECT ---")
vec_linear_select <- c(3, 2, 9, 0, 7, 5, 4, 8, 6, 1, 10, 12, 11, 15, 14)
print(paste("8th smallest (i=8):", select_linear(vec_linear_select, 8))) # Should be 7
print(paste("True median (R):", median(vec_linear_select)))
print(paste("1st smallest (i=1):", select_linear(vec_linear_select, 1))) # Should be 0


# =============================================================================
# LINKED LISTS (From: Alg 10 Estructuras.pdf)
# =============================================================================
# LIST-SEARCH(L, k) - Searches for an element with key k in list L
# LIST-INSERT(L, x) - Inserts element x into list L
# LIST-DELETE(L, x) - Deletes element x from list L

# --- Node Constructor ---
create_node <- function(key) {
  node <- new.env()
  node$key <- key
  node$next_node <- NULL
  node$prev_node <- NULL
  return(node)
}

# --- List Constructor ---
# The list object just holds a pointer to the head
create_list <- function() {
  L <- new.env()
  L$head <- NULL
  return(L)
}

# --- LIST-INSERT (Inserts at the head) ---
list_insert <- function(L, x_node) {
  x_node$next_node <- L$head
  if (!is.null(L$head)) {
    L$head$prev_node <- x_node
  }
  L$head <- x_node
  x_node$prev_node <- NULL
  # No return needed, L and x_node are modified
}

# --- LIST-SEARCH ---
list_search <- function(L, k) {
  x <- L$head
  while (!is.null(x) && x$key != k) {
    x <- x$next_node
  }
  return(x) # Returns the node (env) or NULL
}

# --- LIST-DELETE ---
list_delete <- function(L, x_node) {
  if (!is.null(x_node$prev_node)) {
    x_node$prev_node$next_node <- x_node$next_node
  } else {
    # This node is the head
    L$head <- x_node$next_node
  }
  
  if (!is.null(x_node$next_node)) {
    x_node$next_node$prev_node <- x_node$prev_node
  }
  # x_node is now "floating"
}

# --- Helper to print list ---
print_list <- function(L) {
  vals <- c()
  x <- L$head
  while (!is.null(x)) {
    vals <- c(vals, x$key)
    x <- x$next_node
  }
  print(paste(vals, collapse=" <-> "))
}

# --- Example Usage ---
print("--- LINKED LIST ---")
L <- create_list()
n1 <- create_node(1)
n4 <- create_node(4)
n9 <- create_node(9)
n16 <- create_node(16)

list_insert(L, n1)
list_insert(L, n4)
list_insert(L, n9)
list_insert(L, n16)
print_list(L) # Should be: 16 <-> 9 <-> 4 <-> 1

found_node <- list_search(L, 9)
print(paste("Found node with key:", found_node$key))

list_delete(L, found_node)
print_list(L) # Should be: 16 <-> 4 <-> 1


# =============================================================================
# STACKS AND QUEUES (From: Alg 11 PilasColas.pdf)
# =============================================================================
# STACK-EMPTY(S) - Checks if stack S is empty
# PUSH(S, x) - Inserts x into stack S
# POP(S) - Removes and returns the top element from stack S
# Additional operations mentioned:
#    enqueue - Insert into queue
#    dequeue - Remove from queue
#    isEmpty - Check if empty
#    isFull - Check if full

# --- Stack Constructor ---
create_stack <- function(size = 100) {
  S_env <- new.env()
  S_env$S <- vector(mode = "any", length = size) # Pre-allocate array
  S_env$top <- 0
  return(S_env)
}

# --- STACK-EMPTY ---
stack_empty <- function(S_env) {
  return(S_env$top == 0)
}

# --- PUSH ---
push <- function(S_env, x) {
  if (S_env$top == length(S_env$S)) {
    stop("Stack overflow")
  }
  S_env$top <- S_env$top + 1
  S_env$S[S_env$top] <- x
}

# --- POP ---
pop <- function(S_env) {
  if (stack_empty(S_env)) {
    stop("Stack underflow")
  }
  S_env$top <- S_env$top - 1
  return(S_env$S[S_env$top + 1])
}

# --- Example Usage ---
print("--- STACK ---")
S <- create_stack(10)
push(S, 4)
push(S, 1)
push(S, 3)
print(paste("Popped:", pop(S))) # 3
print(paste("Popped:", pop(S))) # 1
print(paste("Is empty:", stack_empty(S))) # FALSE
print(paste("Popped:", pop(S))) # 4
print(paste("Is empty:", stack_empty(S))) # TRUE


# --- Queue Constructor ---
create_queue <- function(size = 10) {
  Q_env <- new.env()
  # Allocate one extra space to differentiate full from empty
  Q_env$Q <- vector(mode = "any", length = size + 1) 
  Q_env$head <- 1
  Q_env$tail <- 1
  Q_env$size <- size + 1
  return(Q_env)
}

# --- QUEUE-EMPTY (isEmpty) ---
queue_empty <- function(Q_env) {
  return(Q_env$head == Q_env$tail)
}

# --- QUEUE-FULL (isFull) ---
queue_full <- function(Q_env) {
  # Wraps around
  next_tail <- (Q_env$tail %% Q_env$size) + 1
  return(next_tail == Q_env$head)
}

# --- ENQUEUE ---
enqueue <- function(Q_env, x) {
  if (queue_full(Q_env)) {
    stop("Queue overflow")
  }
  Q_env$Q[Q_env$tail] <- x
  
  # Move tail, wrapping around
  Q_env$tail <- (Q_env$tail %% Q_env$size) + 1
}

# --- DEQUEUE ---
dequeue <- function(Q_env) {
  if (queue_empty(Q_env)) {
    stop("Queue underflow")
  }
  x <- Q_env$Q[Q_env$head]
  
  # Move head, wrapping around
  Q_env$head <- (Q_env$head %% Q_env$size) + 1
  return(x)
}

# --- Example Usage ---
print("--- QUEUE ---")
Q <- create_queue(3) # Can hold 3 items
enqueue(Q, 1)
enqueue(Q, 2)
enqueue(Q, 3)
print(paste("Is full:", queue_full(Q))) # TRUE
# enqueue(Q, 4) # This would error
print(paste("Dequeued:", dequeue(Q))) # 1
enqueue(Q, 4) # Now there is space
print(paste("Dequeued:", dequeue(Q))) # 2
print(paste("Dequeued:", dequeue(Q))) # 3
print(paste("Dequeued:", dequeue(Q))) # 4
print(paste("Is empty:", queue_empty(Q))) # TRUE


# =============================================================================
# LINEAR TIME SORTING (From: Sem10s1 OrdenLineal.pdf)
# =============================================================================
# COUNTING-SORT(A, B, k) - Counting sort algorithm
# RADIX-SORT(A, d) - Radix sort algorithm

# --- COUNTING-SORT ---
# Note: R is 1-based, so array C for k=5 must hold indices 0..5.
# We make C 1-based with k+1 elements.
counting_sort <- function(A, k) {
  B <- vector(mode = "integer", length = length(A))
  
  # C must be of size k+1 to hold counts for 0..k
  # In R, indices are 1-based, so we'll index C from 1 to k+1
  # C[i] will store the count of (i-1)
  C <- vector(mode = "integer", length = k + 1)
  
  # 1. Initialize C to all zeros (Already done by R)
  
  # 2. Count occurrences
  for (j in 1:length(A)) {
    val <- A[j]
    C[val + 1] <- C[val + 1] + 1
  }
  # C[i] now contains the number of elements equal to (i-1)
  
  # 3. Sum up counts
  for (i in 2:(k + 1)) {
    C[i] <- C[i] + C[i - 1]
  }
  # C[i] now contains the number of elements <= (i-1)
  
  # 4. Place elements into B
  for (j in length(A):1) {
    val <- A[j]
    B[C[val + 1]] <- val
    C[val + 1] <- C[val + 1] - 1
  }
  
  return(B)
}

# --- Example Usage ---
print("--- COUNTING SORT ---")
vec_count <- c(2, 5, 3, 0, 2, 3, 0, 3)
k <- 5 # Max value
sorted_vec_count <- counting_sort(vec_count, k)
print(paste("Counting Sort:", paste(sorted_vec_count, collapse=",")))


# --- Helper for Radix Sort: Counting sort on a specific digit ---
# (d is the digit number, 1s, 10s, 100s...)
counting_sort_by_digit <- function(A, d) {
  n <- length(A)
  B <- vector(mode = "integer", length = n)
  
  # Digits are 0-9. C is size 10 (indices 0..9)
  # We'll use R indices 1..10
  C <- vector(mode = "integer", length = 10)
  
  # Get the d-th digit of each number
  # (A[j] %/% 10^(d-1)) %% 10
  
  for (j in 1:n) {
    digit <- (A[j] %/% (10^(d-1))) %% 10
    C[digit + 1] <- C[digit + 1] + 1
  }
  
  # Sum up counts
  for (i in 2:10) {
    C[i] <- C[i] + C[i - 1]
  }
  
  # Place elements
  for (j in n:1) {
    digit <- (A[j] %/% (10^(d-1))) %% 10
    B[C[digit + 1]] <- A[j]
    C[digit + 1] <- C[digit + 1] - 1
  }
  
  return(B)
}

# --- RADIX-SORT ---
radix_sort <- function(A, d_max) {
  # d_max is the number of digits in the largest number
  A_sorted <- A
  for (d in 1:d_max) {
    A_sorted <- counting_sort_by_digit(A_sorted, d)
  }
  return(A_sorted)
}

# --- Example Usage ---
print("--- RADIX SORT ---")
vec_radix <- c(329, 457, 657, 839, 436, 720, 355)
d_max <- 3 # All are 3-digit numbers
sorted_vec_radix <- radix_sort(vec_radix, d_max)
print(paste("Radix Sort:", paste(sorted_vec_radix, collapse=",")))


# =============================================================================
# BINARY SEARCH TREES (From: Alg 12 ABB.pdf)
# =============================================================================
# INORDER-TREE-WALK(x) - In-order traversal of tree with root x
# TREE-SEARCH(x, k) - Recursive search in the tree
# ITERATIVE-TREE-SEARCH(x, k) - Iterative search in the tree
# TREE-MINIMUM(x) - Finds the minimum element in subtree with root x
# TREE-MAXIMUM(x) - Finds the maximum element in subtree with root x

# --- Node Constructor for BST ---
create_bst_node <- function(key) {
  node <- new.env()
  node$key <- key
  node$p <- NULL    # Parent
  node$left <- NULL
  node$right <- NULL
  return(node)
}

# --- TREE-INSERT ---
# (Not in the list, but needed to build a tree)
tree_insert <- function(T_root_env, z_node) {
  y <- NULL
  x <- T_root_env$root
  
  while (!is.null(x)) {
    y <- x # y tracks the parent
    if (z_node$key < x$key) {
      x <- x$left
    } else {
      x <- x$right
    }
  }
  
  z_node$p <- y
  if (is.null(y)) {
    T_root_env$root <- z_node # Tree was empty
  } else if (z_node$key < y$key) {
    y$left <- z_node
  } else {
    y$right <- z_node
  }
}

# --- INORDER-TREE-WALK ---
inorder_tree_walk <- function(x_node) {
  if (!is.null(x_node)) {
    inorder_tree_walk(x_node$left)
    print(x_node$key)
    inorder_tree_walk(x_node$right)
  }
}

# --- TREE-SEARCH (Recursive) ---
tree_search <- function(x_node, k) {
  if (is.null(x_node) || k == x_node$key) {
    return(x_node)
  }
  if (k < x_node$key) {
    return(tree_search(x_node$left, k))
  } else {
    return(tree_search(x_node$right, k))
  }
}

# --- ITERATIVE-TREE-SEARCH ---
iterative_tree_search <- function(x_node, k) {
  while (!is.null(x_node) && k != x_node$key) {
    if (k < x_node$key) {
      x_node <- x_node$left
    } else {
      x_node <- x_node$right
    }
  }
  return(x_node)
}

# --- TREE-MINIMUM ---
tree_minimum <- function(x_node) {
  while (!is.null(x_node$left)) {
    x_node <- x_node$left
  }
  return(x_node)
}

# --- TREE-MAXIMUM ---
tree_maximum <- function(x_node) {
  while (!is.null(x_node$right)) {
    x_node <- x_node$right
  }
  return(x_node)
}

# --- Example Usage ---
print("--- BINARY SEARCH TREE ---")
# Create a root environment
T_root_env <- new.env()
T_root_env$root <- NULL

# Keys to insert
keys <- c(15, 6, 18, 3, 7, 17, 20, 2, 4, 13, 9)

for (k in keys) {
  tree_insert(T_root_env, create_bst_node(k))
}

print("Inorder-Tree-Walk:")
inorder_tree_walk(T_root_env$root)

print("Find minimum:")
min_node <- tree_minimum(T_root_env$root)
print(min_node$key) # 2

print("Find 13 (iterative):")
found_node_bst <- iterative_tree_search(T_root_env$root, 13)
print(found_node_bst$key)

print("Find 16 (recursive):")
not_found_node <- tree_search(T_root_env$root, 16)
print(paste("Is 16 found?", !is.null(not_found_node))) # FALSE
