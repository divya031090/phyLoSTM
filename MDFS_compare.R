#' Max information gains
#'
#' @param acceleration.type acceleration type
#'   ('scalar' for none, 'avx'/'avx2' for use of the AVX/AVX2 instruction set respectively, 'cuda' for CUDA)
#' @param dimensions number of dimensions
#' @param divisions number of divisions
#' @param discretizations number of discretizations
#' @param seed seed for PRNG used during discretizations
#' @param range discretization range (from 0.0 to 1.0)
#' @param pseudo.count pseudo count
#' @param reduce.method discretization reduce method (either "max" or "mean")
#' @param data input data where columns are variables and rows are observations
#' @param decision decision variable as a boolean vector of length equal to number of observations
#' @return numeric vector with max information gain for each input variable
#' @examples
#'   ComputeMaxInfoGains(data = madelon$data, decision = madelon$decision,
#'     discretizations = 1, range = 0, divisions = 22, dimensions = 1)
#' @export
#' @useDynLib CuCubes CuCubes
ComputeMaxInfoGains <- function(
    acceleration.type = 'scalar',
    dimensions = 1,
    divisions = 1,
    discretizations = 1,
    seed = 0,
    range = 1.0,
    pseudo.count = 0.001,
    reduce.method = 'max',
    data,
    decision) {
  n <- length(decision)
  k <- ncol(data)

  if (pseudo.count <= 0) {
    stop('Pseudo count has to be strictly greater than 0.')
  }

  if (n != nrow(data)) {
    stop('Length of decision is not equal to the number of rows in data.')
  }

  if (!all(decision == 0 || decision == 1)) {
    stop('Decision must be a vector of 0s and 1s only.')
  }

  if (reduce.method == 'max') {
    reduce.method.int = 0
  } else if (reduce.method == 'mean') {
    reduce.method.int = 1
  } else {
    stop('Unknown reduce.method')
  }

  if (acceleration.type == 'scalar') {
    acceleration.type.int = 0
  } else if (acceleration.type == 'avx') {
    if (discretizations %% 4 != 0) {
      stop('AVX Error: Number of discretizations must be a multiple of 4')
    }
    acceleration.type.int = 1
  } else if (acceleration.type == 'avx2') {
    if (discretizations %% 8 != 0) {
      stop('AVX2 Error: Number of discretizations must be a multiple of 8')
    }
    acceleration.type.int = 2
  } else if (acceleration.type == 'cuda') {
    if (dimensions == 1) {
      stop('CUDA-accelerated CuCubes does not work in 1 dimension')
    }
  } else {
    stop('Unknown acceleration.type')
  }

  if (acceleration.type == 'cuda') {
    rst <- .C(
      'r_cucubes',
      as.integer(n),                     # n
      as.integer(k),                     # k
      as.integer(dimensions),            # dim
      as.integer(divisions),             # div
      as.integer(discretizations),       # disc
      as.integer(seed),                  # seed
      as.double(range),                  # range
      as.double(pseudo.count),           # pseudo_count
      as.integer(reduce.method.int),     # reduce_method
      as.integer(FALSE),
      as.integer(16),
      as.integer(rep(TRUE, 16)),
      as.double(data),                   # data
      as.integer(decision),              # decision
      out=double(length=k))              # IG max output
  } else {
    rst <- .C(
      CuCubes,
      as.integer(acceleration.type.int), # acceleration type
      as.integer(0),                     # output type (0 for max IGs)
      as.integer(n),                     # n
      as.integer(k),                     # k
      as.integer(dimensions),            # dim
      as.integer(divisions),             # div
      as.integer(discretizations),       # disc
      as.integer(seed),                  # seed
      as.double(range),                  # range
      as.double(pseudo.count),           # pseudo_count
      as.integer(reduce.method.int),     # reduce_method
      as.double(0),                      # ig_thr (ignored)
      integer(length=0),                 # interesting_vars (ignored)
      as.integer(0),                     # interesting_vars_count (ignored)
      as.double(data),                   # data
      as.integer(decision),              # decision
      out=double(length=k))              # IG max output
  }

  return(rst$out)
}

#' Interesting tuples
#'
#' @param acceleration.type acceleration type ('scalar' for none, 'avx'/'avx2' for use of the AVX/AVX2 instruction set respectively)
#' @param dimensions number of dimensions
#' @param divisions number of divisions
#' @param discretizations number of discretizations
#' @param seed seed for PRNG used during discretizations
#' @param range discretization range (from 0.0 to 1.0)
#' @param pseudo.count pseudo count
#' @param reduce.method discretization reduce method (either "max" or "mean")
#' @param ig.thr IG threshold above which the tuple is interesting
#' @param interesting.vars variables for which to check the IGs (none = all)
#' @param data input data where columns are variables and rows are observations
#' @param decision decision variable as a boolean vector of length equal to number of observations
#' @return none (the function prints results)
#' @export
#' @useDynLib CuCubes CuCubes
ComputeInterestingTuples <- function(
    acceleration.type = 'scalar',
    dimensions = 1,
    divisions = 1,
    discretizations = 1,
    seed = 0,
    range = 1.0,
    pseudo.count = 0.001,
    reduce.method = 'max',
    ig.thr,
    interesting.vars = c(),
    data,
    decision) {
  n <- length(decision)
  k <- ncol(data)

  if (pseudo.count <= 0) {
    stop('Pseudo count has to be strictly greater than 0.')
  }

  if (n != nrow(data)) {
    stop('Length of decision is not equal to the number of rows in data.')
  }

  if (!all(decision == 0 || decision == 1)) {
    stop('Decision must be a vector of 0s and 1s only.')
  }

  if (reduce.method == 'max') {
    reduce.method.int = 0
  } else if (reduce.method == 'mean') {
    reduce.method.int = 1
  } else {
    stop('Unknown reduce.method')
  }

  if (acceleration.type == 'scalar') {
    acceleration.type.int = 0
  } else if (acceleration.type == 'avx') {
    if (discretizations %% 4 != 0) {
      stop('AVX Error: Number of discretizations must be a multiple of 4')
    }
    acceleration.type.int = 1
  } else if (acceleration.type == 'avx2') {
    if (discretizations %% 8 != 0) {
      stop('AVX2 Error: Number of discretizations must be a multiple of 8')
    }
    acceleration.type.int = 2
  } else {
    stop('Unknown acceleration.type')
  }

  rst <- .C(
      CuCubes,
      as.integer(acceleration.type.int),    # acceleration type
      as.integer(1),                        # output type (1 for interesting tuples)
      as.integer(n),                        # n
      as.integer(k),                        # k
      as.integer(dimensions),               # dim
      as.integer(divisions),                # div
      as.integer(discretizations),          # disc
      as.integer(seed),                     # seed
      as.double(range),                     # range
      as.double(pseudo.count),              # pseudo_count
      as.integer(reduce.method.int),        # reduce_method
      as.double(ig.thr),                    # ig_thr
      as.integer(interesting.vars),         # interesting_vars
      as.integer(length(interesting.vars)), # interesting_vars_count
      as.double(data),                      # data
      as.integer(decision),                 # decision
      double(length=0))                     # IG max output (ignored)
}
