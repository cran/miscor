##########################################################################################################
#
# miscor: Miscellaneous Functions for the Correlation Coefficient
#
# Internal function: BBolve
#
# Function copied from the PoisNonNor package <cran.r-project.org/web/packages/PoisNonNor>
internal.BBsolve <- function(par, fn, method = c(2, 3, 1), control = list(),
                             quiet = TRUE, ...) {

  #---------------------------------
  internal.dfsane <- function (par, fn, method = 2, control = list(), quiet = FALSE,
                               alertConvergence = TRUE, ...) {

    ctrl <- list(maxit = 1500, M = 10, tol = 1e-07, trace = TRUE,
                 triter = 10, quiet = FALSE, noimp = 100, NM = FALSE,
                 BFGS = FALSE)

    namc <- names(control)
    if (!all(namc %in% names(ctrl))) {
      stop("unknown names in control: ", namc[!(namc %in% names(ctrl))])
    }

    ctrl[namc] <- control
    M <- ctrl$M
    maxit <- ctrl$maxit
    tol <- ctrl$tol
    trace <- ctrl$trace
    triter <- ctrl$triter
    noimp <- ctrl$noimp
    NM <- ctrl$NM
    BFGS <- ctrl$BFGS
    fargs <- list(...)

    lsm <- function(x, fn, F, fval, alfa, M, lastfv, eta, fcnt, bl, fargs) {

      maxbl <- 100
      gamma <- 1e-04
      sigma1 <- 0.1
      sigma2 <- 0.5
      lam1 <- lam2 <- 1
      cbl <- 0
      fmax <- max(lastfv)

      while (cbl < maxbl) {
        d <- -alfa * F
        xnew <- x + lam1 * d
        Fnew <- try(do.call(fn, append(list(xnew), fargs)))
        fcnt = fcnt + 1
        if (class(Fnew) == "try-error" || any(is.nan(Fnew))) {
          return(list(xnew = NA, Fnew = NA, fcnt = fcnt,
                      bl = bl, lsflag = 1, fune = NA))
        }

        fune1 <- sum(Fnew * Fnew)
        if (fune1 <= (fmax + eta - (lam1^2 * gamma * fval))) {
          if (cbl >= 1)
            bl <- bl + 1

          return(list(xnew = xnew, Fnew = Fnew, fcnt = fcnt,
                      bl = bl, lsflag = 0, fune = fune1))
        }

        xnew <- x - lam2 * d
        Fnew <- try(do.call(fn, append(list(xnew), fargs)))
        fcnt <- fcnt + 1
        if (class(Fnew) == "try-error" || any(is.nan(Fnew))) {
          return(list(xnew = NA, Fnew = NA, fcnt = fcnt,
                      bl = bl, lsflag = 1, fune = NA))
        }

        fune2 <- sum(Fnew * Fnew)
        if (fune2 <= (fmax + eta - (lam2^2 * gamma * fval))) {
          if (cbl >= 1)
            bl <- bl + 1
          return(list(xnew = xnew, Fnew = Fnew, fcnt = fcnt,
                      bl = bl, lsflag = 0, fune = fune2))
        }

        lamc <- (2 * fval * lam1^2)/(2 * (fune1 + (2 * lam1 - 1) * fval))
        c1 <- sigma1 * lam1
        c2 <- sigma2 * lam1

        lam1 <- if (lamc < c1)
          c1
        else if (lamc > c2)
          c2
        else lamc
        lamc <- (2 * fval * lam2^2)/(2 * (fune2 + (2 * lam2 - 1) * fval))
        c1 <- sigma1 * lam2
        c2 <- sigma2 * lam2

        lam2 <- if (lamc < c1)
          c1
        else if (lamc > c2)
          c2
        else lamc
        cbl <- cbl + 1
      }

      return(list(xnew = xnew, Fnew = Fnew, fcnt = fcnt, bl = bl,
                  lsflag = 2, fune = fune))
    }

    n <- length(par)
    fcnt <- iter <- bl <- 0
    alfa <- eta <- 1
    eps <- 1e-10
    lastfv <- rep(0, M)

    U <- function(x, ...) drop(crossprod(fn(x, ...)))
    if (NM) {
      res <- try(optim(par = par, fn = U, method = "Nelder-Mead",
                       control = list(maxit = 100), ...), silent = TRUE)
      if (class(res) == "try-error") {
        cat(res)
        stop("\nFailure in Nelder-Mead Start.  Try another starting value \n")
      }
      else if (any(is.nan(res$par)))
        stop("Failure in Nelder-Mead Start (NaN value).  Try another starting value \n")
      par <- res$par
      fcnt <- as.numeric(res$counts[1])
    }

    F <- try(fn(par, ...))
    fcnt <- fcnt + 1
    if (class(F) == "try-error")
      stop("Failure in initial functional evaluation. \n")
    else if (!is.numeric(F) || !is.vector(F))
      stop("Function must return a vector numeric value.")
    else if (any(is.nan(F), is.infinite(F), is.na(F)))
      stop("Failure in initial functional evaluation. \n")
    else if (length(F) == 1)
      if (!quiet)
        warning("Function returns a scalar. Function BBoptim or spg is better.")
    F0 <- normF <- sqrt(sum(F * F))

    if (trace)
      cat("Iteration: ", 0, " ||F(x0)||: ", F0/sqrt(n), "\n")
    pbest <- par
    normF.best <- normF
    lastfv[1] <- normF^2
    flag <- 0
    knoimp <- 0

    while (normF/sqrt(n) > tol & iter <= maxit) {
      if ((abs(alfa) <= eps) | (abs(alfa) >= 1/eps))
        alfa <- if (normF > 1)
          1
      else if (normF >= 1e-05 & normF <= 1)
        1/normF
      else if (normF < 1e-05)
        1e+05
      if (iter == 0) {
        alfa <- min(1/normF, 1)
        alfa1 <- alfa2 <- alfa
      }
      temp <- alfa2
      alfa2 <- alfa
      if (normF <= 0.01)
        alfa <- alfa1
      alfa1 <- temp
      ls.ret <- lsm(x = par, fn = fn, F = F, fval = normF^2,
                    alfa, M = M, lastfv = lastfv, eta, fcnt, bl, fargs)
      fcnt <- ls.ret$fcnt
      bl <- ls.ret$bl
      flag <- ls.ret$lsflag
      if (flag > 0)
        break
      Fnew <- ls.ret$Fnew
      pnew <- ls.ret$xnew
      fune <- ls.ret$fune
      pF <- sum((pnew - par) * (Fnew - F))
      pp <- sum((pnew - par)^2)
      FF <- sum((Fnew - F)^2)
      alfa <- if (method == 1)
        pp/pF
      else if (method == 2)
        pF/FF
      else if (method == 3)
        sign(pF) * sqrt(pp/FF)
      if (is.nan(alfa))
        alfa <- eps
      par <- pnew
      F <- Fnew
      fun <- fune
      normF <- sqrt(fun)
      if (normF < normF.best) {
        pbest <- par
        normF.best <- normF
        knoimp <- 0
      }
      else knoimp <- knoimp + 1
      iter <- iter + 1
      lastfv[1 + iter%%M] <- fun
      eta <- F0/(iter + 1)^2
      if (trace && (iter%%triter == 0))
        cat("iteration: ", iter, " ||F(xn)|| =  ", normF,
            "\n")
      if (knoimp == noimp) {
        flag <- 3
        break
      }
    }
    conv <- if (flag == 0) {
      if (normF.best/sqrt(n) <= tol)
        list(type = 0, message = "Successful convergence")
      else if (iter > maxit)
        list(type = 1, message = "Maximum limit for iterations exceeded")
      else list(type = 2, message = "Method stagnated")
    }
    else if (flag == 1)
      list(type = 3, message = "Failure: Error in function evaluation")
    else if (flag == 2)
      list(type = 4, message = "Failure: Maximum limit on steplength reductions exceeded")
    else if (flag == 3)
      list(type = 5, message = "Lack of improvement in objective function")
    if (BFGS & (conv$type == 2 | conv$type == 5)) {
      if (!quiet)
        cat(" Calling `L-BFGS-B' in `optim' \n")
      res <- try(optim(par = pbest, fn = U, method = "L-BFGS-B",
                       control = list(pgtol = 1e-08, factr = 1000, maxit = 200),
                       ...), silent = TRUE)
      if (!inherits(res, "try-error") && !any(is.nan(res$par))) {
        normF.new <- sqrt(res$value)
        if (normF.new < normF.best) {
          normF.best <- normF.new
          pbest <- res$par
        }
      }
      fcnt <- fcnt + as.numeric(res$counts[1])
      if (normF.best/sqrt(length(par)) <= tol)
        conv <- list(type = 0, message = "Successful convergence")
    }
    if (alertConvergence && (0 != conv$type))
      warning("Unsuccessful convergence.")
    return(list(par = pbest, residual = normF.best/sqrt(length(par)),
                fn.reduction = F0 - normF.best, feval = fcnt, iter = iter,
                convergence = conv$type, message = conv$message))
  }
  #---------------------------------

  ctrl <- list(maxit = 1500, M = c(50, 10), tol = 1e-07, trace = FALSE,
               triter = 10, noimp = 100, NM = c(TRUE, FALSE))

  namc <- names(control)
  if (!all(namc %in% names(ctrl))) {
    stop("unknown names in control: ", namc[!(namc %in% names(ctrl))])
  }

  if (is.matrix(par)) {
    stop("argument par should not be a matrix in BBsolve.")
  }

  ctrl[namc] <- control
  M <- ctrl$M
  maxit <- ctrl$maxit
  tol <- ctrl$tol
  trace <- ctrl$trace
  triter <- ctrl$triter
  noimp <- ctrl$noimp
  NM <- if (length(par) > 1 & length(par) <= 20) { ctrl$NM } else { FALSE }
  control.pars <- expand.grid(method = method, M = M, NM = NM)
  feval <- iter <- 0
  ans.best.value <- Inf

  for (i in 1:nrow(control.pars)) {
    cpars <- unlist(control.pars[i, ])
    temp <- try(internal.dfsane(par = par, fn, method = cpars[1],
                                control = list(M = as.numeric(cpars[2]), NM = cpars[3],
                                               maxit = maxit, tol = tol, trace = trace, triter = triter,
                                               noimp = noimp), quiet = quiet, alertConvergence = FALSE, ...), silent = TRUE)

    if (!inherits(temp, "try-error")) {
      feval <- feval + temp$feval
      iter <- iter + temp$iter

      if (temp$convergence == 0) {
        ans.best <- temp
        ans.best$feval <- feval
        ans.best$iter <- iter
        ans.best$cpar <- cpars
        break
      } else
        if (temp$residual < ans.best.value) {
        ans.best <- temp
        ans.best.value <- ans.best$residual
        ans.best$feval <- feval
        ans.best$iter <- iter
        ans.best$cpar <- cpars
      }
    }
  }
  if (!quiet) {
    if (ans.best$convergence != 0)
      cat("  Unsuccessful convergence.\n")
    else cat("  Successful convergence.\n")
  }
  ans.best
}
