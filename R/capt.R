capture <- function(f)
{
    formals   <- fn_fmls_names(f)
    names     <- all.names(fn_body(f))
    informals <- setdiff(names, formals)
    # calculates escapes
    escapes <- map2_lgl(env_has(fn_env(f), informals, F),
                        env_has(fn_env(f), informals, T),
                        ~ .x + .y > 0)
    # assume variables that don't escape are defined in function
    escapes <- names(keep(escapes, ~ is_true(.x)))
    vars <- discard(escapes, ~ eval(expr(is_function(!!sym(.x)))))

    funs <- keep(escapes, ~ eval(expr(is_function(!!sym(.x)))))
    closures <- discard(funs, ~ eval(expr(is_primitive(!!sym(.x)))))
    ns <- map_chr(closures, ~ eval(expr(environmentName(fn_env(!!sym(.x))))))
    required.packages <- keep(ns, ~ .x != environmentName(global_env()))
    print(required.packages)
    preamble <- map(required.packages, ~ expr(library(!!sym(.x))))

    e <- env()
    bind <- function(x) env_bind(e, !!x := env_get(fn_env(f), x, inherit = T))
    walk(vars, bind)
    walk(closures, bind)
    fn_env(f) <- e
    fn_body(f) <- expr(`{`(!!!c(preamble, fn_body(f))))
    return(f)
}

# slurm.batch <- function(f, data.file, slurm.file)
# {
#   save(f, file = data.file)
#   write(
#     c(
#       "#!/bin/sh",
#       paste0("#SBATCH --array=1-", 1, "%40"),
#       "#SBATCH --mail-type=ALL",
#       "#SBATCH --mail-user=alexrix@umich.edu",
#       "#SBATCH --ntasks=1",
#       "#SBATCH --partition=junglebook",
#       "#SBATCH --time=5:00:00",
#       "#SBATCH --job-name=test_boxr",
#       "#SBATCH --mem-per-cpu=5000mb",
#       "Rscript -e \"",
#       "i <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))",
#       "i <- ifelse(is.na(i), 1, i)",
#       paste0("load(\\\"", data.file, "\\\")"),
#       "f(i)\""
#     ), file = slurm.file)
# }

make.parallizable <- function(f, ...)
{
    args <- dots_values(...)
    n <- length(args[[1]])
    if (!every(map_dbl(args, length), ~ .x == n))
        stop("arguments to f must all be the same length.")
    args <- as.data.frame(args)
    function(i)
    {
        library(rlang)
        if (i > n)
            stop("index out of range")
        exec(f, !!!args[i,])
    }
}

# args <- dots_values(...)
# for (arg in args)
#   if (length(arg) != length(args[[1]]))
#      stop("arguments to f must all be the same length.")
# args <- as.data.frame(args)


# .g <- function(i)
# {
#   if (i > nrow(args))
#     stop("index out of range")
#   if (!require("rlang")) {
#       warning("rlang not available.")
#       return(NULL)
#   }
#   ret.codes <- lapply(required.packages, function(x) eval(expr(require(!!x))))
#   if (any(is_false(ret.codes))) {
#       warning("Not all required packages available.")
#       return(NULL)
#   }
#   exec(f, !!!args[i,])
# }
# return(.g)
