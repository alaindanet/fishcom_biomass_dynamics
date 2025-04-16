println("Rundir is $(pwd())")

import Pkg
using Distributed, Serialization

first_sim = parse(Int, ARGS[1])
last_sim = parse(Int, ARGS[2])

println("Running parameters combination from $(ARGS[1]) to $(ARGS[2])")

#ncpu = maximum([length(Sys.cpu_info()), 15])
ncpu = 3

#Dir
#proj_dir = expanduser("~/fishcom_biomass_dynamics")
proj_dir = expanduser(".")

#Flag enables all the workers to start on the project of the current dir
flag = "--project=" * proj_dir
println("Workers run with flag: $(flag)")
addprocs(ncpu - 1, exeflags=flag)
#addprocs(5, exeflags=flag)
println("Using $(ncpu -1) cores")


@everywhere import Pkg, Random.seed!
# @everywhere using DifferentialEquations, EcologicalNetworksDynamics, SparseArrays
@everywhere using EcologicalNetworksDynamics, SparseArrays
@everywhere using LinearAlgebra, DataFrames
@everywhere using Distributions, ProgressMeter
@everywhere using StatsBase, Graphs, Arrow
#@everywhere include("../src/sim.jl")
#@everywhere include("../src/utils.jl")
@everywhere include("src/sim.jl")
@everywhere include("src/utils.jl")
