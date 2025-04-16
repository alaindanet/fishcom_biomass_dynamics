using EcologicalNetworksDynamics, Statistics
using Infiltrator
using Graphs

include("src/utils.jl")

alpha_ij = .5
# 1 to 10 is clearly ok; does not change much after that
K_global = 1
Z = 50
p = ProducersCompetition(; diag = 1.0, off = alpha_ij)

include("src/sim.jl")
fw1 = Foodweb(:niche; S = 100, C = 0.06, reject_cycles = true);
ti = sim(fw1.A, t = 5000, Z = 100,
         K_global = 3,
         saveat = 10,
         tspan = [3500, 5000]);
get_output_sim(ti.model, ti.solution)
