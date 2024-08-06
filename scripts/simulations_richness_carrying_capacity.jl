include("../scripts/sim_setup.jl")
#include("scripts/sim_setup.jl")

# Prepare saving
# dest_dir = "/mnt/parscratch/users/bi1ahd/sim/assembly_richness_carrying_capacity/"
dest_dir = "/home/alain/Documents/post-these/mnhn/fishcom_biomass_dynamics/data-raw/"

if !isdir(dest_dir)
    mkdir(dest_dir)
end

#df_fw = DataFrame(Arrow.Table(joinpath(proj_dir, "scripts/fw_comb_ct_S.arrow")))
df_fw = DataFrame(Arrow.Table(joinpath("scripts/fw_comb_ct_S.arrow")))
#
# Add carrying capacity
K = collect(1:10)

param_names = (:fw_id, :K)
param_tmp = map(p -> (;Dict(k => v for (k, v) in zip(param_names, p))...),
            Iterators.product(df_fw[:, :fw_id], K)
           )[:]

param_df = innerjoin(DataFrame(param_tmp), df_fw, on = :fw_id)
param_df[!, :sim_id] = 1:nrow(param_df)

# Reshape interaction matrix
reshape_array(vec) = reshape(vec, (
                                   round(Int, sqrt(length(vec))),
                                   round(Int, sqrt(length(vec)))
                                  )
                            )
param_df[!, :fw] = map(x -> reshape_array(x), param_df[:, :fw])

# Make a tuple vector
param = NamedTuple.(eachrow(param_df))

# Warm-up
pm = sample(param)
println("Running warmup")
ti = sim(Int.(pm.fw), Z = 100,
         t = 5000, K_global = pm.K,
         saveat = 10, tspan = [3500, 4000]);
warmup = get_output_sim(ti.model, ti.solution)
println("$(warmup)")


if last_sim > size(param, 1)
    last_sim = size(param, 1)
end
println("Running param sim from lines $first_sim to $last_sim")

timing = @elapsed sim_output = @showprogress pmap(p ->
                         merge(
                               (sim_id = p.sim_id,),
                               begin
                                   s = sim(Int.(p.fw),
                                           Z = 100,
                                           t = 5000,
                                           K_global = p.K,
                                           saveat = 10,
                                           tspan = [3500, 5000]);
                                   get_output_sim(s.model, s.solution, last = 1)
                               end
                              ),
                         param,
                         #param[1:10],
                         #sample(param, 5),
                         batch_size = 100
                        )
sim_output
df = innerjoin(select(param_df, Not([:fw])), DataFrame(sim_output), on = :sim_id)

println("$(length(sim_output)) simulations took $(round(timing /60, digits = 2)) minutes to run")

# Saving
file_ts = string("sim_richness_carrying_capacity.arrow")
Arrow.write(joinpath(dest_dir, file_ts), df)
