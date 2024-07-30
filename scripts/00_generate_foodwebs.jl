using CSV, StatsBase, DataFrames, Arrow, EcologicalNetworksDynamics

nrep = 10
S = [10, 25, 50, 75, 80, 100]
C = 0.02:.02:.32

########################
#  Generate Food-webs  #
########################

rep = 1:1
names = (:rep, :richness, :connectance)
param = map(p -> (;Dict(k => v for (k, v) in zip(names, p))...),
            Iterators.product(rep, S, C)
           )[:]

fw_test = map(p -> try
                  Foodweb(:niche; S = p.richness, C = p.connectance, tol_C = .02,
                          reject_cycles = true)
              catch
                  missing
              end,
              param
             )
# Get only the foodweb that were successfully generated
param2 = param[.! ismissing.(fw_test)]

# Try to get same number of sampling per richness
df = groupby(DataFrame(param2), :richness)
rounded_mean(data_col) = round(mean(data_col), digits = 2)
# Get minimum, mean, and maximum connectance for each richness
df2 = combine(df, :connectance => minimum, :connectance => rounded_mean, :connectance => maximum)
df3 = stack(df2, 2:4, value_name= :connectance)
select!(df3, [:richness, :connectance])

function try_foodweb(S; C = .1, tol_C = .05, n = 5, kwargs...)
    fw = (A = missing,)
    local i = 1

    while all([ismissing(fw.A), i <= n])
        println("i = $i")
        fw = try Foodweb(:niche; S = S, C = C, tol_C = tol_C,
                          reject_cycles = true, kwargs...)
        catch
            (A = missing,)
        end
        i = i + 1
    end
    fw
end

fw = try_foodweb(10; C = 0.5, tol_C = .05).A

# Generate food webs with minimum, mean, and maximum connectance
df4 = repeat(df3, nrep)
df4[!, :rep] = reduce(vcat, [repeat([i], nrow(df3)) for i in 1:nrep])

foodweb = map(p -> (rep = p.rep, S = p.richness, C = p.connectance,
                    fw = try_foodweb(p.richness; C = p.connectance, tol_C = .05).A
                  ),
             NamedTuple.(eachrow(df4))
            )
df_fw = DataFrame(foodweb)
df_fw = df_fw[.! ismissing.(df_fw.fw),:]
# Create a foodweb_id
df_fw[!, :fw_id] = 1:nrow(df_fw)
Arrow.write("../fw_comb_ct_S.arrow", df_fw)
