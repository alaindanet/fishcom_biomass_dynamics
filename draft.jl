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

living_species_indexes = findall(ti.solution[:, end] .> 0)
mi = disconnected_species(
                     ti.model.A[living_species_indexes, living_species_indexes],
                     ti.model.metabolic_classes[living_species_indexes],
                     ti.model.species_names[living_species_indexes]
                    )
length(mi.species_to_keep)


base_model = default_model(
              fw1, BodyMass(Z = 100), BioenergeticResponse(h = 1.5),
              without = ProducerGrowth
             );
K = K_global * (1 + (alpha_ij * (base_model.n_producers - 1))) / base_model.n_producers
# Rebuild model
m = base_model + LogisticGrowth(; r = 1, K = K, producers_competition = p);
B0 = rand(m.richness);
sol = simulate(m, B0, 2000, extinction_threshold = 10^-6);
get_output_sim(m, sol)

living_species_indexes = findall(sol[:, end] .> 0)
ti = disconnected_species(
                     m.A[living_species_indexes, living_species_indexes],
                     m.metabolic_classes[living_species_indexes],
                     m.species_names[living_species_indexes]
                    )
to_keep_idxs = [i ∈  ti.species_to_keep for i in m.species_names]
base_model = default_model(
                           Foodweb(m.A[to_keep_idxs, to_keep_idxs]),
                           MetabolicClass(m.metabolic_classes[to_keep_idxs]),
              without = ProducerGrowth
             );
consumer_to_remove = retrieve_disconnected_consumers(
                                m.A[living_species_indexes, living_species_indexes],
                                m.metabolic_classes[living_species_indexes],
                                m.species_names[living_species_indexes]
                               )
to_keep = setdiff(m.species_names[living_species_indexes], consumer_to_remove)
to_keep_idxs = [i ∈ to_keep for i in m.species_names]

tmp_meta = m.metabolic_classes[to_keep_idxs]
tmp_A = m.A[to_keep_idxs, to_keep_idxs]
tmp_species = m.species_names[to_keep_idxs]
isolated_producer_mask = tmp_meta .== :producer .&& [sum(tmp_A[:,i]) == 0 for i in 1:size(tmp_A, 1)]
isolated_producers = tmp_species[isolated_producer_mask]
to_keep_final = setdiff(m.species_names[living_species_indexes], [consumer_to_remove; isolated_producers])

to_keep_idxs_final = [i ∈  to_keep_final for i in m.species_names]

base_model = default_model(
                           Foodweb(m.A[to_keep_idxs_final, to_keep_idxs_final]),
                           MetabolicClass(m.metabolic_classes[to_keep_idxs_final]),
              without = ProducerGrowth
             );
base_model.species_names


show(stdout, "text/plain", properties(lo))

sol = sim([0 0 0; 0 0 0; 1 0 0]; Z = .1, saveat = 0:10:100);
sol.model.n_producers == 1

sol = sim([0 0 0 0 0; 1 0 0 0 0; 0 1 0 0 0; 0 0 1 0 0; 0 0 0 1 0], Z = 50, K_global = 1);
sol = sim([0 0 0 0 0 0; 1 0 0 0 0 0; 0 1 0 0 0 0; 0 0 1 0 0 0; 0 0 0 1 0 0; 0 0 0 0 1 0],
          Z = 50,
          K_global = .1
         );
sol.solution[:, end]
living_species_indexes = findall(sol.solution[:, end] .> 0)
producers_idexes = findall(sol.model.metabolic_classes .== :producer)
connected_graph(sol.model.A[living_species_indexes, living_species_indexes], producers_idexes)



tu = [
      0  0  0  0  0  0  0  0  0  0;
      1  0  1  0  0  0  1  0  0  0;
      0  0  0  0  0  0  0  0  0  0;
      0  0  0  0  1  0  0  0  0  0;
      0  0  0  0  0  0  0  0  0  0;
      1  0  1  0  0  0  1  1  0  0;
      0  0  0  0  1  0  0  1  0  0;
      0  0  0  0  0  0  0  0  0  0;
      0  0  1  0  1  0  1  1  0  0;
      1  0  1  0  0  0  0  0  0  0
     ]
species = base_model.species_names[1:10]
myprod = [1, 3, 8]
meta = repeat([:invertebrate], 10)
meta[myprod] .= :producer
to_remove = retrieve_disconnected_consumers(tu, meta, species)
to_keep = [i ∉ to_remove for i in species]
tu[to_keep, to_keep]

default_model(Foodweb(tu[to_keep, to_keep]), MetabolicClass(meta[to_keep]))

unique(base_model.metabolic_classes)

species_index = collect(1:size(tu,1))
producer_mask = [i ∈ myprod for i in species_index]
consumers = setdiff(species_index, myprod)
consumer_mask = [i ∈ consumers for i in species_index]
connected_graph(tu, myprod)
graph_component = connected_components(SimpleDiGraph(tu))

no_outgoing_link = [sum(tu[i,:]) == 0 for i in species_index]
no_ingoing_link = [sum(tu[:,i]) == 0 for i in species_index]

starving_consumers = consumer_mask .&& no_outgoing_link
isolated_producers = producer_mask .&& no_ingoing_link

to_remove = starving_consumers .|| isolated_producers

tu = tu[.!to_remove, .!to_remove]
graph_component = connected_components(SimpleDiGraph(tu))

sol.model
check_disconnected_species(sol.model.A, findall(sol.solution[:, end] .> 0))
producers_idxs = findall(ti.metabolic_classes .== :producer)
ti = Model() +
Foodweb([0 0 0; 0 0 0; 1 0 0]) +
MetabolicClass([:producer, :producer, :invertebrate])


base_model = default_model(fw1, BodyMass(; Z = Z), without = [ProducerGrowth, BioenergeticResponse])
fw = Foodweb([0 0 0; 0 0 0; 1 0 0])
bm = BodyMass([1, 2, 3])
mc = MetabolicClass([:producer, :producer, :invertebrate])
be = BioenergeticResponse()
lg = LogisticGrowth(K = [1, 1, 0])
ti = Model() + fw + bm + mc + be + lg

ti.species_index[[:s1, :s2]]
[ti.species_index[i] for i in [:s1, :s2]]
ti.species_index
dict_alive = filter(p -> last(p) ∈ [2], ti.species_index)
keys(dict_alive)
ti.metabolic_classes
species_alive_mask = [0, 1, 1] .> 0

show(stdout, "text/plain", properties(m))
properties(m2)

persistence(sol)

trophic_structure(sol)
