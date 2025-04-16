"""
         connected_graph(A, producer_indexes)

removed consumers not connected to a producer (starving consumers) and isolated producers.

# Argument

- A: Adjacency matrix
- producer_indexes: position of producers in the adjacency matrix

# Value

return a tuple:
- graph_component: the graph components connected to a producer
- species: all the species connected

# Examples

# Consumer 3 is connected to consumer 2 which has no more feeding links
A = [0 0 0; 0 0 0; 0 1 0]
producers_idxs = [1]
expected = Int64[]
expected == connected_graph(A, producers_idxs).species

# Consumer 3 is connected to consumer 2 which is connected to a producer
A = [0 0 0; 1 0 0; 0 1 0]
producers_idxs = [1]
expected = Int64[1, 2, 3]
expected == connected_graph(A, producers_idxs).species

# Producer 1 is disconnected
A = [0 0 0; 0 0 0; 0 1 0]
producers_idxs = [1, 2]
expected = [2, 3]
expected == connected_graph(A, producers_idxs).species

# Only producers and all disconnected
A = [0 0 0; 0 0 0; 0 0 0]
producers_idxs = [1, 2, 3]
expected = Int64[]
expected == connected_graph(A, producers_idxs).species

# Only disconnected consumers
A = [0 0; 0 0]
producers_idxs = []
expected = Int64[]
expected == connected_graph(A, producers_idxs).species

# Two trophic chains and two producers
A = [0 0 0 0; 0 0 0 0; 1 0 0 0; 0 1 0 0]
producers_idxs = [1, 2]
expected = [1, 2, 3, 4]
all(expected .== sort(connected_graph(A, producers_idxs).species)) == true

# Two trophic chains on one producer
A = [0 0 0 0; 1 0 0 0; 0 1 0 0; 1 0 0 0]
producers_idxs = [1]
expected = [1, 2, 3, 4]
all(expected .== sort(connected_graph(A, producers_idxs).species)) == true

# Two trophic chains on one isolated producer
A = [0 0 0 0; 0 0 0 0; 1 0 0 0; 1 0 0 0]
producers_idxs = [1, 2]
expected = [1, 3, 4]
all(expected .== sort(connected_graph(A, producers_idxs).species)) == true

# One trophic chain and one isolated consumer (4)
A = [0 0 0 0; 1 0 0 0; 0 1 0 0; 0 0 0 0]
producers_idxs = [1]
expected = [1, 2, 3]
all(expected .== sort(connected_graph(A, producers_idxs).species)) == true

"""
function connected_graph(A, producer_indexes)
    # Get all the sepated components of the graph
    graph_component = connected_components(SimpleDiGraph(A))
    # Get the components that are connected to a producer
    connected_to_producers = [
                              length(intersect(producer_indexes, graph_component[i])) > 0
                              for i in 1:length(graph_component)
                             ]
    # Get the isolated components (i.e. number of species == 1)
    isolated_component = [length(graph_component[i]) == 1 for i in 1:length(graph_component)]
    # filter
    to_keep = connected_to_producers .& (.!isolated_component)

    # get the component to keep and the corresponding species
    graph_to_keep = graph_component[to_keep]
    species_idxs_to_keep = reduce(vcat, graph_to_keep, init = Int64[])
    (graph_components = graph_to_keep, species = species_idxs_to_keep,)
end

function disconnected_species(A, metabolic_class, species_names)

    consumer_to_remove = retrieve_disconnected_consumers(
                                                         A,
                                                         metabolic_class,
                                                         species_names
                                                        )
    to_keep = setdiff(species_names, consumer_to_remove)
    to_keep_idxs = [i ∈ to_keep for i in species_names]

    tmp_meta = metabolic_class[to_keep_idxs]
    tmp_A = A[to_keep_idxs, to_keep_idxs]
    tmp_species = species_names[to_keep_idxs]
    isolated_producer_mask = tmp_meta .== :producer .&& [sum(tmp_A[:,i]) == 0 for i in 1:size(tmp_A, 1)]
    isolated_producers = tmp_species[isolated_producer_mask]
    to_keep_final = setdiff(species_names, [consumer_to_remove; isolated_producers])

    to_keep_idxs_final = [i ∈  to_keep_final for i in species_names]

    (
     starving_consumers = consumer_to_remove,
     isolated_producers = isolated_producers,
     species_to_keep = to_keep_final,
     idxs_to_keep = to_keep_idxs_final
    )

end

function retrieve_disconnected_consumers(A, metabolic_class, species)
    A = deepcopy(A)
    are_there_disconnected = true
    out = []
    while are_there_disconnected
        idxs = collect(1:size(A, 1))
        no_outgoing_link = findall([sum(A[i,:]) == 0 for i in idxs])
        disconnected_cons_idxs = idxs[[i ∈  no_outgoing_link && metabolic_class[i] == :invertebrate for i in idxs]]
        disconnected_cons_species = species[disconnected_cons_idxs]
        are_there_disconnected = length(disconnected_cons_species) > 0
        idxs_to_keep = [i ∉ disconnected_cons_idxs for i in 1:size(A, 1)]
        A = A[idxs_to_keep, idxs_to_keep]
        species = species[idxs_to_keep]
        metabolic_class = metabolic_class[idxs_to_keep]
        append!(out, disconnected_cons_species)
    end
    out
end
