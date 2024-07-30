function sim(A;
        alpha_ij = .5,
        K_global = 10,
        h = 1.5,
        Z = 50,
        t = 9000,
        extinction_threshold = 1e-6,
        kwargs...
    )

    base_model = default_model(Foodweb(A),
                               BodyMass(; Z = Z),
                               BioenergeticResponse(h = h),
                               without = ProducerGrowth)
    p = ProducersCompetition(; diag = 1.0, off = alpha_ij)
    global K = K_global * (1 + (alpha_ij * (base_model.n_producers - 1))) / base_model.n_producers
    # Rebuild model
    global m = base_model + LogisticGrowth(; r = 1, K = K, producers_competition = p)
    B0 = rand(m.richness)
    global solution = simulate(m, B0, t; extinction_threshold = extinction_threshold, kwargs...)

    #@infiltrate
    # Remove dead species
    living_species_indexes = findall(solution[:, end] .> 0)

    global check_graphs = disconnected_species(
                              m.A[living_species_indexes, living_species_indexes],
                              m.metabolic_classes[living_species_indexes],
                              m.species_names[living_species_indexes]
                             )
    global species_indexes_to_keep = [i ∈  check_graphs.species_to_keep for i in m.species_names]
    global species_to_remove = length(living_species_indexes) != length(check_graphs.species_to_keep)

    while species_to_remove && length(check_graphs.species_to_keep) > 0
        #@infiltrate
        new_A = m.A[species_indexes_to_keep, species_indexes_to_keep]
        fw = Foodweb(new_A)
        sp = Species(m.species_names[species_indexes_to_keep])
        bm = BodyMass(m.body_masses[species_indexes_to_keep])
        mc = MetabolicClass(m.metabolic_classes[species_indexes_to_keep])
        be = BioenergeticResponse(h = h,
                                  w = ConsumersPreferences(m.w[species_indexes_to_keep, species_indexes_to_keep]))
        global base_model = default_model(fw, sp, bm, mc, be, without = [ProducerGrowth])
        #base_model = default_model(fw, bm, mc, without = [ProducerGrowth])
        global K = K_global * (1 + (alpha_ij * (base_model.n_producers - 1))) / base_model.n_producers
        lg = LogisticGrowth(; r = 1, K = K, producers_competition = p)
        global m = base_model + lg

        global B0 = solution[species_indexes_to_keep, end]
        global solution = simulate(m, B0, t; extinction_threshold = extinction_threshold, kwargs...)

        global living_species_indexes = findall(solution[:, end] .> 0)
        global check_graphs = disconnected_species(
                                            m.A[living_species_indexes, living_species_indexes],
                                            m.metabolic_classes[living_species_indexes],
                                            m.species_names[living_species_indexes]
                                           )
        global species_indexes_to_keep = [i ∈  check_graphs.species_to_keep for i in m.species_names]
        global species_to_remove = length(living_species_indexes) != length(check_graphs.species_to_keep)
        if length(check_graphs.species_to_keep) == 0
            break
            println("no more species")
        end
    end

    if length(check_graphs.species_to_keep) > 0
        output = (model = m, solution = solution);
    else
        output = (model = missing, solution = missing);
    end
    output
end

function get_output_sim(model, solution; last = 1)

    if ismissing(model)
        output = (;
         total_bm = missing,
         final_richness = missing,
         maximum_trophic_level = missing,
         average_trophic_level = missing,
         weighted_average_trophic_level = missing,
         connectance_final = missing,
         persistence_final = missing,
         top_consumer_bm = missing,
         producer_bm = missing,
         intermediate_consumer_bm = missing,
         top_consumer_richness = missing,
         producer_richness = missing,
         intermediate_consumer_richness = missing
        )
        return output
    end
    living_species_indexes = findall(solution[:, end] .> 0)

    total_bm = mean(total_biomass(solution)[(end - last + 1):end])
    final_richness = mean(richness(solution)[(end - last + 1):end])
    bm_species = vec(mean(solution[living_species_indexes, (end - last + 1):end], dims = 2))
    rel_bm_species = bm_species ./ sum(bm_species)

    #@infiltrate
    m = default_model(Foodweb(model.A[living_species_indexes, living_species_indexes]),
                       MetabolicClass(model.metabolic_classes[living_species_indexes])
                  )
    tops_consumer_mask =  m.consumers_mask .& m.tops_mask

    trophic_levels_final = get_trophic_levels(m)
    maximum_trophic_level = maximum(trophic_levels_final)
    average_trophic_level = mean(trophic_levels_final)
    weighted_average_trophic_level = sum(trophic_levels_final .* rel_bm_species)
    connectance_final = m.n_trophic_links / (m.richness * (m.richness - 1))
    persistence_final = persistence(solution[end])
    top_consumer_bm = sum(bm_species[tops_consumer_mask])
    producer_bm = sum(bm_species[m.producers_mask])
    intermediate_consumer_bm = total_bm - top_consumer_bm - producer_bm
    top_consumer_richness = sum(tops_consumer_mask)
    producer_richness = sum(m.producers_mask)
    intermediate_consumer_richness = final_richness - top_consumer_richness - producer_richness

    (;
     total_bm,
     final_richness,
     maximum_trophic_level,
     average_trophic_level,
     weighted_average_trophic_level,
     connectance_final,
     persistence_final,
     top_consumer_bm,
     producer_bm,
     intermediate_consumer_bm,
     top_consumer_richness,
     producer_richness,
     intermediate_consumer_richness
    )
end
