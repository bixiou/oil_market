module OracleDAG

export Market, build_A, solve_oracle

struct Market
    alpha::Float64        # prix de choke
    beta::Float64         # pente de la demande inverse
    r::Float64            # taux d'interet
    C::Vector{Float64}    # couts marginaux
    KAP::Vector{Float64}  # capacites de production
    S0::Vector{Float64}   # stocks initiaux
    A::Matrix{Float64}    # matrice de conduite
    T::Int                # horizon
end

"""
    build_A(blocs) -> A

Construit la matrice de conduite. Deux agents partageant le meme indice de bloc
appartiennent au meme cartel ; un indice negatif designe un preneur de prix,
dont la ligne est nulle.
"""
function build_A(blocs::Vector{Int})
    N = length(blocs)
    A = zeros(N, N)
    for i in 1:N
        blocs[i] < 0 && continue
        for j in 1:N
            blocs[j] == blocs[i] && (A[i, j] = 1.0)
        end
    end
    return A
end

"Partitionne les agents en blocs decisionnels a partir de A."
function blocs_of(A::Matrix{Float64})
    N = size(A, 1)
    seen = falses(N)
    out = Vector{Vector{Int}}()
    for i in 1:N
        seen[i] && continue
        if A[i, i] == 0.0
            push!(out, [i])
            seen[i] = true
        else
            B = [j for j in 1:N if A[i, j] == 1.0 && A[j, j] == 1.0]
            push!(out, B)
            for j in B
                seen[j] = true
            end
        end
    end
    return out
end

"""
    bloc_br!(x, t, B, ceff, m, R) -> ecart max

Meilleure reponse d'un cartel, traite comme un agent unique face a la demande
residuelle R. Son cout effectif est la moyenne des couts de ses membres ponderee
par les capacites, et sa production est repartie proportionnellement a celles-ci.
"""
function bloc_br!(x, t::Int, B::Vector{Int}, ceff::AbstractMatrix{Float64},
                  m::Market, R::Float64)
    cap_C = sum(m.KAP[i] for i in B)
    ceff_C = sum(ceff[t, i] * m.KAP[i] for i in B) / cap_C

    Q_C = clamp((m.alpha - m.beta * R - ceff_C) / (2.0 * m.beta), 0.0, cap_C)

    resid = 0.0
    for i in B
        xn = Q_C * (m.KAP[i] / cap_C)
        resid = max(resid, abs(xn - x[t, i]))
        x[t, i] = xn
    end
    return resid
end

"""
    static_period!(x, ceff, m, BL) -> (residu, iterations)

Boucle (a). A couts effectifs donnes, chaque periode est un equilibre statique
independant. On balaie les blocs en mettant chacun a jour a quantites rivales
figees, jusqu'au point fixe. La projection sur [0, kappa] decouvre les regimes
actifs au fil des balayages.
"""
function static_period!(x::Matrix{Float64}, ceff::AbstractMatrix{Float64}, m::Market,
                        BL::Vector{Vector{Int}};
                        tol::Float64 = 1e-13, maxit::Int = 20_000, warm::Bool = false)
    Th, N = size(ceff)
    warm || fill!(x, 0.0)
    resid, it = Inf, 0

    @inbounds while it < maxit
        it += 1
        resid = 0.0
        for B in BL
            if length(B) == 1
                i = B[1]
                invden = 1.0 / (m.beta * (1.0 + m.A[i, i]))
                kap_i = m.KAP[i]
                for t in 1:Th
                    s = 0.0
                    for k in 1:N
                        k == i && continue
                        s += (1.0 + m.A[i, k]) * x[t, k]
                    end
                    xn = clamp((m.alpha - ceff[t, i] - m.beta * s) * invden, 0.0, kap_i)
                    resid = max(resid, abs(xn - x[t, i]))
                    x[t, i] = xn
                end
            else
                for t in 1:Th
                    R = sum(x[t, k] for k in 1:N if !(k in B))
                    resid = max(resid, bloc_br!(x, t, B, ceff, m, R))
                end
            end
        end
        resid < tol && break
    end
    return resid, it
end

"Actualise les couts effectifs pour les rentes mu, puis resout l'equilibre statique."
function totals!(x, ceff, mu::Vector{Float64}, m::Market, BL, Th::Int;
                 tol::Float64 = 1e-13, warm::Bool = false)
    @inbounds for t in 1:Th
        g = (1.0 + m.r)^(t - 1)
        for i in eachindex(mu)
            ceff[t, i] = m.C[i] + mu[i] * g
        end
    end
    resid, _ = static_period!(x, view(ceff, 1:Th, :), m, BL; tol = tol, warm = warm)
    return resid
end

"""
    solve_oracle(m) -> (mu, x, p, conv)

Equilibre en boucle ouverte. Trois boucles imbriquees : l'equilibre statique a
couts effectifs donnes, une dichotomie sur chaque rente visant l'epuisement du
stock, et un balayage sur les rentes jusqu'au point fixe. `conv` est le deplacement
maximal des rentes au dernier balayage : au-dela de `tol`, la solution n'a pas
converge et ne doit pas etre utilisee.
"""
function solve_oracle(m::Market; S::Vector{Float64} = m.S0, Th::Int = m.T,
                      nouter::Int = 400, br_tol::Float64 = 1e-13,
                      mu_tol::Float64 = 1e-13, tol::Float64 = 1e-11)
    N = length(m.C)
    BL = blocs_of(m.A)
    x, ceff = zeros(Th, N), zeros(Th, N)
    mu, trial, mu_old = zeros(N), zeros(N), zeros(N)
    halfwidth = fill(m.alpha, N)   # demi-largeur de l'encadrement, adaptee a chaque balayage
    conv = m.alpha

    # Extraction cumulee de l'agent i sous le vecteur de rentes v.
    cum_at(v, i) = (totals!(x, ceff, v, m, BL, Th; tol = br_tol, warm = true);
                    sum(@view x[:, i]))

    totals!(x, ceff, mu, m, BL, Th; tol = br_tol)

    for _ in 1:nouter
        copyto!(mu_old, mu)
        # Dichotomie laxiste tant qu'on est loin du point fixe, resserree ensuite.
        mu_tol_s = max(mu_tol, 1e-3 * conv)

        for i in 1:N
            # Stock non contraignant a rente nulle : la rente est nulle.
            copyto!(trial, mu); trial[i] = 0.0
            if cum_at(trial, i) <= S[i]
                mu[i] = 0.0
                halfwidth[i] = m.alpha
                continue
            end

            # Encadrement local, avec repli sur [0, alpha] s'il ne contient pas la solution.
            lo = max(0.0, mu[i] - halfwidth[i])
            hi = min(m.alpha, mu[i] + halfwidth[i])
            copyto!(trial, mu); trial[i] = hi
            if cum_at(trial, i) > S[i]
                lo, hi = 0.0, m.alpha
            else
                copyto!(trial, mu); trial[i] = lo
                cum_at(trial, i) <= S[i] && (lo = 0.0)
            end

            while hi - lo > mu_tol_s
                mid = 0.5 * (lo + hi)
                copyto!(trial, mu); trial[i] = mid
                cum_at(trial, i) > S[i] ? (lo = mid) : (hi = mid)
            end

            newmu = 0.5 * (lo + hi)
            halfwidth[i] = max(8.0 * abs(newmu - mu[i]), 1e-6 * m.alpha)
            mu[i] = newmu
        end

        conv = maximum(abs.(mu .- mu_old))
        conv < tol && break
    end

    totals!(x, ceff, mu, m, BL, Th; tol = br_tol, warm = true)
    p = [m.alpha - m.beta * sum(@view x[t, :]) for t in 1:Th]
    return mu, x, p, conv
end

end # module


using .OracleDAG

"Cartel et frange a deux agents : rentes comparees a la solution connue."
function test_m2()
    m = Market(100.0, 1.0, 0.05, [10.0, 20.0], [20.0, 30.0], [700.0, 450.0],
               build_A([0, -1]), 50)
    mu, x, _, conv = solve_oracle(m)
    cum = sum(x, dims = 1)[:]
    println("test M2   : mu = ", round.(mu, digits = 3), "   attendu [12.821, 21.757]")
    println("            epuisement = ", round.(cum, digits = 3), " / ", m.S0)
    return maximum(abs.(mu .- [12.821, 21.757])) < 5e-3 &&
           maximum(abs.(cum .- m.S0)) < 1e-2 && conv < 1e-9
end

"Un cartel scinde en deux membres identiques doit donner le meme prix qu'un agent unique."
function test_bloc()
    m1 = Market(100.0, 1.0, 0.05, [10.0, 20.0], [20.0, 30.0], [600.0, 450.0],
                build_A([0, -1]), 50)
    m2 = Market(100.0, 1.0, 0.05, [10.0, 10.0, 20.0], [10.0, 10.0, 30.0],
                [300.0, 300.0, 450.0], build_A([0, 0, -1]), 50)
    _, _, p1, c1 = solve_oracle(m1)
    _, x2, p2, c2 = solve_oracle(m2)
    e = maximum(abs.(p1 .- p2))
    println("test bloc : ecart de prix = ", e)
    println("            cumul du bloc = ", round(sum(x2[:, 1:2]), digits = 3), " / 600.0")
    return e < 1e-6 && c1 < 1e-9 && c2 < 1e-9
end

println(test_m2()   ? "chemin scalaire valide" : "CHEMIN SCALAIRE FAUX")
println(test_bloc() ? "chemin bloc valide"     : "CHEMIN BLOC FAUX")
