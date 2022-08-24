import  { run, Rel, conde, exist, eq, conso, nilo } from 'https://esm.sh/ramo';

const riffleo = Rel((a, b, o) =>
    exist((carA, cdrA, carB, cdrB, carO, cdrO, z0, z1) =>
        conde(
            // If `a` and `b` are both empty, then the output is empty
            [nilo(a), nilo(b), nilo(o)],
            
            // If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            [conso(carA, cdrA, a), nilo(b), eq(o, a)],
            
            // If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            [nilo(a), conso(carB, cdrB, b), eq(o, b)],
            
            // When both `a` and `b` are non-empty
            [conso(carA, cdrA, a), conso(carB, cdrB, b), conso(carO, cdrO, o),
                conde(
                    [eq(carO, carA), eq(z0, cdrA), eq(z1, b)],
                    [eq(carO, carB), eq(z0, a), eq(z1, cdrB)]),
                    
                riffleo(z0, z1, cdrO)])))

const roleo = Rel((role, el, er, biclique, eo) => [
    exist((x) => [
        eq(x, [role, el, er, biclique, eo]),
        conde(
            eq(x, ['a', 1, 0, 'l', 0]),
            eq(x, ['b', 0, 1, 'r', 0]),
            eq(x, ['u', 1, 0, 'o', 1]),
            eq(x, ['v', 0, 1, 'o', 1]),
            eq(x, ['w', 1, 1, 'o', 1]),
            eq(x, ['o', 0, 0, 'o', 0]))])])


const allRoleso = Rel((a, b, c, d, e) =>
    conde(
        eq([a, b, c, d, e], [[], [], [], [], []]),
        exist((carA, cdrA, carB, cdrB, carC, cdrC, carD, cdrD, carE, cdrE) => [
            conso(carA, cdrA, a),
            conso(carB, cdrB, b),
            conso(carC, cdrC, c),
            conso(carD, cdrD, d),
            conso(carE, cdrE, e),
            roleo(carA, carB, carC, carD, carE),
            allRoleso(cdrA, cdrB, cdrC, cdrD, cdrE)])))
            
const proveso = Rel((hypergraph, bicliques, steps) =>
    conde(
        [nilo(steps), anyLessThan3o(hypergraph)],
        exist((step, restSteps, e1, e2, el, er, eo, restHyperedges, biclique, restBicliques, filteredBicliques, newHypergraph) => [
            conso(step, restSteps, steps),

            // Pick two hyperedges
            riffleo([e1, e2], restHyperedges, hypergraph),
            
            // Order matters since one will be considered "left" and the other "right"
            riffleo([el], [er], [e1, e2]),
            
            // Pick one biclique
            riffleo([biclique], restBicliques, bicliques),
            
            // These hyperedges must be connected
            allRoleso(step, el, er, biclique, eo),

            filterBicliqueso(biclique, restBicliques, filteredBicliques),
            
            // Recur on the rest of the steps with the new hypergraph
            conso(eo, restHyperedges, newHypergraph),
            proveso(newHypergraph, filteredBicliques, restSteps)])))

const lessThan3o = Rel((hyperedge) =>
    exist((n) => [
        conde(
            eq(n, ['z']),
            eq(n, ['s', 'z']),
            eq(n, ['s', 's', 'z'])),
            
        sizeo(hyperedge, n)]))
            
const anyLessThan3o = Rel((hypergraph) =>
    exist((hyperedge, restHyperedges) => [
        conso(hyperedge, restHyperedges, hypergraph),
        conde(
            lessThan3o(hyperedge),
            anyLessThan3o(restHyperedges))]))

const sizeo = Rel((hyperedge, size) =>
    conde(
        [nilo(hyperedge), eq(size, ['z'])],
        exist((first, rest, sizeRec) => [
            conso(first, rest, hyperedge),
            conde(
                [eq(first, 0), eq(size, sizeRec)],
                [eq(first, 1), conso('s', sizeRec, size)],
            ),
            sizeo(rest, sizeRec)])))
            
            
const filterBicliqueso = Rel((x, l, o) =>
    conde(
        [nilo(l), nilo(o)],
        exist((carL, cdrL, oRest) => [
            conso(carL, cdrL, l),
            conde(
                [conso(carL, oRest, o),
                    allDisjointo(x, carL)],
                [eq(o, oRest),
                    anyOverlapo(x, carL)]),
            filterBicliqueso(x, cdrL, oRest)])))
            
const overlapo = Rel((u, v) =>
    exist((x) => [
        eq(x, [u, v]),
        conde(
            eq(x, ['l', 'l']),
            eq(x, ['l', 'r']),
            eq(x, ['r', 'l']),
            eq(x, ['r', 'r']))]))
            
const anyOverlapo = Rel((x, y) =>
    exist((carX, cdrX, carY, cdrY) => [
        conso(carX, cdrX, x),
        conso(carY, cdrY, y),
        conde(
            overlapo(carX, carY),
            anyOverlapo(cdrX, cdrY))]))
            
const disjointo = Rel((u, v) =>
    conde(
        eq(u, 'o'),
        eq(v, 'o')))
        
const allDisjointo = Rel((x, y) =>
    conde(
        [nilo(x), nilo(y)],
        exist((carX, cdrX, carY, cdrY) => [
            conso(carX, cdrX, x),
            conso(carY, cdrY, y),
            disjointo(carX, carY),
            allDisjointo(cdrX, cdrY)])))
            

const abcHypergraph = [
    [1, 0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 1, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 1, 1]]
    
const abcBicliques = [
    ['l', 'r', 'o', 'o', 'o', 'o', 'o', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'o', 'l', 'r', 'o', 'o'],
    // ...
    ['o', 'o', 'l', 'l', 'l', 'l', 'r', 'o', 'o']]


const k4Hypergraph = [
    [1, 1, 1, 0],
    [0, 1, 1, 1]]
    
const k4Bicliques = [
    ['l', 'o', 'o', 'r']]


