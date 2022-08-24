
type Biclique = Array<('l' | 'r' | 'o')>
type Hyperedge = Array<('x' | 'o')>
type ProofStep = Array<('a' | 'b' | 'u' | 'v' | 'w' | 'o')>
type Hypergraph = Array<Hyperedge>
function anyOverlap(a: Biclique, b: Biclique): boolean {
    const numVertices = a[0].length;
    
    for (let v = 0; v < numVertices; v++) {
        const x = a[v] + b[v]
        
        if (x === 'll' || x === 'lr' || x === 'rl' || x === 'rr') {
            return true;
        }
    }
    
    return false;
}
function anyEdgeHasLessThanThree(h: Hypergraph): boolean {
    for (let hyperedge of h) {
        let edgeSize = 0;
        for (let vertex of hyperedge) {
            if (vertex === 'x') {
                edgeSize += 1;
            }
        }
        
        if (edgeSize < 3) {
            return true;
        }
    }
    
    return false;
}
function attemptProofStep(
    a: Hyperedge,
    b: Hyperedge,
    biclique: Biclique
): [ProofStep, Hyperedge] | null {
    
    const numVertices = a.length;
    let proofStep: ProofStep = [];
    let newH: Hyperedge = [];
    
    for (let v = 0; v < numVertices; v++) {
        const x = a[v] + b[v] + biclique[v];
    
        if (x === 'xol') {
            proofStep.push('a');
            newH.push('o');

        } else if (x === 'oxr') {
            proofStep.push('b');
            newH.push('o');

        } else if (x === 'xoo') {
            proofStep.push('u');
            newH.push('x');
        
        } else if (x === 'oxo') {
            proofStep.push('v');
            newH.push('x');
        
        } else if (x === 'xxo') {
            proofStep.push('w');
            newH.push('x');
        
        } else if (x === 'ooo') {
            proofStep.push('o');
            newH.push('o');
        
        } else {
            return null;
        }
    }
    
    return [proofStep, newH];
}

function remove2add1(h: Hypergraph, i: number, j: number, newH: Hyperedge): Hypergraph {
    let result = [];
    
    for (let index = 0; index < h.length; index++) {
        if (index !== i && index !== j) {
            result.push(h[index]);
        }
    }
    
    result.push(newH);
    return result;
}
function proves(h: Hypergraph, bicliques: Array<Biclique>): (Array<ProofStep> | null) {
    if (anyEdgeHasLessThanThree(h)) {
        return [];
    }

    for (let i = 0; i < h.length; i++) {
        for (let j = 0; j < h.length; j++) {
            for (let biclique of bicliques) {
                if (i !== j) {
                    const attempt = attemptProofStep(h[i], h[j], biclique);
                    
                    if (attempt !== null) {
                        const [proofStep, newHyperedge] = attempt;
                        
                        const proofSteps = proves(
                            remove2add1(h, i, j, newHyperedge),
                            bicliques.filter(x => !anyOverlap(x, biclique))
                        );
                        
                        if (proofSteps !== null) {
                            return proofSteps.concat([proofStep]);
                        }
                    }
                }
            }
        }
    }
    
    return null;
}

function spernerFamily(hypergraph: Hypergraph): Hypergraph {
    let result = [];
    for (let i = 0; i < hypergraph.length; i++) {
        let anySubset = false;
        for (let j = 0; j < hypergraph.length; j++) {
            if (i !== j && isSubset(hypergraph[j], hypergraph[i])) {
                anySubset = true;
                break;
            }
        }

        if (!anySubset) {
            result.push(hypergraph[i]);
        }
    }

    return result;
}
function isSubset(subset: Hyperedge, superset: Hyperedge): boolean {
    const numVertices = subset.length;

    for (let i = 0; i < numVertices; i++) {
        if (subset[i] === 'x' && superset[i] === 'o') {
            return false;
        }
    }
    
    return true;
}

const test = [
    ['x', 'x', 'o', 'x', 'o'],
    ['x', 'o', 'o', 'x', 'o'],
    ['x', 'o', 'o', 'x', 'x']];
    
console.log(spernerFamily(test));
const k4Hypergraph: Hypergraph = [
    ['x', 'x', 'x', 'o'],
    ['o', 'x', 'x', 'x']]
    
const k4Bicliques: Array<Biclique> = [
    ['l', 'o', 'o', 'r']]

const abcHypergraph: Hypergraph = [
    ['x', 'o', 'x', 'x', 'o', 'o', 'o', 'o', 'o'],
    ['o', 'x', 'o', 'o', 'x', 'x', 'o', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'o', 'o', 'x', 'x', 'x']]
    
const abcBicliques: Array<Biclique> = [
    ['l', 'r', 'o', 'o', 'o', 'o', 'o', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'l', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'o', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'l', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'o', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'o', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'o', 'l', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'o', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'l', 'l', 'r', 'o', 'o']]
    
console.log(proves(abcHypergraph, abcBicliques))

