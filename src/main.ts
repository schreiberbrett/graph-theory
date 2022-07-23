function setEquals<T>(a: Set<T>, b: Set<T>): boolean {
  if (a.size !== b.size) {
    return false;
  }

  for (let x of a) {
    if (!b.has(x)) {
      return false;
    }
  }

  return true;
}

function compareSets(
  a: Set<number>,
  b: Set<number>,
  domainSize: number,
): number {
  // Fail fast if the sets are not the same length
  if (a.size > b.size) return 1;
  if (a.size < b.size) return -1;

  // Otherwise, lexographically, compare two sets by comparing their bitstring representations.
  // This requires that you restrict the domain so that the strings are of a finite length.
  let aList: string[] = [];
  let bList: string[] = [];
  for (let i = 0; i < domainSize; i++) {
    aList.push(a.has(i) ? "1" : "0");
    bList.push(b.has(i) ? "1" : "0");
  }

  let aString: string = aList.join("");
  let bString: string = bList.join("");

  return aString.localeCompare(bString);
}

/**
 * @see {diagrams/sample-zeta-graph.svg}
 */
type ZetaGraph = {
  vertexCount: number;
  zetaCount: number;

  // A symmetric relationship from vertex to vertex.
  hasNeighbor: [number, number][];

  // A many-to-many relationship from vertex to zeta group.
  hasZeta: [number, number][];
};

// TODO: This is quadratic complexity. Can it be optimized using DFS?
function exhaustiveFilterNonCandidates(
  g: ZetaGraph,
): [ZetaGraph, number[], number[]] {
  let [
    newG,
    vertexRelabelings,
    zetaRelabelings,
  ] = filterNonCandidates(g);
  
  let vertexRelabelings = new Array<number>(g.vertexCount);
  for (let v = 0; v < g.vertexCount; v++) {
      vertexRelabelings[v] = v;
  }

  let zetaRelabelings = new Array<number>(g.zetaCount);
  for (let z = 0; z < g.zetaCount; z++) {
      zetaRelabelings[z] = z;
  }
  
  if (newG.vertexCount === g.vertexCount && newG.zetaCount === g.zetaCount) {
  }

  return [
    newG,
    vertexRelabelings,
    zetaRelabelings,
  ];
}

function filterNonCandidates(g: ZetaGraph): [ZetaGraph, number[], number[]] {
  let hasANeighbor = new Array<boolean>(g.vertexCount);

  for (let v = 0; v < g.vertexCount; v++) {
    hasANeighbor[v] = false;
  }

  for (let [v, _] of g.hasNeighbor) {
    hasANeighbor[v] = true;
  }

  let wVerticesPerZeta = new Array<number>(g.zetaCount).fill(0);
  let zetasPerVertex = new Array<number>(g.vertexCount).fill(0);
  for (let [v, z] of g.hasZeta) {
    if (!hasANeighbor[v]) {
      wVerticesPerZeta[z]++;
    }

    zetasPerVertex[v]++;
  }

  let zetaRelabelings = new Array<number>(g.zetaCount);
  let newZetaCount = 0;
  for (let i = 0; i < g.zetaCount; i++) {
    if (wVerticesPerZeta[i] >= 3) {
      zetaRelabelings[i] = -1; // Sentinel value indicating the zeta was deleted
    } else {
      zetaRelabelings[i] = newZetaCount;
      newZetaCount++;
    }
  }

  // Assume all vertices should be deleted. Then find any counterexamples which prove that the vertex should remain.
  let hasRemainingZeta = new Array<boolean>(g.vertexCount).fill(false);
  for (let [v, z] of g.hasZeta) {
    if (zetaRelabelings[z] !== -1) {
      hasRemainingZeta[v] = true;
    }
  }

  let vertexRelabelings = new Array<number>(g.vertexCount);
  let newVertexCount = 0;
  for (let v = 0; v < g.vertexCount; v++) {
    if (hasRemainingZeta[v]) {
      vertexRelabelings[v] = newVertexCount;
      newVertexCount++;
    } else {
      vertexRelabelings[v] = -1;
    }
  }

  // Relabel
  let newHasNeighbor = new Array<[number, number]>();
  for (let [u, v] of g.hasNeighbor) {
    const newU = vertexRelabelings[u];
    const newV = vertexRelabelings[v];

    if (newU !== -1 && newV !== -1) {
      newHasNeighbor.push([newU, newV]);
    }
  }

  let newHasZeta = new Array<[number, number]>();
  for (let [v, o] of g.hasZeta) {
    const newV = vertexRelabelings[v];
    const newO = zetaRelabelings[o];

    if (newO !== -1) {
      newHasZeta.push([newV, newO]);
    }
  }

  const newZeta: ZetaGraph = {
    vertexCount: newVertexCount,
    zetaCount: newZetaCount,
    hasNeighbor: newHasNeighbor,
    hasZeta: newHasZeta,
  };

  return [newZeta, vertexRelabelings, zetaRelabelings];
}

/**
 * @param hasNeighbor
 *  A table of pairs where [u, v] is in the table if u ~ v is an edge in the graph.
 *  Many-to-many.
 *  If [u, v] is in `hasNeighbor`, then [v, u] must also be in `hasNeighbor`.
 *  The vertices MUST be labelled as 0 to (n - 1) if there are n vertices in the graph.
 *
 * @param hasOddCycle
 *  A table of pairs where [v, x] is in the table if v is a vertex in the odd cycle x.
 *  Many-to-many.
 *
 * @returns a pair of a new `hasNeighbor` relation and a new `hasOddCycle` relation where any
 */
function combine(g: ZetaGraph): [ZetaGraph, number[]] {
  // Represent the neighbors as an adjacency matrix using strings '0' or '1'
  let adjacencyMatrix = new Array<Array<string>>(g.vertexCount);
  for (let u = 0; u < g.vertexCount; u++) {
    adjacencyMatrix[u] = new Array<string>(g.vertexCount);
    for (let v = 0; v < g.vertexCount; v++) {
      adjacencyMatrix[u][v] = "0";
    }
  }

  for (let [u, v] of g.hasNeighbor) {
    adjacencyMatrix[u][v] = "1";
  }

  // Create a mapping from vertex to its neighbor-bitstring.
  // A bitstring is a canonical representation of vertex's neighbors.
  let bitstrings = new Array<string>(g.vertexCount);
  for (let v = 0; v < g.vertexCount; v++) {
    const bitstring = adjacencyMatrix[v].join("");
    bitstrings[v] = bitstring;
  }

  let uniqueBitstrings = 0;
  let bitstringToLabel = new Map<string, number>();
  for (let bitstring of bitstrings) {
    if (!bitstringToLabel.has(bitstring)) {
      bitstringToLabel.set(bitstring, uniqueBitstrings);
      uniqueBitstrings++;
    }
  }

  let vertexRelabelings = new Array<number>(g.vertexCount);
  for (let v = 0; v < g.vertexCount; v++) {
    vertexRelabelings[v] = bitstringToLabel.get(bitstrings[v])!;
  }

  // Now we can relabel the edges and odd cycles.
  // Relabeling the "has neighbor" relation can create duplicates, but we want unique rows only.
  // So we hash the row using a delimiter (#) to keep track of rows already added.
  let newHasNeighborSet = new Set<string>();
  let newHasNeighbor = new Array<[number, number]>();
  for (let [u, v] of g.hasNeighbor) {
    let newU = vertexRelabelings[u];
    let newV = vertexRelabelings[v];

    const hash: string = newU.toString() + "#" + newV.toString();
    if (!newHasNeighborSet.has(hash)) {
      newHasNeighborSet.add(hash);
      newHasNeighbor.push([newU, newV]);
    }
  }

  let newHasZeta = new Array<[number, number]>(g.hasZeta.length);
  for (let i = 0; i < g.hasZeta[i].length; i++) {
    const [v, z] = g.hasZeta[i];
    newHasZeta[i] = [vertexRelabelings[v], z];
  }

  const newG: ZetaGraph = {
    vertexCount: uniqueBitstrings + 1,
    zetaCount: g.zetaCount,
    hasNeighbor: newHasNeighbor,
    hasZeta: newHasZeta,
  };

  return [newG, vertexRelabelings];
}

const example: ZetaGraph = {
  vertexCount: 8,
  hasNeighbor: [
    [0, 1],
    [1, 0],
    [2, 4],
    [2, 5],
    [3, 4],
    [3, 5],
    [4, 2],
    [4, 3],
    [5, 2],
    [5, 3],
    [6, 7],
    [7, 6],
  ],
  zetaCount: 4,
  hasZeta: [
    [0, 0],
    [1, 1],
    [2, 0],
    [3, 1],
    [4, 2],
    [5, 3],
    [6, 2],
    [7, 3],
  ],
};

const example2 = [
  [0, 2],
  [0, 4],
  [1, 7],
  [2, 0],
  [3, 6],
  [4, 0],
  [5, 7],
  [6, 3],
  [7, 5],
  [7, 1],
];

const exampleOC2 = [
  [0, 0],
  [1, 1],
  [2, 1],
  [2, 2],
  [3, 1],
  [4, 2],
  [5, 2],
  [5, 3],
  [6, 3],
  [7, 3],
];

const reductionNumberOfVertices = 18;
const reductionNumberOfOddCycles = 5;
const reductionNeighbor = [
  [3, 5],
  [3, 6],
  [5, 3],
  [6, 3],
  [9, 12],
  [12, 9],
  [14, 16],
  [16, 14],
];

const reductionOddCycles = [
  [0, 0],
  [1, 0],
  [2, 0],
  [3, 0],
  [4, 1],
  [5, 1],
  [8, 1],
  [9, 1],
  [6, 4],
  [7, 4],
  [10, 4],
  [11, 2],
  [12, 2],
  [13, 2],
  [14, 2],
  [15, 3],
  [16, 3],
  [17, 3],
];

console.log("Hello, world!");
