def verify_cbs(proof_step, graph):
    for v in range(len(proof_step))
        if proof_step[v] == 'r1':
            for u in range(len(proof_step)):
                if proof_step[u] == 'r2' and graph[v][u] == 0:
                    return False
                    
        if proof_step[v] == 'r2':
            for u in range(len(proof_step)):
                if proof_step[u] == 'r1' and graph[v][u] == 0:
                    return False
                    
    return True

