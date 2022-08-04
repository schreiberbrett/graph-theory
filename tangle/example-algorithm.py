def is_cbs(proof_step, graph):
    return verify_cbs(proof_step, proof_step, graph)

def verify_cbs(proof_step, current_proof_step, graph):
    if current_proof_step == [] and graph == []:
        return True

    proof_step_v = current_proof_step[0]
    graph_v = graph[0]
    
    return (
        p(proof_step, graph_v, proof_step_v) and
        verify_cbs(proof_step, current_proof_step[1:], graph[1:])
    )
    
def p(proof_step, graph_v, proof_step_v):
    if proof_step == [] and graph_v == []:
        return True
        
    proof_step_u = proof_step[0]
    graph_v_u = graph_v[0]
    
    return (
        ((graph_v_u == 1) or (
            (not_r1(proof_step_v) or not_r2(proof_step_u)) and
            (not_r2(proof_step_v) or not_r1(proof_step_u))
        ) and p(proof_step[1:], graph_v[1:], proof_step_v))
    )
    
def not_r1(x):
    return (
        x == 'r2' or
        x == 'b1' or
        x == 'b2' or
        x == 'b' or
        x == 'o'
    )
    
def not_r2(x):
    return (
        x == 'r1' or
        x == 'b1' or
        x == 'b2' or
        x == 'b' or
        x == 'o'
    )

