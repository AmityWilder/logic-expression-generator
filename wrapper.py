import logic_expression_generator

circuit = logic_expression_generator.gen_random_circuit(2, 5, ["and", "or", "not"])
print(circuit)
