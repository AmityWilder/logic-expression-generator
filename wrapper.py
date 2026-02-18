from cffi import FFI

ffi = FFI()
ffi.cdef("""
    typedef struct {
        void* data;
        size_t size;
    } CCircuit;
    CCircuit generate_random_circuit(unsigned char, unsigned short, bool, bool, bool, bool, bool, bool, bool, bool);
    bool circuit_is_none(CCircuit);
""")

C = ffi.dlopen("./target/debug/liblogic_expression_generator.dylib")
circuit = C.generate_random_circuit(1, 5, True, True, True, True, True, True, True, True)
print(circuit)
