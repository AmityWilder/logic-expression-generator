from cffi import FFI

ffi = FFI()
ffi.cdef("""
    typedef struct {
        void* data;
        size_t size;
    } CCircuit;
    CCircuit generate_random_circuit(unsigned char, unsigned short, bool, bool, bool, bool, bool, bool, bool, bool);
""")

C = ffi.dlopen("./target/debug/logic_expression_generator.dll")
C.generate_random_circuit(1, 5, )