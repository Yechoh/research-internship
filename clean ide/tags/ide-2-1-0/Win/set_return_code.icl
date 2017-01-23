implementation module set_return_code;

import code from "set_return_code.obj";

import StdString;
import StdPSt;

:: *UniqueWorld :== World;

set_return_code_world :: !Int !UniqueWorld -> UniqueWorld;
set_return_code_world a0 a1 = code {
    ccall set_return_code "I:V:A"
    fill_a 0 1
    pop_a 1
}

set_return_code_pst :: !Int !(PSt .l) -> PSt .l;
set_return_code_pst a0 a1 = set_return_code_a a0 a1;

set_return_code_a :: !Int !.a -> .a;
set_return_code_a a0 a1 = code {
    ccall set_return_code "I:V:A"
}

// void set_return_code (int return_code);
