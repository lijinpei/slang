module test();
    integer i1, i2;
    struct {
        integer m1;
    } i;
    initial begin
        if (i1 &&& i matches '{.j, .k} &&& i2);
        i =  i1 &&& i matches '{.j, .k} &&& i2 ? i : i;
        case (i1) matches
            '{.j, .k}: ;
            default: ;
        endcase
    end
endmodule

