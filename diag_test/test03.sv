module test();
    integer i1, i2;
    struct {
        integer m1;
    } i;
    initial begin
        if (i1 &&& i matches '{m1:.j, m2:.k} &&& i2);
        i =  i1 &&& i matches '{m1:.j, m2:.k} &&& i2 ? i : i;
        case (i) matches
            '{m1:.j, m2:.k} &&& i2:;
            default: ;
        endcase
    end
endmodule

