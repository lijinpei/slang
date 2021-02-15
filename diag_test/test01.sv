module test();
    integer i;
    initial begin
        if (i &&& i matches '{.j, .k});
        i =  i &&& i matches '{.j, .k} ? i : i;
        case (i) matches
            '{.j, .k} &&& i: ;
            default: ;
        endcase
    end
endmodule
