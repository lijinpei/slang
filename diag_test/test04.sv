module test();
    integer i1, i2;
    string i;
    integer i3;
    bit [23:0] i4;
    logic [23:0] i5, i6;
    initial begin
        if (i1 &&& i matches "123" &&& i2);
        if (i1 &&& i3 matches "123" &&& i2);
        if (i1 &&& i4 matches "123" &&& i2);
        if (i1 &&& i5 matches "123" &&& i2);
        if (i1 &&& i5 matches i6 &&& i2);
        i3 = i1 &&& i matches "123" &&& i2 ? i3 : i3;
        i3 = i1 &&& i3 matches "123" &&& i2 ? i3 : i3;
        i3 = i1 &&& i4 matches "123" &&& i2 ? i3 : i3;
        i3 = i1 &&& i5 matches "123" &&& i2 ? i3 : i3;
        i3 = i1 &&& i5 matches i6 &&& i2 ? i3 : i3;
        case (i) matches
            "123" &&& i2:;
            default: ;
        endcase
        case (i3) matches
            "123" &&& i2:;
            default: ;
        endcase
        case (i4) matches
            "123" &&& i2:;
            default: ;
        endcase
        case (i5) matches
            "123" &&& i2:;
            default: ;
        endcase
        case (i5) matches
            i6 &&& i2:;
            default: ;
        endcase
    end
endmodule


