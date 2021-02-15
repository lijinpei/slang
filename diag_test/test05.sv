module test();
    typedef struct {
        integer m1, m2;
    } S1;
    S1 s1;
    typedef struct {
        S1 m1, m2;
    } S2;
    S2 s2;
    integer i1;
    initial begin
        if (s1 matches '{.p, .p});
        if (s2 matches '{'{.p1, .p2}, '{.p3, .p1}});
        i1 = i1 &&& s1 matches '{.p, .p} &&& i1 ? i1 : i1;
        i1 = i1 &&& s2 matches '{'{.p1, .p2}, '{.p3, .p1}} &&& i1 ? i1 : i1;
        case (s1) matches
            '{.p, .p} &&& i1 ? i1 : i1:;
            default: ;
        endcase
        case (s2) matches
            '{'{.p1, .p2}, '{.p3, .p1}} &&& i1 ? i1 : i1 :;
            default: ;
        endcase
    end
endmodule
