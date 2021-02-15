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
        if (s1 matches '{.p1, .p2});
        else
        begin
            i1 = p1 + p2;
        end
        if (s2 matches '{'{.p1, .p2}, '{.p3, .p4}});
        else
        begin
            i1 = p1 + p2 + p3 + p4;
        end
        i1 = i1 &&& s1 matches '{.p1, .p2} &&& p1 + p2 ? i1 : p1 + p2;
        i1 = i1 &&& s2 matches '{'{.p1, .p2}, '{.p3, .p4}} &&& p1 + p2 + p3 + p4 ? i1 : p1 + p2 + p3 + p4 ;
        case (s1) matches
            '{.p1, .p2} &&& p1 + p2 ? p1 + p2 : i1 :;
            default: i1 = p1 + p2;
        endcase
        case (s2) matches
            '{'{.p1, .p2}, '{.p3, .p4}} &&& p1 + p2 + p3 + p4:;
            default: i1 = p1 + p2 + p3 + p4;
        endcase
    end
endmodule



