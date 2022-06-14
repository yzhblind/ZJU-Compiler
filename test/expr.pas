PROGRAM ExprTest(input, output);
    VAR
        A, B: integer;
        V1, V2, V3, V4, V5, V10: integer;

    BEGIN
        A := (3 + 5 * (3 + 4)) div 2; {19}
        B := ((2 < 3) * 10 + ((4 or 1) * 3 > 10) * 5) + (2 and 1) * 3; {15}
        writeln(A, B);
    END.