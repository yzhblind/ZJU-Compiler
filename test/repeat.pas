PROGRAM Add(input, output);
    VAR
        A, B, OUT: integer;
    BEGIN
        A := 10;
        B := 1;
        REPEAT
            A := A - 1;
            B := B * 2;
        UNTIL A = 0;
        OUT := B;
    END.