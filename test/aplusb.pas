PROGRAM Add(input, output);
    CONST
        A = 50;
        B = 60;
    VAR
        C, D, OUT: integer;
    BEGIN
        D := 20;
        C := ((A + B) * (B - A)) div D + D;
        OUT := C;
    END.