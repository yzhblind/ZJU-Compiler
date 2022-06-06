PROGRAM Add(input, output);
    CONST
        A = 50;
        B = 60;
        E = 5;
    VAR
        C, D, OUT: integer;
    BEGIN
        D := (A + B) * (B - A) + E;  (* 1105 *)
        if A * B > D then BEGIN
            if (E = D - 1100) then C := (D * D) div 5
            else C := 35;
        END
        else C := 100;
        OUT := C;
    END.