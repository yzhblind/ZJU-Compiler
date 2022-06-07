PROGRAM Add(input, output);
    VAR
        C, OUT: integer;

    Function Fac(A: Integer) : Integer;
        BEGIN
            If A = 0 then RESULT := 1
            else RESULT := A * Fac(A - 1);
        END;

    Function Pow(B: Integer; A: Integer) : Integer;
        VAR
            ANS: integer;
        BEGIN
            ANS := 1;
            REPEAT
                A := A - 1;
                ANS := ANS * B;
            UNTIL A = 0;
            RESULT := ANS;
        END;

    BEGIN
        C := Pow(2, 10);
        OUT := C;
    END.