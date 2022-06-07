PROGRAM Add(input, output);
    VAR
        C, OUT: integer;

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