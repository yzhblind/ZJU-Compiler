PROGRAM Add(input, output);
    VAR
        C: integer;

    Function Gcd(A: Integer; B: Integer) : Integer;
        BEGIN
            If B = 0 then RESULT := A
            else RESULT := Gcd(B, A mod B);
        END;

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
        write(10, 20);
    END.