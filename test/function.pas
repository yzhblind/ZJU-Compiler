PROGRAM Func(input, output);
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

    Function Pow(A: Integer; B: Integer) : Integer;
        VAR
            ANS: integer;
        BEGIN
            ANS := 1;
            REPEAT
                If (B mod 2 = 1) then ANS := ANS * A;
                A := A * A;
                B := B div 2;
            UNTIL B = 0;
            RESULT := ANS;
        END;

    BEGIN
        write(Gcd(Pow(2, 10), Fac(5))); {gcd(1024, 120)}
    END.