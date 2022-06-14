PROGRAM Func(input, output);
    CONST
        P = 10007;
    VAR
        i, C: integer;
        f: ARRAY [0..1000] OF Integer;

    Function Gcd(A: Integer; B: Integer) : Integer;
        BEGIN
            If B = 0 then RESULT := A
            else RESULT := Gcd(B, A mod B);
        END;

    Function Fac(A: Integer) : Integer;
        BEGIN
            If A = 0 then RESULT := 1
            else RESULT := (A * Fac(A - 1)) mod P;
        END;

    Function Pow(A: Integer; B: Integer) : Integer;
        VAR
            ANS: integer;
        BEGIN
            ANS := 1;
            REPEAT
                If (B mod 2 = 1) then ANS := (ANS * A) mod P;
                A := (A * A) mod P;
                B := B div 2;
            UNTIL B = 0;
            RESULT := ANS;
        END;

    Function Catalan(N: Integer) : Integer;
        VAR
            i, j, tmp: integer;
        BEGIN
            N := N + 2;
            f[0] := 1;
            f[1] := 1;            
            i := 2;
            REPEAT
                j := 0;
                tmp := 0;
                REPEAT
                    tmp := tmp + f[j] * f[i - j];
                    j := j + 1;
                UNTIL j > i - 1;
                f[i] := tmp;
                i := i + 1;
            UNTIL i = N;
            RESULT := f[N - 1];
        END;

    BEGIN
        writeln(Gcd(Pow(2, 10), Fac(5))); {gcd(1024, 120)}
        writeln(Catalan(10)); {Catalan number}
        i := 1;
        WHILE i<=11 DO BEGIN
            writeln(f[i]); 
            i:=i+1;
        END;
    END.