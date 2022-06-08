PROGRAM arrtest(input, output);
    CONST
        MaxElts = 50;
    VAR
        A, B, ANS: integer;
        arr: ARRAY [1..MaxElts] OF Integer;

    BEGIN
        A := 0;
        B := 1;
        REPEAT
            A := A + 1;
            arr[A] := B;
            B := B * 2;
        UNTIL A = 10;
        
        A := 0;
        REPEAT
            A := A + 1;
            ANS := ANS + arr[A];
        UNTIL A = 10;
        writeln(ANS);
    END.